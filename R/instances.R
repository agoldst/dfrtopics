# Functions for creating, reading, and exploring MALLET instances.

#' Create MALLET instances from a document frame
#'
#' Given a data frame of document IDs and texts (one per doc), such as
#' that returned by \code{\link{wordcounts_texts}}, create a MALLET
#' \code{InstanceList} object. This function is a simple wrapper for
#' \code{\link[mallet]{mallet.import}}. N.B. MALLET does tokenization,
#' stopword removal, and casefolding on these texts, but if you have
#' used \code{\link{wordcounts_texts}}, you may have already done
#' those tasks yourself. To ensure MALLET does no further stoplisting,
#' pass \code{stoplist_file=NULL} (the default). To ensure MALLET does
#' no extra tokenization, pass \code{token.regex="\\S+"} (whitespace
#' tokenization---\emph{not} the default). To prevent MALLET from
#' casefolding, pass \code{preserve.case=T}. Or, equivalently, use the
#' function \code{\link{wordcounts_instances}} instead.  
#'
#' The \code{InstanceList} object is the form in which MALLET
#' understands a corpus. These are the objects passed on to the
#' model-training routines. If saved to disk the same corpus may be used
#' with command-line MALLET.  
#'
#' If java gives out-of-memory errors, try increasing the Java heap size to a
#' large value, like 4GB, by setting \code{options(java.parameters="-Xmx4g")}
#' \emph{before} loading this package (or rJava).
#'
#' @param docs data frame with \code{id} and \code{text} columns
#' @param stoplist_file name of a text file with one stopword per line, passed
#'   on to MALLET, if it exists. If it does not, or if this is \code{NULL} (the
#'   default), no words are removed.
#' @param ... passed on to \code{\link[mallet]{mallet.import}}. A possibly
#'   important parameter to adjust is \code{token.regex}.
#' @return an rJava reference to a MALLET \code{InstanceList}
#' @seealso \code{\link{train_model}}, \code{\link{write_instances}}
#'
#' @export
#'
make_instances <- function (docs, stoplist_file=NULL, ...) {
    load_mallet()

    # TODO support putting labels ("targets") on instances as well. Have to
    # bypass mallet::mallet.import and RTopicModel.addInstances for that
    # then we can support Labeled LDA:
    # http://www.mimno.org/articles/labelsandpatterns/

    no_stop <- is.null(stoplist_file)
    if (!no_stop && !file.exists(stoplist_file)) {
        warning("Stoplist file not found. Using an empty stoplist.")
        no_stop <- TRUE
    }

    if (no_stop) {
        stoplist_file <- tempfile()
        writeLines("", stoplist_file)
    }
    insts <- mallet::mallet.import(docs$id, docs$text,
        stoplist=stoplist_file, ...)
    if (no_stop) {
        unlink(stoplist_file)
    }

    insts
}

#' Create MALLET instances from a word-counts data frame
#'
#' Given a data frame representing documents as feature counts, create a MALLET 
#' \code{InstanceList} object which can then be passed on to 
#' \code{\link{train_model}} or saved to disk for later use with 
#' \code{\link{write_instances}}. This function is a small convenience wrapper 
#' for \code{\link{make_instances}} that ensures no further stopword removal, 
#' tokenization, or casefolding is done.
#'
#' If your tokens themselves contain whitespace, change the \code{sep} parameter
#' and adjust the \code{token_regex} accordingly.
#'
#' @param counts data frame with \code{id}, \code{word}, \code{weight} columns
#' @param shuffle randomize word order before passing on to MALLET? (See 
#'   \code{\link{wordcounts_texts}}
#' @param sep separator to use between words
#' @param token_regex regular expression matching a token. Ordinarily, this
#'   should correspond to \code{sep} (hence the default, whitespace 
#'   tokenization), since no further tokenization should be done.
#' @param preserve_case if FALSE, all words are lowercased by MALLET
#' @return an rJava reference to a MALLET \code{InstanceList}
#'
#' @seealso \code{\link{make_instances}} which this wraps, 
#'   \code{\link{train_model}}, \code{\link{write_instances}}
#' @export 
wordcounts_instances <- function (counts, shuffle=FALSE,
                                  sep=" ", token_regex="\\S+",
                                  preserve_case=TRUE) {
    txts <- wordcounts_texts(counts, shuffle=shuffle, sep=sep)
    make_instances(txts, stoplist_file=NULL,
                   token.regexp=token_regex, preserve.case=preserve_case)
}

#' Save a mallet InstanceList object to a file
#'
#' Saves mallet instances to disk using MALLET's file writer. The result is then
#' equivalent to \code{mallet import-dirs} or similar at the command line.
#'
#' @param instances reference to the \code{InstanceList}
#' @param filename name of file to write to
#'
#' @seealso \code{\link{read_instances}}, \code{\link{make_instances}}
#'
#' @export
#'
write_instances <- function (instances, filename) {
    f <- rJava::.jnew("java/io/File", path.expand(filename))
    rJava::.jcall(instances, "V", "save", f)
}

#' Read a mallet \code{InstanceList} object from a file
#'
#' Reads a mallet \code{InstanceList} object from a file.
#'
#' @param filename the instance file
#' @return a reference to the MALLET \code{InstanceList} object
#' @seealso \code{\link{write_instances}}, \code{\link{make_instances}}
#'   \code{\link{train_model}}
#'
#' @export
#'
read_instances <- function (filename) {
    load_mallet()

    f <- rJava::.jnew("java/io/File", path.expand(filename))
    rJava::.jcall("cc/mallet/types/InstanceList",
                  "Lcc/mallet/types/InstanceList;", "load", f)
}

#' Extract term-document matrix from instances
#'
#' Given an instance list, returns a term-document matrix (sparse format).
#'
#' If the matrix is \code{m}, then \code{m[i, j]} gives the weight of word
#' \code{i} in document \code{j}. If another term-weighting is desired, this
#' matrix is convenient to operate on.
#'
#' For the idea of going sparse, h/t Ben Marwick. The conversion is fairly slow
#' because it involves copying all the corpus data from Java to R and then goes
#' on to commit the Ultimate Sin and use a \code{for} loop. Pass
#' \code{verbose=T} for some reports on progress. TODO: make smarter.
#'
#' @return a \code{\link[Matrix]{sparseMatrix}} with documents in columns and
#'   words in rows. The ordering of the words is as in the vocabulary
#'   (\code{\link{instances_vocabulary}}), and the ordering of documents is as
#'   in the instance list (\code{\link{instances_ids}}).
#'
#' @param instances file holding MALLET instances or rJava reference to a MALLET
#'   \code{InstanceList} object from e.g. \code{\link{read_instances}}
#' @param verbose if TRUE, give some progress messaging
#'
#' @seealso \code{\link[Matrix]{sparseMatrix}},
#' \code{\link{instances_vocabulary}}, \code{\link{instances_ids}},
#' \code{\link{read_wordcounts}} for access to unprocessed wordcounts data (i.e.
#' before stopword removal, etc.).
#'
#' @export
#'
instances_Matrix <- function (instances,
                              verbose=getOption("dfrtopics.verbose")) {

    if (verbose)
        blurt <- message
    else
        blurt <- function (...) { }

    blurt("Retrieving instances")
    if (class(instances) == "character") {
        blurt("Loading from ", instances)
        instances <- read_instances(instances)
    }
    nwords <- rJava::.jcall(
        rJava::.jcall(instances, "Lcc/mallet/types/Alphabet;", "getAlphabet"),
        "I", "size"
    )

    instances <- rJava::.jcall(instances,
        "[Ljava/lang/Object;", "toArray",
        evalArray=TRUE, simplify=TRUE
    )

    blurt("Compiling tdm")

    # ugh. Solution from:
    # http://stackoverflow.com/questions/8843700/creating-sparse-matrix-from-a-list-of-sparse-vectors
    blurt("Tabulating instances into sparseVector list")

    instance_tf <- function (inst) {
        counts <- tabulate(instance_vector(inst))
        Matrix::sparseVector(counts, seq_along(counts), length=nwords)
    }

    vs <- lapply(instances, instance_tf)
    n_x <- vapply(vs, function(v) length(v@i), numeric(1))

    # If we wanted to be totally vectorial, we could compute a
    # running total of n_x, which tells us how to index into our
    # row and column vectors for each document:
    #
    #    acc <- matrix(0L,nrow=length(n_x),ncol=length(n_x))
    #    acc[upper.tri[rsum,diag=T]] <- 1L
    #    indices <- n_x %*% acc
    #
    # and then calculate the a:a+l-1 sequences in advance,
    # but whatevs!

    N <- sum(n_x)
    rs <- integer(N)
    cs <- integer(N)
    xs <- integer(N)

    a <- 1
    for(k in seq_along(vs)) {
        stopifnot(is(vs[[k]], "sparseVector"))
    }

    blurt("Building sparseMatrix parameters")
    for (k in seq_along(vs)) {
        l <- n_x[k]
        if (l == 0) {
            next
        }

        elems_k <- a:(a + l - 1)

        cs[elems_k] <- k
        rs[elems_k] <- vs[[k]]@i
        xs[elems_k] <- vs[[k]]@x
        a <- a + l
    }

    blurt("Constructing sparseMatrix")

    result <- Matrix::sparseMatrix(i=rs, j=cs, x=xs)

    # non-sparse version:
    #       instance_tf <- function(inst) {
    #       tabulate(instance_vector(inst),nbins=nwords)
    #    }
    #    result <- vapply(instances,instance_tf,integer(nwords))
    #}
    result
}

#' Extract document id's from an InstanceList
#'
#' Returns a vector of id's ("names") from an \code{InstanceList}, in the order
#' MALLET keeps them in.
#'
#' @param instances a reference to an \code{InstanceList} object (from
#'   \code{\link{read_instances}} or \code{trainer$instances})
#'
#' @return a character vector of document ID's
#'
#' @export
#'
instances_ids <- function (instances) {
    iter <- rJava::.jcall(instances, "Ljava/util/Iterator;", "iterator")

    instance_name <- function() {
        inst <- rJava::.jcall(iter, "Ljava/lang/Object;", "next")
        rJava::.jstrVal(rJava::.jcall(inst, "Ljava/lang/Object;", "getName"))
    }

    replicate(
        rJava::.jcall(instances, "I", "size"),
        instance_name()
    )
}

#' Retrieve an instance from the instance list by id
#'
#' A convenience function that pulls out a single (document) from an
#' InstanceList using its id.  Again mostly useful for inspecting the results of
#' stopwording, etc.
#'
#' To subscript an InstanceList by position, use its own accessor:
#' \code{instances$get(3L)}. The index is zero-based and must be an integer (not
#' numeric).
#'
#' @param instances a reference to an \code{InstanceList} object (from
#'   \code{\link{read_instances}} or \code{trainer$instances})
#'
#' @param id a document id
#'
#' @param id_map map from instance index to id's, to match against \code{id}.
#'   Calculated by default, but if you're going to do this a lot, precalculate
#'   the \code{id_map} just once.
#'
#' @return reference to a MALLET \code{Instance}.
#'
#' @export
#'
get_instance <- function (instances, id, id_map=instances_ids(instances)) {
    j <- match(id, id_map) - 1
    rJava::.jcall(instances, "Ljava/lang/Object;", "get", as.integer(j))
}

#' Convert a MALLET Instance to an integer vector
#'
#' Given a single MALLET Instance (not an InstanceList), this function retrieves
#' an R vector representation of the \code{FeatureSequence}.
#'
#' A \code{FeatureSequence} is a list of \emph{zero-based} indices into the
#' vocabulary. For convenience, this function adds 1 so that the result can be
#' used to index directly into a vocabulary vector (e.g. from
#' \code{trainer$getVocabulary()} or \code{\link{instances_vocabulary}}). Note
#' that although MALLET's topic-modeling works on feature \emph{sequences}
#' because it is designed to preserve the order of words in the documents it
#' models, if you have used pre-aggregated data from JSTOR the "sequences" will
#' be meaningless.
#'
#' @param instance a reference to a single Instance
#' @return an integer vector, with \emph{one-based} indices into the vocabulary
#'
#' @seealso \code{\link{instances_vocabulary}}, \code{\link{wordcounts_texts}},
#' \code{\link{instance_text}}
#'
#' @export
#'
instance_vector <- function (instance) {
    fs <- rJava::.jcall(instance, "Ljava/lang/Object;", "getData")
    rJava::.jcall(fs, "[I", "getFeatures") + 1
}

#' Transform an instance back into text
#'
#' "Reads" a MALLET instance as a text string for quick inspection.
#'
#' In the case of DfR data, the resulting string will not have meaningful word
#' order: not very informative except for spot-checks for garbage data, correct
#' stop-wording, etc.
#'
#' Repeated calls will be much faster if you retrieve the vocabulary separately
#' and pass that in as \code{vocab}. An \code{InstanceList} guarantees that all
#' \code{Instances} have the same vocabulary.
#'
#' @param instance reference to an instance
#' @param vocab character vector giving the vocabulary
#' @param collapse passed on to \code{\link[base]{paste}}
#' @return A string "spelling out" the instance text
#'
#' @export
#'
instance_text <- function (instance,
                           vocab=instances_vocabulary(instance),
                           collapse=" ") {
    stringr::str_c(vocab[instance_vector(instance)], collapse=collapse)
}


#' Retrieve the vocabulary from the instances
#'
#' Retrieves a list of feature types (the "vocabulary") from an InstanceList
#' reference.
#'
#' If you have the topic model trainer object, the vocabulary is retrievable
#' more quickly with an \code{RTopicModel} method:
#' \code{trainer$getVocabulary()}. But every \code{InstanceList} knows its
#' vocabulary.
#'
#'
#' @param instances reference to the \code{InstanceList}
#' @param newlines_significant if vocabulary words include newlines, a slower
#'   method of extracting the vocabulary will be used (unusual for DfR)
#' @return character vector mapping one-based word indices to words as strings
#'
#' @export
#'
instances_vocabulary <- function (instances, newlines_significant=FALSE) {

    alph <- rJava::.jcall(instances,
        "Lcc/mallet/types/Alphabet;", "getAlphabet")

    if (newlines_significant) {
        # .jevalArray is slow on even a moderate vocabulary.
        alph_arr <- rJava::.jcall(alph, "[Ljava/lang/Object;", "toArray")
        
        vocab <- vapply(alph_arr, rJava::.jstrVal, character(1))
    } else {
        # This silly-looking method is faster, though it assumes
        # that none of the vocabulary items contain '\n'
        alph_str <- rJava::.jcall(alph, "S", "toString")
        vocab <- unlist(
            stringr::str_split(stringr::str_trim(alph_str), "\n")
        )
    }

    vocab
}

#' Retrieve instance lengths
#'
#' Given an InstanceList, this gives a vector of total (stopped) word lengths.
#'
#' @param instances reference to an InstanceList object
#' @return an integer vector
#'
#' @export
#'
instances_lengths <- function (instances) {
    iter <- rJava::.jcall(instances, "Ljava/util/Iterator;", "iterator")

    instance_size <- function() {
        inst <- rJava::.jcall(iter, "Ljava/lang/Object;", "next")
        rJava::.jcall(rJava::.jcall(inst, "Ljava/lang/Object;", "getData"),
            "I",
            "size"
        )
    }
    replicate(rJava::.jcall(instances, "I", "size"),
        instance_size()
    )
}

#' Generate a new InstanceList compatible with an old one
#'
#' Given an InstanceList object and some new documents (in the same two-column
#' data-frame format as that expected by \code{\link{make_instances}}), this
#' yields a reference to a new InstanceList which is "compatible" with the old
#' one. These new instances can be used, for example, with MALLET's
#' topic-inferencing functionality.
#'
#' "Compatible" instances have the same vocabulary in the same order as the
#' reference InstanceList. In particular, this means words not in the reference
#' vocabulary must be dropped.
#'
#' The new InstanceList object can be written to disk with
#' \code{\link{write_instances}}.
#'
#' @param docs data frame with \code{id} and \code{text} columns
#' @param instances reference InstanceList object
#' @return a reference to the new InstanceList object
#'
#' @seealso \code{\link{infer_topics}}
#' @export
#'
compatible_instances <- function (docs, instances) {

    mallet_pipe <- rJava::.jcall(instances,
        "Lcc/mallet/pipe/Pipe;", "getPipe")

    new_insts <- rJava::.jnew("cc/mallet/types/InstanceList",
                       rJava::.jcast(mallet_pipe, "cc/mallet/pipe/Pipe"))

    rJava::.jcall("cc/mallet/topics/RTopicModel", "V", "addInstances",
        new_insts, docs$id, docs$text)

    new_insts
}
