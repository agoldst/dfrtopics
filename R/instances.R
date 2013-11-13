# Functions for creating, reading, and exploring MALLET instances.

#' Create MALLET instances from a document frame
#' 
#' Given a frame like that returned by \code{\link{docs_frame}},
#' create a MALLET \code{InstanceList} object
#'
#' @param docs data frame with \code{id} and \code{text} columns
#' @param stoplist_file passed on to MALLET
#' @param java_heap if non-null, java is restarted with this heap parameter
#' @param ... passed on to \code{\link[mallet]{mallet.import}}
#' @return an rJava reference to a MALLET \code{InstanceList}
#' @seealso \code{\link{train_model}}
#' \code{\link{write_instances}}
#'
#' @export
#' 
make_instances <- function(docs,stoplist_file,java_heap=NULL,...) {
    .reload_mallet(java_heap)

    # token regex: letters only, by default
    # another possibility would be to include punctuation \p{P}
    mallet.import(docs$id,docs$text,
                  stoplist.file=stoplist.file,
                  ...)
}

#' Save a mallet InstanceList object to a file
#'
#' Saves mallet instances to disk using MALLET's file writer. The result is then 
#' equivalent to \code{mallet import-dirs} or similar at the command line.
#' @param instances reference to the \code{InstanceList}
#' @param output_file filename
#'
#' @seealso \code{\link{read_instances}},
#' \code{\link{make_instances}}
#'
#' @export
#' 
write_instances <- function(instances,output_file="instances.mallet") {
  instances$save(new(J("java.io.File"),output_file))
}

#' Read a mallet \code{InstanceList} object from a file
#'
#' Reads a mallet \code{InstanceList} object from a file.
#'
#' @param filename the instance file
#' @return a reference to the MALLET \code{InstanceList} object
#' @seealso \code{\link{write_instances}},
#' \code{\link{make_instances}}
#' \code{\link{train_model}}
#'
#' @export
#' 
read_instances <- function(filename) {
    J("cc.mallet.types.InstanceList","load",new(J("java.io.File"),
                                                path.expand(filename)))
}

#' Extract term-document matrix from instances
#'
#' Given an instance list, returns a term-document matrix.
#'
#' \emph{N.B.} Instances hold processed text (stopwords already removed, text typically
#' lowercased, etc.)
#' For the idea of going sparse, h/t Ben Marwick. I also experimented with parallelization 
#' using \code{doMC}, but not successfully.
#'
#' @return a \code{\link{Matrix:sparseMatrix}} with documents in columns and terms 
#' in rows. The ordering of the terms
#' is as in the vocabulary (\code{\link{instances_vocabulary}}), and the ordering of 
#' documents is as in the
#' instance list (\code{\link{instances_ids}}).
#'
#' @param instances file holding MALLET instances or rJava reference to a MALLET 
#' \code{InstanceList} object from e.g. \code{\link{read_instances}}
#' @param verbose if TRUE, give some progress messaging
#'
#' @seealso
#' \code{\link{Matrix:sparseMatrix}},
#' \code{\link{instances_vocabulary}},
#' \code{\link{instances_ids}},
#' \code{\link{read_dfr}} for access to the unprocessed wordcounts data (i.e. before 
#' stopword removal, etc.).
#'
#' @export
#'
instances_term_document_matrix <- function(instances,verbose=F) {
    if(verbose) {
        log <- message
    }
    else {
        log <- function(...) {NULL}
    }

    nwords <- instances$getAlphabet()$size()
    if (class(instances)=="character") {
        log("Loading instances from ",instances)
        instances <- read_instances(instances)
    }

    instances <- .jevalArray(instances$toArray(),simplify=T) 

    log("Retrieved instances from mallet.")
    log("Compiling tdm...")

    # ugh. Solution from:
    # http://stackoverflow.com/questions/8843700/creating-sparse-matrix-from-a-list-of-sparse-vectors
    log("Tabulating instances into sparseVector list")

    instance_tf <- function(inst) {
        counts <- tabulate(instance_vector(inst))
        sparseVector(counts,seq_along(counts),length=nwords)
    }

    vs <- lapply(instances,instance_tf) 
    n_x <- sapply(vs,function(v) length(v@i))

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
        stopifnot(is(vs[[k]],"sparseVector"))
    }

    log("Building sparseMatrix parameters")
    for (k in seq_along(vs)) {
        l <- n_x[k]
        if(l == 0) {
            next
        }

        elems_k <- a:(a + l - 1)

        cs[elems_k] <- k
        rs[elems_k] <- vs[[k]]@i
        xs[elems_k] <- vs[[k]]@x
        a <- a + l
    }

    log("Constructing sparseMatrix")

    result <- sparseMatrix(i=rs,j=cs,x=xs)

    # non-sparse version:
    #       instance_tf <- function(inst) {
    #       tabulate(instance_vector(inst),nbins=nwords)
    #    }
    #    result <- vapply(instances,instance_tf,integer(nwords))
    #}
    result
}

#' Extract document id's
#'
#' Returns a vector of id's ("names") from an \code{InstanceList},
#' in the order MALLET keeps them in
#'
#' @param instances a reference to an \code{InstanceList} object (from 
#' \code{\link{read_instances}} or \code{trainer$instances})
#'
#' @return a character vector of document ID's
#'
#' @export
#'
instances_ids <- function(instances) {
    iter <- instances$iterator()

    instance_name <- function() {
        inst <- .jcall(iter,"Ljava/lang/Object;","next")
        .jstrVal(.jcall(inst,"Ljava/lang/Object;","getName"))
    }

    replicate(instances$size(),instance_name())
}

#' retrieve an instance from the instance list by id
#'
#' Pulls out a single instance (document) from the instance list by id.
#'
#' @param instances a reference to an \code{InstanceList} object (from 
#' \code{\link{read_instances}} or \code{trainer$instances})
#'
#' @param id a document id
#'
#' @param id_map map from instance index to id's, to match against \code{id}. Calculated 
#' by default, but if you're going to do this a lot, precalculate the \code{id_map} just 
#' once.
#'
#' @return reference to a MALLET \code{Instance}.
#'
#' @export
#'
get_instance <- function(instances,id,id_map=instances_ids(instances)) {
    j <- match(id,id_map) - 1
    .jcall(instances,"Ljava/lang/Object;","get",as.integer(j))
}

#' Convert an instance to a vector
#'
#' Extracts an R vector representation of the \code{FeatureSequence}.
#'
#' A \code{FeatureSequence} is a list of \emph{zero-based} indices
#' into the vocabulary. For convenience, this function adds
#' 1 so that the result can be used to index directly into a
#' vocabulary vector (e.g. from \code{trainer$getVocabulary()} or
#' \code{\link{instances_vocabulary}}). Note that although MALLET's
#' topic-modeling works on feature \emph{sequences} because it is
#' designed to preserve the order of words in the documents it models,
#' the pre-aggregated data from JSTOR means that the "sequences" will be
#' meaningless: \code{\link{docs_frame}} simply puts all the occurrences
#' of a word in a document next to one another.
#'
#' @param instance a reference to an Instance
#' @return an integer vector, with \emph{one-based} indices into the vocabulary
#'
#' @seealso
#' \code{\link{instances_vocabulary}},
#' \code{\link{docs_frame}},
#' \code{\link{instance_text}}
#' 
#' @export
#'
instance_vector <- function(instance) {
    fs <- .jcall(instance,"Ljava/lang/Object;","getData")
    .jcall(fs,"[I","getFeatures") + 1
}

#' Transform an instance back into text
#'
#' "Reads" a MALLET instance as a text string for quick inspection.
#'
#' In the case of DfR data, the resulting string will have all
#' occurrences of each word type next to one another: not very
#' informative except for spot-checks for garbage data, correct
#' stop-wording, etc.
#'
#' Repeated calls will be much faster if you retrieve the vocabulary
#' separately and pass that in as \code{vocab}. An \code{InstanceList}
#' guarantees that all \code{Instances} have the same vocabulary.
#'
#' @param instance reference to an instance
#' @param vocab character vector giving the vocabulary
#' @param collapse passed on to \code{\link{base:paste}}
#' @return A string "spelling out" the instance text
#'
#' @export
#'
instance_text <- function(instance,
                          vocab=instances_vocabulary(instance),
                          collapse=" ") {
    paste(vocab[instance_vector(instance)],collapse=collapse)
}


#' Retrieve the vocabulary from the instances
#'
#' Retrieves the vocabulary from the instances.
#' 
#'  If you have the topic model
#' trainer object, the vocabulary is retrievable more quickly with an
#' \code{RTopicModel} method: \code{trainer$getVocabulary()}. But every 
#' \code{InstanceList} knows its vocabulary.
#'
#' @param instances reference to the \code{InstanceList}
#' @return character vector mapping one-based word indices to terms as strings
#'
#' @export
#'
instances_vocabulary <- function(instances) {
    sapply(.jevalArray(instances$getAlphabet()$toArray()),.jstrVal)
}

#' Aggregate word counts by years
#'
#' Aggregates word counts in a term-document matrix by document years.
#'
#' @param metadata the metadata frame
#' @param tdm term-document sparseMatrix (from result of \code{\link{instances_term_document_matrix}})
#' @param instances supply a reference to an \code{InstanceList} and the routine will 
#' retrieve the id list and vocabulary automatically. Ignored if \code{id_map} and 
#' \code{vocabulary} are specified.
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#'
#' @return A two-element list: \define{
#'      \item{\code{tym}}{the term-year-matrix (as \code{\link{Matrix:sparseMatrix}})} 
#' \item{\code{yseq}}{a map from column indices to dates. Should be
#' sequential but may not be evenly spaced if any year
#' is missing from the data.}
#' }
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#' \code{\link{instances_ids}}
#' \code{\link{instances_vocabulary}}
#' \code{\link{term_year_topic_matrix}}
#'
#' @export
#'
term_year_matrix <- function(metadata,
                             tdm,
                             instances=NULL,
                             id_map=instances_ids(instances),
                             vocabulary=instances_vocabulary(instances)) {
    metadata <- metadata[metadata$id %in% id_map,]
    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    # indicator-matrix version of years
    Y <- Matrix(0,nrow=length(years),ncol=nlevels(years))

    Y[cbind(seq_along(years),years)] <- 1

    result <- tdm %*% Y

    list(tym=result,yseq=levels(years))
}

#' Convert a term-year matrix to a dataframe for plotting
#'
#' This function makes a dataframe, suitable for plotting, of entries
#' from a term-year matrix (conditional on topic or not).
#'
#' @param words which terms to pick out
#'
#' @param term_year matrix with terms in rows and time-slices in columns
#'
#' @param year_seq character vector mapping columns of \code{term_year}
#' to dates, as strings in ISO format (the \code{yseq}
#' component of results from \code{\link{term_year_matrix}} or
#' \code{\link{term_year_topic_matrix}}
#' @param vocab character vector mapping rows of \code{term_year} to terms
#'
#' @param raw_counts if FALSE (the default), divide counts through by column totals or 
#' \code{denominator}
#' 
#' @param total if TRUE, tally up the total incidences of all words in \code{words}
#'
#' @param denominator \code{raw_counts} is FALSE, counts are normalized by this. If this 
#' is NULL, column totals are used. Use this parameter if you
#' are passing in a \code{\link{term_year_topic_matrix}} but you still want the
#' yearly proportion out of all words in the corpus, in which case
#' set \code{denominator=term_year_matrix()$tym}. The columns of
#' \code{denominator} are assumed to correspond to the same years as those of 
#' \code{term_year}, so be careful of dropped years.
#'
#' @return a dataframe with three columns, \code{word,year,weight}. If \code{total} is 
#' TRUE, this will have only one row.
#'
#' @seealso
#' \code{\link{term_year_topic_matrix}},
#' \code{\link{term_year_matrix}}
#'
#' @export
#'
term_year_series_frame <- function(words,term_year,year_seq,vocab,
                                   raw_counts=F,
                                   total=F,
                                   denominator=NULL) {
    w <- match(words,vocab)
    if(any(is.na(w))) {
        message("Dropping words missing from vocabulary: ",
                 paste(words[is.na(w)],collapse=" "))
        words <- words[!is.na(w)]
        w <- w[!is.na(w)]
     }

    # TODO use yearly_series_frame to factor this out
    wts <- term_year[w,,drop=F]
    if(!raw_counts) {
        if(is.null(denominator)) {
            denominator <- colSums(term_year)
        }
        wts <- wts %*% diag(1 / denominator) 
    }

    if (total) {
        wts <- matrix(colSums(wts),nrow=1)
        if(length(words) > 5) {
            words <- paste(words[1:5],collapse=" ")
            words <- paste('Total of "',words,'," etc.',sep="")
        } else {
            words <- paste(words,collapse=" ")
            words <- paste("Total of ",words,sep="")
        }
    }

    # TODO use yearly_series_frame to factor this out too
    rownames(wts) <- words
    colnames(wts) <- year_seq
    series <- melt(as.matrix(wts))
    names(series) <- c("word","year","weight")
    series$year <- as.Date(series$year)

    series
}

#' Derive a term-year counts conditioned on a journal
#'
#' Convenience wrapper for \code{\link{term_year_matrix}, summing counts 
#' only for documents in a given journal
#'
#' @param journal the journal, matched against \code{metadata$journaltitle} (so be careful 
#' about trailing tabs in JSTOR metadata fields)
#' @param metadata the metadata frame
#' @param the term-document matrix from \code{\link{instances_term_document_matrix}}
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#' @return a two-element \code{list(tym=...,yseq...)} as in \code{\link{term_year_matrix}}
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#' \code{\link{instances_ids}}
#' \code{\link{instances_vocabulary}}
#' \code{\link{term_year_matrix}}
#'
#' @export
#'
term_year_matrix_journal <- function(journal,
                                     metadata,
                                     tdm,
                                     id_map,
                                     vocabulary) {

    metadata <- metadata[metadata$id %in% id_map,]
    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    jm <- Diagonal(n=ncol(tdm),x=(journals==journal))

    term_year_matrix(metadata,
                     tdm=tdm %*% jm,
                     id_map=id_map,
                     vocabulary=vocabulary,
                     big=T)
}

#' Sum a term-document matrix over journals and years
#'
#' Calculates the total wordcounts per journal per year from a term-document matrix. Can 
#' be usefully applied not just to the result of 
#' \code{\link{instances_term_document_matrix}} but also to a 
#' \code{\link{term_document_topic_matrix}}.
#'
#' @param tdm matrix or \code{\link{Matrix:sparseMatrix}} with terms in rows and documents 
#' in columns
#' @param metadata metadata frame
#' @param id_map character vector mapping \code{tdm} columns to \code{metadata$id} values
#' @return an ordinary matrix with journals in rows and years in columns; the 
#' \code{\link{base:rownames}} of the result give the \code{journaltitle} values and the 
#' \code{\link{base:colnames}} give the dates as strings
#'
#' @seealso
#'
#' \code{\link{instances_term_document_matrix}},
#' \code{\link{term_document_topic_matrix}},
#' \code{\link{term_year_matrix}},
#' \code{\link{term_year_topic_matrix}}
#'
#' @export
#'
journal_year_matrix <- function(tdm,metadata,id_map) {
    metadata <- metadata[metadata$id %in% id_map,]

    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    Y <- Matrix(0,nrow=length(years),ncol=nlevels(years)) 
    Y[cbind(seq_along(years),years)] <- 1

    result <- matrix(nrow=nlevels(journals),ncol=nlevels(years))

    # The fully algebraic way would be to construct some block matrices
    # to do these sums, but since the number of journals is small, it's
    # not worth it

    Csum <- Matrix(rep(1,nrow(tdm)),nrow=1)

    for(i in seq_along(levels(journals))) {
        jrnl <- levels(journals)[i]

        jm <- Diagonal(n=ncol(tdm),x=(journals==jrnl))
        m <- tdm %*% jm
        tym <- m %*% Y

        result[i,] <- drop(Csum %*% tym)
    }

    rownames(result) <- levels(journals)
    colnames(result) <- levels(years)
    result
}

#' Calculate tf*idf scores
#'
#' Calculates tf*idf scores from a term-document \code{\link{Matrix:sparseMatrix}}.
#'
#' May not be optimal for speed for calculating scores for the whole tdm.
#'
#' @param term numeric index into rows of \code{tdm} (can be a vector)
#' @param doc numeric index into columns of \code{tdm} (can be a vector)
#' @param tdm term-document \code{\link{Matrix:sparseMatrix}}
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#'
#' @export
#'
tf_idf <- function(term,doc,tdm) {
    idf <- log(ncol(tdm) / rowSums(tdm[term,,drop=F] != 0))
    Diagonal(n=length(term),x=idf) %*% tdm[term,doc]
}
