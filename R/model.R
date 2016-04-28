# Core dfrtopics functions: running MALLET and extracting document-topic
# and topic-word information.

#' Make a topic model of DfR documents
#'
#' The basic usage of this package is wrapped up in this convenience function.
#'
#' Given wordcount and metadata files, this function sets up MALLET inputs and
#' then runs MALLET to produce a topic model. Normally you will want
#' finer-grained control over the mallet inputs and modeling parameters. The
#' steps for that process are described in the package vignette. Once the model
#' has been trained, the results can be saved to disk with
#' \code{\link{write_mallet_model}}
#'
#' If java gives out-of-memory errors, try increasing the Java heap size to a
#' large value, like 4GB, by setting \code{options(java.parameters="-Xmx4g")}
#' \emph{before} loading this package (or rJava).
#'
#'
#' @param citations_files character vector with names of DfR
#'   \code{citations.CSV} or \code{citations.tsv} metadata files files
#' @param wordcounts_dirs character vector with names of directories holding
#'   \code{wordcounts*.CSV} files
#' @param stoplist_file name of stoplist file (containing one stopword per line)
#' @param n_topics number of topics to model
#' @param ... passed on to \code{\link{train_model}}
#'
#' @return a \code{\link{mallet_model}} object holding the results
#'
#' @seealso This function simply calls in sequence
#'   \code{\link{read_dfr_metadata}}, \code{\link{read_wordcounts}},
#'   \code{\link{wordcounts_texts}}, \code{\link{make_instances}}, and
#'   \code{\link{train_model}}. To write results to disk, use
#'   \code{\link{write_mallet_model}}
#'
#' @examples
#' # Make a 50-topic model of documents in the wordcounts folder
#' \dontrun{model_dfr_documents("citations.CSV", "wordcounts", 50)}
#'
#' @export
#'
model_dfr_documents <- function(
        citations_files,
        wordcounts_dirs,
        n_topics,
        stoplist_file=file.path(path.package("dfrtopics"),
                                "stoplist", "stoplist.txt"),
        ...)  {
    result <- read_wordcounts(list.files(wordcounts_dirs, full.names=TRUE))
    result <- wordcounts_texts(result)
    result <- make_instances(result, stoplist_file)
    train_model(result, n_topics,
                metadata=read_dfr_metadata(citations_files),
                ...)
}

#' A convenience function for saving all the model outputs at once.
#'
#' Save a series of files with the results of an LDA run. By default this will produce a number of files, including several large ones.
#'
#' The following files are written to \code{output_dir}: \describe{
#'
#' \item{\code{topic_words.csv}}{unnormalized topic-word matrix, CSV format}
#'
#' \item{\code{vocabulary.txt}}{list of words (same order as columns of topic-word
#' matrix), one per line}
#'
#' \item{\code{params.txt}}{Various model parameters, including hyperparameters}
#'
#' \item{\code{top_words.csv}}{topic key words CSV; see \code{\link{top_words}}
#' for the format}
#'
#' \item{\code{doc_topics.csv}}{document-topic matrix CSV}
#'
#' \item{\code{mallet_state.gz}}{MALLET sampling state (a big file)}
#'
#' \item{\code{state.csv}}{simplified version of the sampling state}
#'
#' \item{\code{diagnostics.xml}}{MALLET model diagnostics}
#'
#' \item{\code{doc_ids.txt}}{instance id's, one per line}
#'
#' \item{\code{instances.mallet}}{save the source text "instances" file (not
#' done by default)}
#'
#' \item{\code{topic_scaled.csv}}{CSV with scaled 2D coordinates for the topics.
#' Obtained by applying \code{\link[stats]{cmdscale}} to a matrix of topic
#' divergences calculated by \code{\link{topic_divergences}}}
#'
#' }
#'
#' @param m \code{mallet_model} object
#'
#' @param output_dir where to save all the output files.
#'
#' @param save_instances if TRUE, extract the instance list from the trainer
#'   object and save it to \code{instances.mallet}
#'
#' @param save_scaled if TRUE write a file of 2D coordinates for
#'   the topics
#'
#' @param save_state if TRUE, save the MALLET sampling state in MALLET's format
#'
#' @param simplify_state if TRUE, save the sampling state in a simplified CSV
#' format (requires python)
#'
#' @export
#'
write_mallet_model <- function(m, output_dir=".",
                          n_top_words=50,
                          save_instances=FALSE,
                          save_scaled=FALSE,
                          save_state=TRUE,
                          simplify_state=TRUE) {
    if(!file.exists(output_dir)) {
        message("Creating output directory ",output_dir)
        dir.create(output_dir)
    }

    tw_f <- file.path(output_dir, "topic_words.csv")
    write_matrix_csv(topic_words(m), tw_f)
    message("Wrote ",tw_f)

    vocab_f <- file.path(output_dir, "vocabulary.txt")
    writeLines(vocabulary(m), vocab_f)
    message("Wrote ",vocab_f)

    params <- modeling_parameters(m)
    hyper <- hyperparameters(m)

    params_f <- file.path(output_dir, "params.txt")
    dput(list(params=params, hyper=hyper), params_f)
    message("Wrote ", params_f)

    keys_f <- file.path(output_dir, "top_words.csv")
    write.table(top_words(m, n_top_words), keys_f,
                quote=FALSE, sep=",", row.names=FALSE, col.names=TRUE)
    message("Wrote ", keys_f)

    dt_f <- file.path(output_dir,"doc_topics.csv")
    write.table(doc_topics(m), dt_f,
                quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
    message("Wrote ", dt_f)

    if (save_state) {
        state_f <- file.path(output_dir, "mallet_state.gz")
        write_mallet_state(m, state_f)
        message("Wrote ", state_f)
    }

    if (simplify_state) {
        if (!save_state) {
            state_f <- tempfile()
            write_mallet_state(m, state_f)
        }
        ss_f <- file.path(output_dir, "state.csv")
        simplify_state(state_f, ss_f)
        message("Wrote ", ss_f)

        if (!save_state) {
            unlink(state_f)
        }
    }

    diag_f <- file.path(output_dir, "diagnostics.xml")
    write_diagnostics(m, diag_f, n_top_words=n_top_words)
    message("Wrote ", diag_f)

    id_map_f <- file.path(output_dir, "doc_ids.txt")
    writeLines(doc_ids(m), id_map_f)
    message("Wrote ", id_map_f)

    if (save_instances) {
        inst_f <- file.path(output_dir, "instances.mallet")
        write_instances(instances(m), inst_f)
        message("Wrote ", inst_f)
    }

    if (save_scaled) {
        scaled_f <- file.path(output_dir,"topic_scaled.csv")
        write.table(topic_scaled_2d(m), scaled_f,
                    quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
        message("Wrote ", scaled_f)
    }
}

#' Train a topic model
#'
#' Invokes MALLET's parallel topic modeling algorithm on a set of documents
#' represented as an InstanceList.
#'
#' Create the instance list object with \code{\link{make_instances}}. This
#' function prints MALLET's progress reporting to the console.
#'
#' If Java gives out-of-memory errors, try increasing the Java heap size to a
#' large value, like 4GB, by setting \code{options(java.parameters="-Xmx4g")}
#' \emph{before} loading this package (or rJava).
#'
#' @param instances either an rJava reference to an \code{InstanceList} object
#'   or the name of a file into which such an object has been serialized
#' @param n_topics how many topics to train?
#' @param alpha_sum initial sum of hyperparameters \eqn{alpha_k}: priors of
#'   topics over document
#' @param beta initial value of hyperparameter \eqn{\beta}: prior of topics over
#'   words
#' @param n_iters number of Gibbs sampling iterations to run
#' @param n_max_iters number of "iterated conditional modes"
#' @param optimize_hyperparameters if TRUE (the default), optimize
#'   \eqn{\alpha_k} and \eqn{\beta}. If FALSE, the value of
#'   \code{symmetric_alpha} is ignored.
#' @param n_hyper_iters how often to do hyperparameter optimization
#' @param n_burn_in number of initial "burn-in" iterations before hyperparameter
#'   optimization
#' @param symmetric_alpha if FALSE (the default), allow the \eqn{\alpha_k} to be
#'   different from one another. If TRUE when \code{optimize_hyperparameters} is
#'   TRUE, then the sum of the alphas will still be varied by the algorithm, but
#'   all the \eqn{\alpha_k} will be the same.
#' @param threads number of threads to run in parallel.
#' @param seed MALLET's random number seed: set this to ensure a reproducible
#'   run of the Gibbs sampling algorithm.
#' @param metadata not used in the modeling process, but the model object
#'   returned by the function will store a reference to it if supplied
#'
#' @return a \code{mallet_model} object
#'
#' @seealso \code{\link{make_instances}}, \code{\link{make_instances}},
#'   \code{\link{model_dfr_documents}}, \code{\link{write_mallet_model}}
#'
#' @export
#'
train_model <- function(instances, n_topics,
                        alpha_sum=5, beta=0.01,
                        n_iters=200,
                        n_max_iters=10,
                        optimize_hyperparameters=TRUE,
                        n_hyper_iters=20,
                        n_burn_in=50,
                        symmetric_alpha=FALSE,
                        threads=4L,
                        seed=NULL,
                        metadata=NULL) {

    # Java number types are a problem with R's loose typing. RTopicModel
    # expects doubles everywhere; ParallelTopicModel expects ints
    # for integers. So we have to do some coercions to avoid rJava
    # complaints.

    load_mallet()

    trainer <- mallet::MalletLDA(
        as.numeric(n_topics),
        as.numeric(alpha_sum),
        as.numeric(beta))

   if (rJava::.jinstanceof(trainer, "cc.mallet.topics.ParallelTopicModel")) {
        # new mallet: RTopicModel IS a ParallelTopicModel
        ptm <- trainer
    } else {
        # old mallet: RTopicModel HAS a ParallelTopicModel
        ptm <- rJava::.jfield(trainer,
            "Lcc/mallet/topics/ParallelTopicModel;", "model")
    }

    rJava::.jcall(ptm, "V", "setNumThreads", as.integer(threads))
    if (!is.null(seed)) {
        rJava::.jcall(ptm, "V", "setRandomSeed", as.integer(seed))
        message("MALLET random number seed set to ", seed)
    }

    rJava::.jcall(trainer, "V", "loadDocuments", instances)

    if (optimize_hyperparameters) {
        rJava::.jcall(ptm, "V", "setSymmetricAlpha", symmetric_alpha)
        rJava::.jcall(trainer, "V", "setAlphaOptimization",
            as.numeric(n_hyper_iters), as.numeric(n_burn_in))
    }
    else {
        rJava::.jcall(trainer, "V", "setAlphaOptimization", 0, 0)
    }

    rJava::.jcall(trainer, "V", "train", as.numeric(n_iters))
    rJava::.jcall(trainer, "V", "maximize", as.numeric(n_max_iters))

    result <- mallet_model(
        model=trainer,
        params=list(
            n_iters=n_iters,
            n_max_iters=n_max_iters,
            optimize_hyperparameters=optimize_hyperparameters,
            n_hyper_iters=n_hyper_iters,
            n_burn_in=n_burn_in,
            symmetric_alpha=symmetric_alpha,
            threads=threads,
            seed=seed,
            initial_alpha_sum=alpha_sum,
            initial_beta=beta,
            final_ll=rJava::.jcall(ptm, "D", "modelLogLikelihood")
        ),
        doc_topics=mallet::mallet.doc.topics(trainer,
            smoothed=FALSE, normalized=FALSE),
        metadata=match_metadata(metadata,
            rJava::.jcall(trainer, "[S", "getDocumentNames")
        )
    )

    # assign metadata; issue warning if it doesn't match
    if (!is.null(metadata) && is.null(result$metadata)) {
        warning(
"Supplied metadata does not match instance document ID's.
Model metadata will be NULL.
To set metadata later, use metadata(m) <- ..."
        )
    }

    result
}

#' Access the number of topics in the model
#'
#' Returns the number of topics in a model.
#'
#' @param m \code{\link{mallet_model}} object
#'
#' @return The number of topics.
#'
#' @export
n_topics <- function (m) UseMethod("n_topics")

#' @export
n_topics.mallet_model <- function (m) {
    if (!is.null(ParallelTopicModel(m))) {
        rJava::.jfield(ParallelTopicModel(m), "I", "numTopics")
    } else if (!is.null(m$doc_topics)) {
        ncol(m$doc_topics)
    } else if (!is.null(m$topic_words)) {
        nrow(m$topic_words)
    } else {
        NULL # return null if we haven't loaded enough information yet
    }
}

#' Access the number of documents modeled
#'
#' Returns the number of documents modeled.
#'
#' @param m \code{\link{mallet_model}} object
#'
#' @return The number of documents.
#'
#' @export
n_docs <- function (m) UseMethod("n_docs")

#' @export
n_docs.mallet_model <- function (m) {
    if (!is.null(m$doc_topics)) {
        nrow(m$doc_topics)
    } else if (!is.null(m$doc_ids)) {
        length(m$doc_ids)
    } else if (!is.null(RTopicModel(m))) {
        rJava::.jcall(
            rJava::.jfield(RTopicModel(m),
                "Lcc/mallet/types/InstanceList;",
                "instances"),
            "I",
            "size"
        )
    } else {
        NULL # return null if we haven't loaded enough information yet
    }
}

#' Access stored modeling parameters
#'
#' Returns a list of modeling parameters (number of iterations, initial
#' hyperparameter values, etc.) used to create the model.
#'
#' @export
modeling_parameters  <- function (m) UseMethod("modeling_parameters")

#' @export
modeling_parameters.mallet_model  <- function (m) m$params


#' Access MALLET's glue model object
#'
#' This function returns a reference to the top-level Java object representing
#' an LDA model.
#'
#' For its R interface, MALLET uses a class RTopicModel. This has some
#' convenience methods for accessing and manipulating a topic model from
#' R using rJava. It is also used by the functions in the \pkg{mallet}
#' package.
#'
#' In earlier versions of MALLET, this object had a data member of class
#' ParallelTopicModel instance. In the latest MALLET, RTopicModel inherits from
#' ParallelTopicModel.
#'
#' @param m a \code{mallet_model} object
#' @return a reference to the RTopicModel object (or NULL if unavailable)
#' @export
#'
RTopicModel <- function (m) UseMethod("RTopicModel")

#' @export
RTopicModel.mallet_model <- function (m) m$model

#' Access MALLET's model object
#'
#' This function returns a reference to the main Java object representing an LDA
#' model in MALLET.
#'
#' @param m a \code{mallet_model} object
#' @return a reference to a ParallelTopicModel object (or NULL if unavailable)
#' @seealso \code{\link{RTopicModel}}
#' @export
#'
ParallelTopicModel <- function (m) UseMethod("ParallelTopicModel")

#' @export
ParallelTopicModel.mallet_model <- function (m) {
    if (is.null(m$model)) {
        NULL
    } else if (rJava::.jinstanceof(
            m$model, "cc.mallet.topics.ParallelTopicModel")) {
        # new mallet: RTopicModel IS a ParallelTopicModel
        rJava::.jcast(m$model, "cc/mallet/topics/ParallelTopicModel")
    } else {
        # old mallet: RTopicModel HAS a ParallelTopicModel
        rJava::.jfield(m$model,
            "Lcc/mallet/topics/ParallelTopicModel;", "model")
    }
}

#' Access the InstanceList stored by a model
#'
#' MALLET models store a reference to their source documents. This is an
#' accessor function for that reference.
#'
#' @param m a \code{mallet_model} object
#' @return a reference to the InstanceList object, or NULL if not available
#'
#' @export
#'
instances <- function (m) UseMethod("instances")

#' @export
`instances<-` <- function (m, value) UseMethod("instances<-")

#' @export
instances.mallet_model <- function (m) {
    if (!is.null(m$instances)) {
        m$instances
    } else if (!is.null(RTopicModel(m))) {
        rJava::.jfield(RTopicModel(m), "Lcc/mallet/types/InstanceList;",
                       "instances")
    } else {
        NULL
    }
}

#' @export
`instances<-.mallet_model` <- function (m, value) {
    m$instances <- value
    m
}

#' The document-topic matrix
#'
#' Extracts the matrix from a \code{mallet_model} model with documents in rows and
#' topic weights in columns. The document order is the same as the ordering of
#' the documents supplied to MALLET.
#'
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and
#' unsmoothed weights as the "raw" form of the model; this is not strictly
#' correct, it is easier to reason about and to do post-hoc calculations with.
#' It is up to the user to apply normalization and smoothing where appropriate.
#'
#' @param m a \code{mallet_model} object
#' @return a numeric matrix
#'
#' @export
#'
doc_topics <- function (m) UseMethod("doc_topics")

#' @export
`doc_topics<-` <- function (m, value) UseMethod("doc_topics<-")

#' @export
doc_topics.mallet_model <- function (m) {
    dtm <- m$doc_topics
    if (is.null(dtm) && !is.null(RTopicModel(m))) {
        dtm <- mallet::mallet.doc.topics(RTopicModel(m),
            smoothed=FALSE, normalized=FALSE)
    }
    dtm
}

#' @export
`doc_topics<-.mallet_model` <- function (m, value) {
    m$doc_topics <- value
    m
}

#' Load model elements into a model object
#'
#' To ensure doc-topic or topic-word matrices are not recalculated every time
#' you access them, you can use these functions. R's functional design makes
#' memoizing a little cumbersome (or requires another package).
#'
#' @param m \code{mallet_model} object
#' @return a copy of \code{m} with the data element in question loaded. In
#'   principle the (list) copy is "shallow" and should not be expensive in time
#'   or memory.
#'
#' @examples
#' \dontrun{
#' m <- load_doc_topics(m)  # precalculate doc-topics matrix
#' doc_topics(m)            # will return stored values
#' }
#'
#' @export
load_doc_topics <- function (m) {
    m$doc_topics <- doc_topics(m)
    m
}

#' Retrieve document IDs
#'
#' Extracts a character vector of IDs of the modeled documents from a
#' \code{mallet_model} model.
#'
#' @param m a \code{mallet_model} object
#' @return a character vector
#'
#' @export
#'
doc_ids <- function (m) UseMethod("doc_ids")

#' @export
`doc_ids<-` <- function (m, value) UseMethod("doc_ids<-")

#' @export
doc_ids.mallet_model <- function (m) {
    if (!is.null(m$doc_ids)) {
        m$doc_ids
    } else if (!is.null(RTopicModel(m))) {
        rJava::.jcall(RTopicModel(m), "[S", "getDocumentNames")
    } else {
        stop("Neither the model object nor a pre-loaded list of IDs is available.
             To load the latter, use doc_ids(m) <- readLines(...)")
    }
}

#' @export
`doc_ids<-.mallet_model` <- function (m, value) {
    m$doc_ids <- value
    m
}

#' Retrieve model corpus vocabulary
#'
#' Extracts a character vector of the word types included in a \code{mallet_model}
#' model.
#'
#' @param m a \code{mallet_model} object
#' @return a character vector
#'
#' @export
#'
vocabulary <- function (m) UseMethod("vocabulary")

#' @export
`vocabulary<-` <- function (m, value) UseMethod("vocabulary<-")

#' @export
vocabulary.mallet_model <- function (m) {
    if (!is.null(m$vocab)) {
        m$vocab
    } else if (!is.null(RTopicModel(m))) {
        rJava::.jcall(RTopicModel(m), "[S", "getVocabulary")
    } else {
        stop("Neither the model object nor a pre-loaded vocabulary is available.
             To load the latter, use vocabulary(m) <- readLines(...)")
    }
}

#' @export
`vocabulary<-.mallet_model` <- function (m, value) {
    m$vocab <- value
    m
}

#' Get numeric word indices
#'
#' This shortcut function returns the numeric indices of specific words in the
#' model's vocabulary (corresponding to column indices in the topic-word
#' matrix).
#'
#' @param m a \code{mallet_model} object
#' @param words character vector of words to find indices of
#' @return numeric vector of indices into the vocabulary
#'
#' @export
#'
word_ids <- function (m, words) match(words, vocabulary(m))

#' The topic-words matrix
#'
#' Extracts the matrix from a \code{mallet_model} model with topics in rows and word
#' weights in columns. The word order is the same as that of the vocabulary for
#' the MALLET InstanceList.
#'
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and
#' unsmoothed weights as the "raw" form of the model; it is easier to reason
#' about and to do post-hoc calculations with, even if not rigorously correct.
#' It is up to the user to apply normalization and smoothing where appropriate.
#'
#' @param m a \code{mallet_model} object
#' @return a numeric matrix
#'
#' @export
#'
topic_words <- function (m, ...) UseMethod("topic_words")

#' @export
`topic_words<-` <- function (m, value) UseMethod("topic_words<-")

#' @export
topic_words.mallet_model <- function (m) {
    tw <- m$topic_words
    if (is.null(tw) && !is.null(RTopicModel(m))) {
        tw <- as(
            mallet::mallet.topic.words(RTopicModel(m),
                smoothed=FALSE, normalized=FALSE),
            "sparseMatrix"
        )
    }

    tw
}

#' @export
`topic_words<-.mallet_model` <- function (m, value) {
    m$topic_words <- value
    m
}

#' @export
#' @rdname load_doc_topics
load_topic_words <- function (m) {
    m$topic_words <- topic_words(m)
    m
}

#' Topic key words
#'
#' The most common way to summarize topics is to list their top-weighted words,
#' together with their topic weights. Though every topic assigns some
#' probability to every word in the whole vocabulary, we often disregard all but
#' its most frequent words.
#'
#' The data frame returned by this function supplies no new information not
#' already present in the topic-word matrix; it is in effect an aggressively
#' sparse representation of the full topic-word matrix. But it is so commonly
#' used that it makes more sense to store it on its own. Indeed, when analyzing
#' model outputs, one will often prefer to load just this data frame and the
#' doc-topics matrix into memory, rather than the full topic-word matrix.
#'
#' @param m a \code{mallet_model} object
#' @param n number of top words per topic to return (omit for all available)
#' @param weighting a function to transform the full topic-word matrix before
#'   calculating top-ranked words. If NULL, taken to be identity. Other
#'   possibilities include \code{\link{tw_blei_lafferty}} and
#'   \code{\link{tw_sievert_shirley}}.
#' @return a data frame with three columns, \code{topic} (indexed from 1),
#'   \code{word} (character), and \code{weight}
#'
#' @seealso \code{\link{tw_blei_lafferty}}, \code{\link{tw_sievert_shirley}}
#'
#' @export
top_words <- function (m, ...) UseMethod("top_words")

#' @export
`top_words<-` <- function (m, value) UseMethod("top_words<-")

#' @export
top_words.mallet_model <- function (m, n=NULL, weighting=NULL) {
    result <- NULL
    K <- n_topics(m)
    if (!is.null(m$top_words) && is.null(weighting)) {
        if (is.null(n)) {
            result <- m$top_words
        } else if (nrow(m$top_words) == n * K) {
            result <- m$top_words
        } else if (nrow(m$top_words) >= n * K) {
            i <- rep(seq(n), times=K) +
                rep(seq(0, by=nrow(m$top_words) / K, length.out=K), each=n)
            result <- m$top_words[i, ]
        }
    }

    if (is.null(result)) {
        tw <- topic_words(m)
        if (!is.null(tw)) {
            if (is.null(n)) {
                stop("For the full topic-words matrix, use topic_words(x)")
            }
            if (!is.null(weighting)) {
                tw <- weighting(tw)
            }
            if (n > ncol(tw)) {
                n <- ncol(tw)
            }
            ij <- top_n_row(tw, n)
            result <- dplyr::data_frame_(list(
                topic=~ ij[ , 1],
                word=~ vocabulary(m)[ij[ , 2]],
                weight=~ tw[ij]
            ))
        } else {
            warning(
"The model object is not available, and the pre-loaded list of top words is
either missing or too short.  To load the latter, use
    top_words(m) <- read.csv(...)"
            )
        }
    }

    result
}

#' @export
`top_words<-.mallet_model` <- function (m, value) {
    m$top_words <- value
    m
}

#' @export
#' @rdname load_doc_topics
load_top_words <- function (m, n=NULL, weighting=NULL) {
    m$top_words <- top_words(m, n, weighting)
    m
}

#' Retrieve metadata
#'
#' Get or set the metadata for an \code{mallet_model} model.
#'
#' The setter method drops any metadata rows that do not match the document IDs
#' for the model.
#'
#' @param m a \code{mallet_model} object
#' @param value a metadata data frame
#' @return a character vector
#'
#' @export
#'
metadata <- function (m) UseMethod("metadata")

#' @export
`metadata<-` <- function (m, value) UseMethod("metadata<-")

#' @export
metadata.mallet_model <- function (m) m$metadata

#' @export
`metadata<-.mallet_model` <- function (m, value) {
    m$metadata <- match_metadata(value, doc_ids(m))
    m
}

# utility function for ensuring metadata rows match and are in same order
# as doc-topic matrix
match_metadata <- function (meta, ids) {
    i <- match(ids, meta$id)
    if (any(is.na(i))) {
        NULL
    } else {
        meta[i, ]
    }
}

#' Retrieve estimated model hyperparameters
#'
#' Get or set the estimates for the model hyperparameters \eqn{\alpha} and
#' \eqn{\beta}. These are used in smoothing the document-topic and topic-word
#' matrices respectively.
#'
#' @param m a \code{mallet_model} object
#' @return a list with two elements, \code{alpha} (a vector, with one value per
#'   topic), and \code{beta} (a single number)
#'
#' @export
#'
hyperparameters <- function (m) UseMethod("hyperparameters")

#' @export
`hyperparameters<-` <- function (m, value) UseMethod("hyperparameters<-")

#' @export
hyperparameters.mallet_model <- function (m) {
    if (!is.null(m$hyper)) {
        m$hyper
    } else if (!is.null(RTopicModel(m))) {
        list(alpha=rJava::.jcall(RTopicModel(m), "[D", "getAlpha"),
             beta=rJava::.jfield(ParallelTopicModel(m), "D", "beta"))
    } else {
        stop(
"Neither the model object nor pre-loaded hyperparameters are available."
        )
    }
}

#' @export
`hyperparameters<-.mallet_model` <- function (m, value) {
    m$hyper <- value
    m
}

#' The model object
#'
#' A topic model is a complicated beastie, and exploring it requires keeping
#' track of a number of different kinds of data. The \code{mallet_model} object
#' strives to encapsulate some of this for you. Package users shouldn't need to
#' invoke the constructor explicitly; to obtain model objects, use either
#' \code{\link{train_model}} or \code{\link{load_mallet_model}}. The
#' \code{summary} method indicates which elements of the model have been loaded
#' into R's memory.
#'
#' Any of the parameters to the constructor can be omitted. No validation is
#' performed.
#'
#' @param doc_topics document-topic matrix
#' @param doc_ids vector of document ids corresponding to rows of
#'   \code{doc_topics}
#' @param vocab vector of word types corresponding to columns of
#'   \code{topic_words}
#' @param top_words data frame with top-ranked words and their weights for each
#'   topic
#' @param topic_words topic-word matrix (expected to be sparse)
#' @param params list of modeling parameters
#' @param hyper list of estimated hyperparameters \eqn{\alpha} and \eqn{\beta}
#' @param metadata data frame of metadata
#' @param model reference to an RTopicModel object from MALLET
#' @param ss final Gibbs sampling state represented as "simplified"
#'   \code{big.matrix}
#' @param instances reference to InstanceList (redundant if \code{model}
#'   specified)
#'
#' @export
mallet_model <- function (doc_topics=NULL,
                     doc_ids=NULL,
                     vocab=NULL,
                     top_words=NULL,
                     topic_words=NULL,
                     params=NULL,
                     hyper=NULL,
                     metadata=NULL,
                     model=NULL,
                     ss=NULL,
                     instances=NULL) {
    structure(list(doc_topics=doc_topics,
                   doc_ids=doc_ids,
                   vocab=vocab,
                   top_words=top_words,
                   topic_words=topic_words,
                   hyper=hyper,
                   metadata=metadata,
                   model=model,
                   params=params,
                   ss=ss),
              class="mallet_model")
}

#' @export
#' @rdname mallet_model
print.mallet_model <- function (x) {
    s <- stringr::str_c(
"A topic model created by MALLET

Number of topics: ", n_topics(x), "
Number of documents: ", n_docs(x), "
Number of word types: ", length(vocabulary(x)))

    cat(s)
    invisible(x)
}

#' @export
#' @rdname mallet_model
summary.mallet_model <- function (x) {
    members <- c("model", "instances", "doc_topics", "top_words", "topic_words",
                 "vocab", "doc_ids", "hyper", "ss")
    lst <- lapply(members, function (m) !is.null(x[[m]]))
    names(lst) <- members
    lst$n_topics <- n_topics(x)
    lst$n_docs <- n_docs(x)
    lst$n_words <- length(vocabulary(x))

    structure(lst, class="mallet_model_summary")
}

#' @export
#' @rdname mallet_model
print.mallet_model_summary <- function (x) {
    yesno <- function (m) ifelse(x[[m]], "yes", " no")

    s <- stringr::str_c(
"A topic model created by MALLET

Number of topics: ", x$n_topics, "
Number of documents: ", x$n_docs, "
Number of word types: ", x$n_words, "

Locally present:

MALLET model object:    ", yesno("model"), "
MALLET instances:       ", yesno("instances"), "
doc-topic matrix:       ", yesno("doc_topics"), "
top words data frame:   ", yesno("top_words"), "
topic-word matrix:      ", yesno("topic_words"), "
vocabulary:             ", yesno("vocab"), "
document ids:           ", yesno("doc_ids"), "
hyperparameters:        ", yesno("hyper"), "
sampling state:         ", yesno("ss")
    )

    cat(s)
    invisible(x)
}

#' Read in model outputs from files
#'
#' Load a model object from a set of files (like those produced from
#' \code{\link{write_mallet_model}}). Leave a filename out to skip loading that
#' piece of the puzzle.
#'
#' @param doc_topics_file document-topic matrix file (CSV)
#' @param doc_ids_file document id file (text, one ID per line)
#' @param vocab_file model vocabulary file (text, one word type per line)
#' @param top_words_file topic top word data frame file (CSV)
#' @param topic_words_file topic-word matrix file (CSV)
#' @param metadata_file metadata file (CSV or TSV)
#' @param params_file modeling parameters file (read with
#'   \code{\link[base]{dget}})
#' @param state_file CSV with simplified Gibbs sampling state (created by
#'   \code{\link{output_mallet_model}} or \code{\link{simplify_state}})
#'
#' @return a \code{mallet_model} object
#'
#' @seealso \code{\link{load_mallet_model_directory}},
#'   \code{\link{load_from_mallet_state}},
#'
#' @export
load_mallet_model <- function(
        doc_topics_file,
        doc_ids_file,
        vocab_file,
        top_words_file=NULL,
        topic_words_file=NULL,
        metadata_file=NULL,
        params_file=NULL,
        state_file=NULL) {

    if (!is.null(top_words_file)) {
        top_w <- dplyr::tbl_df(read.csv(top_words_file, as.is=TRUE,
                                        quote=""))
    } else {
        top_w <- NULL
    }

    if (!is.null(topic_words_file)) {
        tw <- as(read_matrix_csv(topic_words_file), "sparseMatrix")
    } else {
        tw <- NULL
    }

    if (!is.null(metadata_file)) {
        metadata <- read_dfr_metadata(metadata_file)
    } else {
        metadata <- NULL
    }

    if  (!is.null(params_file)) {
        p <- dget(params_file)
        params <- p$params
        hyper <- p$hyper
    } else {
        params <- NULL
        hyper <- NULL
    }

    if (!is.null(state_file)) {
        ss <- read_sampling_state(state_file)
    } else {
        ss <- NULL
    }

    ids <- readLines(doc_ids_file)

    result <- mallet_model(
        doc_topics=read_matrix_csv(doc_topics_file),
        doc_ids=ids,
        vocab=readLines(vocab_file),
        top_words=top_w,
        topic_words=tw,
        params=params,
        hyper=hyper,
        ss=ss,
        metadata=match_metadata(metadata, ids))

    result
}

#' Load a model with conventional filenames from a directory
#'
#' If you accept the defaults from \code{\link{write_mallet_model}}, you can read t
#' results back in simply by passing the directory name here.
#'
#' The expected filenames are \code{doc_topics.csv}, \code{doc_ids.txt},
#' \code{vocabulary.txt}, \code{top_words.csv}, \code{topic_words.csv}, and
#' \code{params.txt}.
#'
#' @param f directory name
#' @param load_topic_words logical: load the full topic-word matrix?
#' @param metadata_file document metadata file(s) (optional)
#'
#' @return \code{mallet_model} object
#'
#' @seealso \code{\link{load_mallet_model}},
#'   \code{\link{load_from_mallet_state}}
#'
#' @export
load_mallet_model_directory <- function (f, load_topic_words=FALSE,
                                    load_sampling_state=FALSE,
                                    metadata_file=NULL) {
    tw <- if (load_topic_words) file.path(f, "topic_words.csv") else NULL
    ss <- if (load_sampling_state) file.path(f, "state.csv") else NULL
    load_mallet_model(doc_topics_file=file.path(f, "doc_topics.csv"),
                 doc_ids_file=file.path(f, "doc_ids.txt"),
                 vocab_file=file.path(f, "vocabulary.txt"),
                 top_words_file=file.path(f, "top_words.csv"),
                 params_file=file.path(f, "params.txt"),
                 topic_words_file=tw,
                 state_file=ss,
                 metadata_file=metadata_file)
}

#' Load a model with files from dfrtopics 0.1
#'
#' The convention for exporting model outputs differed in earlier versions of
#' this package. This file loads in a folder of model outputs on the old
#' conventions. To skip loading some elements, set the file name to NULL.
#'
#' @param f directory name
#' @param doc_topics_file document-topics CSV (document topic proportions with a
#'   header row and an extra column of document IDs)
#' @param keys_file the "weighted keys" or top topic-words CSV with top \eqn{n}
#'   words in each topic for some \eqn{n}, together with their weights, and
#'   hyperparameter \eqn{\alpha} estimates (repeated \eqn{n} times for each
#'   topic). MALLET's own "topic keys" output is different.
#' @param vocab_file the model vocabulary, one word per line
#' @param params_file CSV with one data row with saved model parameters (fewer
#'   than in current version)
#' @param topic_words_file CSV with topic-word weights (no header)
#' @param simplified_state_file CSV with a "simplified" sampling state (same as
#'   produced by current \code{\link{simplify_state}}
#' @param metadata_file vector of metadata files to read in (optionally) and
#'   attach to model
#'
#' @return \code{\link{mallet_model}} object
#'
#' @export
load_mallet_model_legacy <- function (
        f=".",
        doc_topics_file=file.path(f, "doc_topics.csv"),
        keys_file=file.path(f, "keys.csv"),
        vocab_file=file.path(f, "vocab.txt"),
        params_file=file.path(f, "params.csv"),
        topic_words_file=NULL,
        simplified_state_file=NULL,
        metadata_file=NULL) {

    m <- list()
    if (!is.null(doc_topics_file)) {
        dtf <- read.csv(doc_topics_file, header=TRUE, as.is=TRUE)
        m$doc_ids <- dtf$id
        dtf$id <- NULL
        m$doc_topics <- as.matrix(dtf)
    }
    if (!is.null(keys_file)) {
        wkf <- read.csv(keys_file, header=TRUE, as.is=TRUE)
        m$hyper <- list(alpha=unique(wkf$alpha))
        m$top_words <- wkf[ , c("topic", "word", "weight")]
    }
    if (!is.null(vocab_file)) {
        m$vocab <- readLines(vocab_file)
    }
    if (!is.null(params_file)) {
        p <- read.csv(params_file)
        if (is.null(m$hyper)) m$hyper <- list()
        m$hyper$beta <- p$beta
        p$beta <- NULL
        m$params <- p
    }

    if (!is.null(topic_words_file)) {
        m$topic_words <- as(read_matrix_csv(topic_words_file), "sparseMatrix")
    }

    if (!is.null(simplified_state_file)) {
        m$ss <- read_sampling_state(simplified_state_file)
    }

    if (!is.null(metadata_file)) {
        m$metadata <- read_dfr_metadata(metadata_file)
    }

    do.call(mallet_model, m)
}

#' Load model from MALLET state output
#'
#' If you have created a topic model using command-line mallet or another tool,
#' this function loads that model into \code{\link{mallet_model}} form suitable
#' for use in this package. It uses the gzipped text file representing the Gibbs
#' sampling state. This state can be used to derive document-topic and
#' topic-word matrices. The model vocabulary and document ID list are obtained
#' from the MALLET instances file.
#'
#' @param mallet_state_file name of gzipped state file
#' @param simplified_state_file name of file to save "simplified" representation
#'   of the state to. If NULL, a temporary file will be used
#' @param instances_file location of MALLET instances file used to create the
#'   model. If NULL, this will be skipped, but the resulting model object will
#'   have missing vocabulary and document ID's.
#' @param keep_sampling_state If TRUE (default), the returned object will hold a
#'   reference to the sampling state \code{big.matrix} as well.
#' @param metadata_file metadata file (CSV or TSV; optional here)
#' @return a \code{\link{mallet_model}} object.
#'
#' @examples
#' \dontrun{
#' system("mallet train-topics --input instances.mallet \\
#'     --output-state topic-state.gz")
#' m <- load_from_mallet_state("topic-state.gz", "state.csv",
#'     "instances.mallet")
#' }
#'
#' @seealso \code{\link{load_mallet_model}},
#'   \code{\link{load_mallet_model_directory}},
#'   \code{\link{write_mallet_state}}
#'
#' @export
#'
load_from_mallet_state <- function (
        mallet_state_file,
        simplified_state_file=file.path(dirname(mallet_state_file),
                                        "state.csv"),
        instances_file=NULL,
        keep_sampling_state=TRUE,
        metadata_file=NULL) {

    gzf <- gzfile(mallet_state_file)
    pp <- readLines(gzf, n=3)
    on.exit(close(gzf))
    a <- stringr::str_match(pp[2], "^#alpha : ([0-9. ]+)$")
    b <- stringr::str_match(pp[3], "^#beta : ([0-9. ]+)$")
    if (is.na(a) || is.na(b)) {
        stop(
"hyperparameter notation missing. Is this really a file produced by
mallet train-topics --output-state?"
        )
    }

    hyper <- list(
        alpha=as.numeric(
            stringr::str_split(stringr::str_trim(a[1, 2]), " ")[[1]]
        ),
        beta=as.numeric(b[1, 2])
    )

    ss_temp <- F
    if (is.null(simplified_state_file)) {
        simplified_state_file <- tempfile()
        ss_temp <- T
    }

    simplify_state(mallet_state_file, simplified_state_file)

    if (!requireNamespace("bigtabulate", quietly=TRUE)) {
        stop(
"bigtabulate package required for model loading from sampling state.
Otherwise, the dplyr manipulation in memory is up to you."
        )
    }

    ss <- read_sampling_state(simplified_state_file)

    K <- length(hyper$alpha)
    docs <- bigtabulate::bigsplit(ss, c("doc", "topic"))
    dtl <- vapply(docs, function (i) sum(ss[i, "count"]), integer(1))
    doc_topics <- matrix(dtl, ncol=K)

    words <- bigtabulate::bigsplit(ss, c("topic", "type"),
                                   splitret="sparselist")
    twl <- vapply(words, function (i) sum(ss[i, "count"]), integer(1))
    twl_ij <- stringr::str_split_fixed(names(twl), ":", 2)
    topic_words <- Matrix::sparseMatrix(
        i=as.integer(twl_ij[ , 1]),
        j=as.integer(twl_ij[ , 2]),
        x=twl)

    doc_ids <- NULL
    vocab <- NULL
    il <- NULL
    if (!is.null(instances_file)) {
        il <- read_instances(instances_file)
        doc_ids <- instances_ids(il)
        vocab <- instances_vocabulary(il)
    }

    if (ss_temp) {
        unlink(simplified_state_file)
    }

    if (!keep_sampling_state) {
        ss <- NULL
    }

    meta <- NULL
    if (!is.null(metadata_file)) {
        meta=read_dfr_metadata(metadata_file)
    }

    mallet_model(
        doc_topics=doc_topics,
        topic_words=topic_words,
        hyper=hyper,
        ss=ss,
        doc_ids=doc_ids,
        vocab=vocab,
        instances=il,
        metadata=meta
    )
}


