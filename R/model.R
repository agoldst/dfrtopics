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
#' \code{\link{write_dfr_lda}}
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
#' @return a \code{\link{dfr_lda}} object holding the results
#'   
#' @seealso \code{\link{read_dfr_metadata}}, \code{\link{read_wordcounts}}, 
#' \code{\link{make_instances}}, \code{\link{train_model}}, 
#' \code{\link{write_dfr_lda}}
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
    result <- read_wordcounts(list.files(wordcounts_dirs, full.names=T)) %>%
        dfr_docs_frame() %>%
        make_instances(stoplist_file) %>%
        train_model(n_topics, ...)
    metadata(result) <- read_dfr_metadata(citations_files)
    result
}

#' A convenience function for saving all the model outputs at once.
#' 
#' Save a series of files with the results of an LDA run.
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
#' @param m \code{dfr_lda} object
#'   
#' @param output_dir where to save all the output files.
#'
#' @param save_instances if TRUE, extract the instance list from the trainer
#'   object and save it to \code{instances.mallet}
#'   
#' @param save_scaled if TRUE write a file of 2D coordinates for
#'   the topics
#'   
#' @export
#' 
write_dfr_lda <- function(m, output_dir=".",
                          n_top_words=50,
                          save_instances=F,
                          save_scaled=F) {
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
                quote=F, sep=",", row.names=F, col.names=T)
    message("Wrote ", keys_f)

    dt_f <- file.path(output_dir,"doc_topics.csv")
    write.table(doc_topics(m), dt_f,
                quote=F, sep=",", row.names=F, col.names=F)
    message("Wrote ", dt_f)

    state_f <- file.path(output_dir, "mallet_state.gz")
    write_mallet_state(m, state_f)
    message("Wrote ", state_f)

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
                    quote=F, sep=",", row.names=F, col.names=F)
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
#' @return a \code{dfr_lda} object
#' 
#' @seealso \code{\link{make_instances}}, \code{\link{make_instances}}, 
#'   \code{\link{model_documents}}, \code{\link{write_dfr_lda}}
#'
#' @export
#'   
train_model <- function(instances, n_topics,
                        alpha_sum=5, beta=0.01,
                        n_iters=200,
                        n_max_iters=10,
                        optimize_hyperparameters=T,
                        n_hyper_iters=20,
                        n_burn_in=50,
                        symmetric_alpha=F,
                        threads=4L,
                        seed=NULL,
                        metadata=NULL) {

    # Java number types are a problem with R's loose typing. RTopicModel
    # expects doubles everywhere; ParallelTopicModel expects ints
    # for integers. So we have to do some coercions to avoid rJava
    # complaints.

    trainer <- MalletLDA(as.numeric(n_topics),
                         as.numeric(alpha_sum),
                         as.numeric(beta))
    trainer$model$setNumThreads(as.integer(threads))
    if (!is.null(seed)) {
        trainer$model$setRandomSeed(as.integer(seed))
        message("MALLET random number seed set to ", seed)
    }

    trainer$loadDocuments(instances)

    if (optimize_hyperparameters) {
        trainer$model$setSymmetricAlpha(symmetric_alpha)
        trainer$setAlphaOptimization(as.numeric(n_hyper_iters),
                                     as.numeric(n_burn_in))
    }
    else {
        trainer$setAlphaOptimization(0, 0)
    }

    trainer$train(as.numeric(n_iters))
    trainer$maximize(as.numeric(n_max_iters))

    result <- dfr_lda(
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
            final_ll=trainer$model$modelLogLikelihood()
        ),
        doc_topics=mallet.doc.topics(trainer, smoothed=F, normalized=F)
    )

    # assign metadata; issue warning if it doesn't match
    if (!is.null(metadata)) {
        if (all(trainer$getDocumentNames() %in% metadata$id)) {
            metadata(result) <- metadata
        } else {
            warning(
"Supplied metadata does not match instance document ID's.
Model metadata will be NULL.
To set metadata later, use metadata(m) <- ..."
            )
        }
    }

    result
}

#' Access the number of topics in the model
#'
#' Returns the number of topics in a model.
#'
#' @export
n_topics <- function (x) UseMethod("n_topics")

#' @export
n_topics.dfr_lda <- function (x) {
    if (!is.null(x$model)) {
        x$model$model$numTopics
    } else if (!is.null(x$doc_topics)) {
        ncol(x$doc_topics)
    } else if (!is.null(x$topic_words)) {
        nrow(x$topic_words)
    } else {
        NULL # return null if we haven't loaded enough information yet
    }
}

#' Access the number of documents modeled
#'
#' Returns the number of documents modeled.
#'
#' @export
n_docs <- function (x) UseMethod("n_docs")

#' @export
n_docs.dfr_lda <- function (x) {
    if (!is.null(x$doc_topics)) {
        nrow(x$doc_topics)
    } else if (!is.null(x$doc_ids)) {
        length(x$doc_ids)
    } else if (!is.null(x$model)) {
        x$model$instances$size()
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
modeling_parameters  <- function (x) UseMethod("modeling_parameters")

#' @export
modeling_parameters.dfr_lda  <- function (x) x$params
 

#' Access MALLET's glue model object
#' 
#' This function returns a reference to the top-level Java object representing
#' an LDA model.
#' 
#' For its R interface, MALLET uses a class RTopicModel. This has some
#' convenience methods for accessing and manipulating a topic model from R using
#' rJava. Most of the modeling functionality is carried out by a
#' ParallelTopicModel instance, which is a data member of this class.
#' 
#' Java's strict typing means you'll have some funtimes with 
#' \code{\link[rJava]{.jcall}}. To index into array-like objects from Java,
#' apply \code{\link[base]{as.integer}} to parameters.
#' 
#' @param x a \code{dfr_lda} object
#' @return a reference to the RTopicModel object
#' @seealso \code{\link{ParallelTopicModel}}
#' @export
#' 
RTopicModel <- function (x) UseMethod("RTopicModel")

#' @export
RTopicModel.dfr_lda <- function (x) x$model

#' Access MALLET's model object
#' 
#' This function returns a reference to the main Java object representing an LDA
#' model in MALLET.
#' 
#' For its R interface, MALLET uses a class RTopicModel, but most of the
#' modeling functionality is carried out by a ParallelTopicModel instance. This
#' object is reachable from R. Once you have the reference to it here, you can
#' use rJava to access all its public methods and members.
#' 
#' Java's strict typing means you'll have some funtimes with 
#' \code{\link[rJava]{.jcall}}. To index into array-like objects from Java,
#' apply \code{\link[base]{as.integer}} to parameters.
#' 
#' @param x a \code{dfr_lda} object
#' @return a reference to the ParallelTopicModel object
#' @seealso \code{\link{RTopicModel}}
#' @export
#' 
ParallelTopicModel <- function (x) UseMethod("ParallelTopicModel")

#' @export
ParallelTopicModel.dfr_lda <- function (x) x$model$model

#' Access the InstanceList stored by a model
#' 
#' MALLET models store a reference to their source documents. This is an
#' accessor function for that reference.
#' 
#' @param x a \code{dfr_lda} object
#' @return a reference to the InstanceList object, or NULL if not available
#'   
#' @export
#' 
instances <- function (x) UseMethod("instances")

#' @export
`instances<-` <- function (x, value) UseMethod("instances<-")

#' @export
instances.dfr_lda <- function (x) {
    obj <- RTopicModel(x)
    if (is.null(obj)) {
        warning("The model object is not available")
        NULL
    } else {
        obj$instances
    }
}

#' @export
`instances<-.dfr_lda` <- function (x, value) {
    x$instances <- value
    x
}

#' The document-topic matrix
#' 
#' Extracts the matrix from a \code{dfr_lda} model with documents in rows and
#' topic weights in columns. The document order is the same as the ordering of
#' the documents supplied to MALLET.
#' 
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and
#' unsmoothed weights as the "raw" form of the model; this is not strictly
#' correct, it is easier to reason about and to do post-hoc calculations with.
#' It is up to t user to apply normalization and smoothing where appropriate.
#' 
#' @param x a \code{dfr_lda} object
#' @return a numeric matrix
#'   
#' @export
#' 
doc_topics <- function (x) UseMethod("doc_topics")

#' @export
`doc_topics<-` <- function (x, value) UseMethod("doc_topics<-")

#' @export
doc_topics.dfr_lda <- function (x) {
    if (!is.null(x$doc_topics))  {
        x$doc_topics
    } else if (!is.null(x$model)) {
        mallet.doc.topics(x$model, smoothed=F, normalized=F)
    } else {
        NULL
    }
}

#' @export
`doc_topics<-.dfr_lda` <- function (x, value) {
    x$doc_topics <- value
    x
}

#' Retrieve document IDs
#' 
#' Extracts a character vector of IDs of the modeled documents from a
#' \code{dfr_lda} model.
#' 
#' @param x a \code{dfr_lda} object
#' @return a character vector
#'   
#' @export
#' 
doc_ids <- function (x) UseMethod("doc_ids")

#' @export
`doc_ids<-` <- function (x, value) UseMethod("doc_ids<-")

#' @export
doc_ids.dfr_lda <- function (x) {
    if (!is.null(x$doc_ids)) {
        x$doc_ids
    } else if (!is.null(x$model)) {
        x$model$getDocumentNames()
    } else {
        stop("Neither the model object nor a pre-loaded list of IDs is available.
             To load the latter, use doc_ids(x) <- readLines(...)")
    }
}

#' @export
`doc_ids<-.dfr_lda` <- function (x, value) {
    x$doc_ids <- value
    x
}

#' Retrieve model corpus vocabulary
#' 
#' Extracts a character vector of the word types included in a \code{dfr_lda}
#' model.
#' 
#' @param x a \code{dfr_lda} object
#' @return a character vector
#'   
#' @export
#' 
vocabulary <- function (x) UseMethod("vocabulary")

#' @export
`vocabulary<-` <- function (x, value) UseMethod("vocabulary<-")

#' @export
vocabulary.dfr_lda <- function (x) {
    if (!is.null(x$vocab)) {
        x$vocab
    } else if (!is.null(x$model)) {
        x$model$getVocabulary()
    } else {
        stop("Neither the model object nor a pre-loaded vocabulary is available.
             To load the latter, use vocabulary(x) <- readLines(...)")
    }
}

#' @export
`vocabulary<-.dfr_lda` <- function (x, value) {
    x$vocab <- value
    x
}

#' The topic-words matrix
#' 
#' Extracts the matrix from a \code{dfr_lda} model with topics in rows and word
#' weights in columns. The word order is the same as that of the vocabulary for
#' the MALLET InstanceList.
#' 
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and
#' unsmoothed weights as the "raw" form of the model; it is easier to reason
#' about and to do post-hoc calculations with, even if not rigorously correct.
#' It is up to the user to apply normalization and smoothing where appropriate.
#' 
#' @param x a \code{dfr_lda} object
#' @return a numeric matrix
#'   
#' @export
#' 
topic_words <- function (x, ...) UseMethod("topic_words")

#' @export
`topic_words<-` <- function (x, value) UseMethod("topic_words<-")

#' @export
topic_words.dfr_lda <- function (x) {
    m <- x$topic_words
    if (is.null(m) && !is.null(x$model)) {
        library("Matrix")
        m <- as(mallet.topic.words(x$model, smoothed=F, normalized=F),
                "sparseMatrix")
    }

    m
}

#' @export
`topic_words<-.dfr_lda` <- function (x, value) {
    x$topic_words <- value
    x
}

#' Topic key words
#' 
#' The most common way to summarize topics is to list their top-weighted words, 
#' together with their topic weights. Though every topic assigns some 
#' probability to every term in the whole vocabulary, we often disregard all but
#' its most frequent terms.
#' 
#' The data frame returned by this function supplies no new information not 
#' already present in the topic-word matrix; it is in effect an aggressively 
#' sparse representation of the full topic-word matrix. But it is so commonly 
#' used that it makes more sense to store it on its own. Indeed, when analyzing 
#' model outputs, one will often prefer to load just this data frame and the 
#' doc-topics matrix into memory, rather than the full topic-word matrix.
#' 
#' @param x a \code{dfr_lda} object
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
top_words <- function (x, ...) UseMethod("top_words")

#' @export
`top_words<-` <- function (x, value) UseMethod("top_words<-")

#' @export
top_words.dfr_lda <- function (x, n=NULL, weighting=NULL) {
    result <- NULL
    K <- n_topics(x)
    if (!is.null(x$top_words) && is.null(weighting)) {
        if (is.null(n)) {
            result <- x$top_words
        } else if (nrow(x$top_words) == n * K) {
            result <- x$top_words
        } else if (nrow(x$top_words) >= n * K) {
            i <- rep(seq(n), times=K) +
                rep(seq(0, nrow(x$top_words) / n * (n - 1), by=n), each=K)
            result <- x$top_words[i, ]
        }
    }
    
    if (is.null(result)) {
        tw <- topic_words(x)
        if (!is.null(tw)) {
            if (is.null(n)) {
                stop("For the full topic-words matrix, use topic_words(x)")
            }
            if (!is.null(weighting)) {
                tw <- weighting(tw)
            }
            ij <- top_n_row(tw, n)
            result <- data_frame_(list(
                topic=~ ij[ , 1],
                word=~ vocabulary(x)[ij[ , 2]],
                weight=~ tw[ij]
            ))
        } else {
            warning(
"The model object is not available, and the pre-loaded list of top words is 
either missing or too short.  To load the latter, use
    top_words(x) <- read.csv(...)"
            )
        }
    }

    result
}

#' @export
`top_words<-.dfr_lda` <- function (x, value) {
    x$top_words <- value
    x
}


#' Retrieve metadata
#' 
#' Get or set the metadata for an \code{dfr_lda} model.
#' 
#' The setter method drops any metadata rows that do not match the document IDs 
#' for the model.
#' 
#' @param x a \code{dfr_lda} object
#' @param value a metadata data frame
#' @return a character vector
#'   
#' @export
#' 
metadata <- function (x) UseMethod("metadata")

#' @export
`metadata<-` <- function (x, value) UseMethod("metadata<-")

#' @export
metadata.dfr_lda <- function (x) x$metadata

#' @export
`metadata<-.dfr_lda` <- function (x, value) {
    ids <- doc_ids(x)
    i <- match(ids, value$id)
    if (any(is.na(i))) {
        stop("Supplied data frame does not have rows matching all document IDs.")
    }
    x$metadata <- value[i, ]
    x
}

#' Retrieve estimated model hyperparameters
#' 
#' Get or set the estimates for the model hyperparameters \eqn{\alpha} and
#' \eqn{\beta}. These are used in smoothing the document-topic and topic-word
#' matrices respectively.
#' 
#' @param x a \code{dfr_lda} object
#' @return a list with two elements, \code{alpha} (a vector, with one value per
#'   topic), and \code{beta} (a single number)
#'   
#' @export
#' 
hyperparameters <- function (x) UseMethod("hyperparameters")

#' @export
`hyperparameters<-` <- function (x, value) UseMethod("hyperparameters<-")

#' @export
hyperparameters.dfr_lda <- function (x) {
    if (!is.null(x$hyper)) {
        x$hyper
    } else if (!is.null(x$model)) {
        list(alpha=x$model$getAlpha(),
             beta=x$model$model$beta)
    } else {
        stop(
"Neither the model object nor pre-loaded hyperparameters are available."
        )
    }
}

#' @export
`hyperparameters<-.dfr_lda` <- function (x, value) {
    x$hyper <- value
    x
}

#' The model object
#' 
#' A topic model is a complicated beastie, and exploring it requires keeping
#' track of a number of different kinds of data. The \code{dfr_lda} object
#' strives to encapsulate some of this for you. Package users shouldn't need to
#' invoke the constructor explicitly; to obtain model objects, use either
#' \code{\link{train_model}} or \code{\link{load_dfr_lda}}. The \code{summary}
#' method indicates which elements of the model have been loaded into R's
#' memory.
#' 
#' Any of the parameters to the constructor can be omitted.
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
#' @param state final Gibbs sampling state
#'   
#' @export
dfr_lda <- function (doc_topics=NULL,
                     doc_ids=NULL,
                     vocab=NULL,
                     top_words=NULL,
                     topic_words=NULL,
                     params=NULL,
                     hyper=NULL,
                     metadata=NULL,
                     model=NULL,
                     state=NULL) {
    structure(list(doc_topics=doc_topics,
                   doc_ids=doc_ids,
                   vocab=vocab,
                   top_words=top_words,
                   topic_words=topic_words,
                   hyper=hyper,
                   metadata=metadata,
                   model=model,
                   params=params,
                   state=state),
              class="dfr_lda")
}

#' @export
#' @rdname dfr_lda
print.dfr_lda <- function (x) {
    s <- str_c(
"A topic model created by MALLET

Number of topics: ", n_topics(x), "
Number of documents: ", n_docs(x), "
Number of word types: ", length(vocabulary(x)))

    cat(s)
    invisible(x)
}

#' @export
#' @rdname dfr_lda
summary.dfr_lda <- function (x) {
    members <- c("model", "instances", "doc_topics", "top_words", "topic_words",
                 "vocab", "doc_ids", "hyper")
    lst <- lapply(members, function (m) !is.null(x[[m]]))
    names(lst) <- members
    lst$n_topics <- n_topics(x)
    lst$n_docs <- n_docs(x)
    lst$n_words <- length(vocabulary(x))

    structure(lst, class="dfr_lda_summary")
}

#' @export
#' @rdname dfr_lda
print.dfr_lda_summary <- function (x) {
    yesno <- function (m) ifelse(x[[m]], "yes", " no")

    s <- str_c(
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
hyperparameters:        ", yesno("hyper")
    )

    cat(s)
    invisible(x)
}

#' Read in model outputs from files
#' 
#' Load a model object from a set of files (like those produced from
#' \code{\link{write_dfr_lda}}). Leave a filename out to skip loading that piece
#' of the puzzle.
#' 
#' @param doc_topics_file document-topic matrix file (CSV)
#' @param doc_ids_file document id file (text, one ID per line)
#' @param vocab_file model vocabulary file (text, one word type per line)
#' @param top_words_file topic top word data frame file (CSV)
#' @param topic_words_file topic-word matrix file (CSV)
#' @param metadata_file metadata file (CSV or TSV)
#' @param params_file modeling parameters file (read with
#'   \code{\link[base]{dget}})
#'   
#' @return a \code{dfr_lda} object
#'   
#' @export
load_dfr_lda <- function(
        doc_topics_file,
        doc_ids_file,
        vocab_file,
        top_words_file=NULL,
        topic_words_file=NULL,
        metadata_file=NULL,
        params_file=NULL) {

    if (!is.null(top_words_file)) {
        top_w <- tbl_df(read.csv(top_words_file, as.is=T))
    } else {
        top_w <- NULL
    }

    if (!is.null(topic_words_file)) {
        library("Matrix")
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
        

    result <- dfr_lda(
        doc_topics=read_matrix_csv(doc_topics_file),
        doc_ids=readLines(doc_ids_file),
        vocab=readLines(vocab_file),
        top_words=top_w,
        topic_words=tw,
        params=params,
        hyper=hyper)

    metadata(result) <- metadata

    result
}

#' Load a model with conventional filenames from a directory
#' 
#' If you accept the defaults from \code{\link{write_dfr_lda}}, you can read t
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
#' @return \code{dfr_lda} object
#'   
#' @export
load_dfr_lda_directory <- function (f, load_topic_words=F,
                                    metadata_file=NULL) {
    tw <- if (load_topic_words) file.path(f, "topic_words.csv") else NULL
    load_dfr_lda(doc_topics_file=file.path(f, "doc_topics.csv"),
                 doc_ids_file=file.path(f, "doc_ids.txt"),
                 vocab_file=file.path(f, "vocabulary.txt"),
                 top_words_file=file.path(f, "top_words.csv"),
                 topic_words_file=tw,
                 metadata_file=metadata_file,
                 params_file=file.path(f, "params.txt"))
}
                 
#' Load a model with files from dfrtopics 0.1 (unimplemented)
#' 
#' The convention for exporting model outputs differed in earlier versions of
#' this package. This file loads in a folder of model outputs on the old
#' conventions.
#' 
#' @param f directory name
#' @param load_topic_words
#'   
#' @export
load_dfr_lda_legacy <- function (f, load_topic_words=F,
                                 metadata_file=NULL) {
    stop("Unimplemented.")
    # TODO implement this
}




