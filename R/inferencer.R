# functions for using MALLET's topic-inference functionality: given an
# existing topic model, estimate topic proportions for new documents

#' Get a topic inferencer object
#'
#' Extracts a reference to a topic-inferencer Java object from a topic model.
#' The model have its \code{\link{ParallelTopicModel}} object available (see
#' \code{\link{train_model}}.
#'
#' @param m \code{\link{mallet_model}} object
#' @return a reference to the Java topic inferencer object
#'
#' @export
inferencer <- function (m) {
    ParallelTopicModel(m)$getInferencer()
}

#' Save an inferencer object to a file
#'
#' Saves an inferencer from \code{\link{inferencer}} to disk for later use.
#'
#' @param inf a reference to a topic inferencer, from \code{\link{inferencer}}
#' @param out_file the name of a file to save to (will overwrite an existing
#'   file)
#'
#' @export
#'
write_inferencer <- function (inf, out_file) {
    fos <- .jnew("java/io/FileOutputStream", out_file)
    oos <- .jnew("java/io/ObjectOutputStream",
                 .jcast(fos, "java/io/OutputStream"))
    oos$writeObject(inf)
    oos$close()
}

#' Retrieve an inferencer object from a file
#'
#' Loads a topic-inferencer saved to disk back into memory
#'
#' @param filename file to read from
#' @return reference to Java topic inferencer object
#'
#' @export
read_inferencer <- function (filename) {
    J("cc.mallet.topics.TopicInferencer")$read(
        new(J("java.io.File"), path.expand(filename))
    )
}

#' Infer document topics
#'
#' Given an already-trained topic model, infer topic proportions for new
#' documents. This is like the Gibbs sampling process for making a topic model,
#' but the topic-word proportions are not updated.
#'
#' @param m either a topic inferencer object from
#'   \code{\link{read_inferencer}} or \code{\link{inferencer}} or a
#'   \code{mallet_model} object
#' @param instances an InstanceList object. It must be compatible i.e., (its
#'   vocabulary must correspond) with the instances on which \code{inferencer}
#'   was trained. Use \code{\link{compatible_instances}} to generate this.
#' @param n_iterations number of Gibbs sampling iterations
#' @param sampling_interval thinning interval
#' @param burn_in number of burn-in iterations
#' @param seed integer random seed; set for reproducibility
#'
#' @return a model object of class \code{\link{mallet_model_inferred}}, which
#'   inherits from \code{\link{mallet_model}}. This does not have all the
#'   elements of t original topic model, however; the new value of interest is
#'   the matrix of estimated document-topic proportions, accessible via
#'   \code{\link{doc_topics}}. The inferencer sampling state and raw weights are
#'   not accessible.
#'
#' @examples
#' \dontrun{
#' # beginning with a model m and new documents docs:
#' inferred_m <- make_instances(docs) %>%
#'     infer_topics(m, .)
#'
#' # extract new doc-topic matrix (NB. topic proportions)
#' doc_topics(inferred_m)
#' # or a convenient data frame of high-ranking topics in each doc
#' docs_top_topics(inferred_m, n=3)
#' # or, similarly, but for high-ranking documents in each topic
#' top_docs(inferred_m, n=3)
#' }
#'
#' @export
infer_topics <- function (m, instances, ...) {
    UseMethod("infer_topics")
}

#' @export
infer_topics.default <- function (m, instances,
        n_iterations=100,
        sampling_interval=10, # aka "thinning"
        burn_in=10,
        seed=NULL) {
    iter <- instances$iterator()
    n_iterations <- as.integer(n_iterations)
    sampling_interval <- as.integer(sampling_interval)
    burn_in <- as.integer(burn_in)
    if (!is.null(seed)) {
        m$setRandomSeed(as.integer(seed))
    }

    doc_topics <- vector("list", instances$size())
    for (j in 1:instances$size()) {
        inst <- .jcall(iter, "Ljava/lang/Object;", "next")
        doc_topics[[j]] <- m$getSampledDistribution(inst,
            n_iterations, sampling_interval, burn_in)
    }


    doc_topics <- do.call(rbind, doc_topics)
    mallet_model_inferred(
        doc_topics=doc_topics,
        doc_ids=instances_ids(instances),
        vocabulary=instances_vocabulary(instances),
        params=list(n_iterations=n_iterations,
                    sampling_interval=sampling_interval,
                    burn_in=burn_in,
                    seed=seed),
        inf=m,
        instances=instances
    )
}

#' @export
infer_topics.mallet_model <- function (m, instances, ...) {
    # can't get NextMethod to work, so what the hey
    infer_topics.default(m=inferencer(m), instances, ...)
}

#' An inferred topic model of new documents
#'
#' Constructs a representation of the results of using MALLET's topic-inference
#' functionality. Users should not call this constructor directly; use
#' \code{\link{infer_topics}} instead. \code{mallet_model_inferred} objects can
#' be used as parameters to functions for a regular \code{mallet_model}, but any
#' function that requires the presence of model outputs other than the
#' document-topic matrix will not work. Note also that MALLET supplies smoothed
#' and normalized topic weights for documents.
#'
#' @export
#'
mallet_model_inferred <- function (
        doc_topics=NULL,
        doc_ids=NULL,
        vocabulary=NULL,
        params=NULL,
        inf=NULL,
        instances=NULL,
        parent=NULL) {
    structure(
        list(doc_topics=doc_topics,
             doc_ids=doc_ids,
             vocabulary=vocabulary,
             params=params,
             inf=inferencer,
             instances=instances),
        class=c("mallet_model_inferred", "mallet_model")
    )
}

#' @export
#' @rdname infer_topics
print.mallet_model_inferred <- function (x) {
    s <- stringr::str_c(
'A model of documents inferred from a previously trained model by MALLET

Number of topics: ', ncol(x$doc_topics), '
Number of documents: ', nrow(x$doc_topics)
    )

    cat(s)
    invisible(x)
}

#' @export
#' @rdname infer_topics
summary.mallet_model_inferred <- function (x) {
    x # just print the object
}

#' @export
#' @rdname infer_topics
docs_top_topics.mallet_model_inferred <- function (m, n) {
    NextMethod("docs_top_topics", weighting=identity)
}

#' @export
#' @rdname infer_topics
top_docs.mallet_model_inferred <- function (m, n) {
    NextMethod("top_docs", weighting=identity)
}

#' Combine a topic model with inferred document topics
#'
#' This function joins together two models that share enough parameters for this
#' operation to make some sense, for example, an ordinary topic model and a
#' model of further documents created by \code{\link{infer_topics}}.
#'
#' @param x, y models with the same topic-word matrices and vocabulary
#' @return a further pseudo-model, inheriting the topic-word matrix of the
#'   arguments, and with a document-topic matrix formed by stacking the
#'   corresponding matrices of the arguments. Note that merging an inferred
#'   topic model means that the document-topic matrix must be row-normalized.
#'
#' @export
merge.mallet_model <- function (x, y) {
    stopifnot(all.equal(vocabulary(x), vocabulary(y)))
    stopifnot(n_topics(x) == n_topics(y))

    result <- list()

    dtx <- doc_topics(x)
    dty <- doc_topics(y)
    if (inherits(x, "mallet_model_inferred") &&
            !inherits(y, "mallet_model_inferred")) {
        dty <- dt_smooth_normalize(y)(dty)
    } else if (inherits(y, "mallet_model_inferred") &&
            !inherits(x, "mallet_model_inferred")) {
        dtx <- dt_smooth_normalize(x)(dtx)
    }

    result$doc_topics <- rbind(dtx, dty)
    result$doc_ids <- c(doc_ids(x), doc_ids(y))
    result$vocabulary <- vocabulary(x)

    twx <- topic_words(x)
    twy <- topic_words(y)
    if (is.null(twx)) {
        result$topic_words <- twy
    } else {
        if (!is.null(twy) && !all.equal(twx, twy)) {
            stop(
"Cannot merge models with distinct topic-word matrices."
            )
        } else {
            result$topic_words <- twx
        }
    }

    topx <- x$top_words
    topy <- y$top_words
    if (is.null(topx)) {
        result$top_words <- topy
    } else {
        if (!is.null(topy) && !all.equal(topx, topy)) {
            stop(
"Cannot merge models with distinct topic-top-word data frames."
            )
        } else {
            result$top_words <- topx
        }
    }

    for (key in c("inf", "model", "hyper")) {
        if (!is.null(x[[key]])) {
            result[[key]] <- x[[key]]
        } else if (!is.null(y[[key]])) {
            result[[key]] <- y[[key]]
        }
    }

    result$param <- list(modeling_parameters(x), modeling_parameters(y))
    result$instances <- list(instances(x), instances(y))

    structure(result,
              class=c("mallet_model_merged", "mallet_model"))
}
