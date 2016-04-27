#' Convert word counts data frame to Structural Topic Model inputs
#'
#' This is a helper function for transforming word-counts-type data into the
#' form expected by \code{\link[stm]{stm}} from the \pkg{stm} package. Given a
#' data frame from \code{\link{read_wordcounts}} (and optionally a frame of
#' metadata), it yields a list whose components can be passed as arguments to
#' \code{stm}. See below for example usage.
#'
#' At present, dfrtopics functions for exploring topic models cannot be applied
#' to \code{stm} objects. That may change in future, but \pkg{stm} supplies a
#' rich array of exploratory and inferential functions itself.
#'
#' Some memory-management might be a good idea here. Once you have the list of
#' inputs from this function, you no longer need the \code{counts} object.
#'
#' @param counts data frame as returned from \code{\link{read_wordcounts}}
#' @param meta optional metadata data frame (with an \code{id} column to be
#'   matched against \code{counts$id}).
#'
#' @return a list with elements \code{documents}, \code{vocab}, \code{ids} and
#'   \code{data} (if \code{meta} has been supplied). \code{ids} are for user
#'   convenience and are not required by \code{stm}, which matches by index. The
#'   other elements are suitable to be passed as the parameters of those names
#'   to \code{\link[stm]{stm}}.
#'
#' @examples \dontrun{
#' library(stm)
#' counts <- read_wordcounts(Sys.glob("wordcounts/*.CSV"))
#' meta <- read_dfr_metadata("citations.tsv")
#' corpus <- wordcounts_stm_inputs(counts, meta)
#' m <- stm(documents=corpus$documents,
#'     vocab=corpus$vocab,
#'     data=corpus$data,
#'     K=25,
#'     prevalence= ~ journaltitle)
#' }
#'
#' @export
#'
wordcounts_stm_inputs <- function (counts, meta=NULL) {
    if (!requireNamespace("stm", quietly=TRUE)) {
        stop("Please install stm from CRAN first.")
    }
    tdm <- wordcounts_Matrix(counts)
    result <- list(
        documents=stm::readCorpus(Matrix::t(x), type="Matrix")$documents,
        vocab=rownames(tdm),
        ids=colnames(tdm)
    )
    if (!is.null(meta)) {
        result$data <- meta[match(colnames(tdm), meta$id), ]
    }
    result
}

#' Convert word counts data frame to a DocumentTermMatrix
#'
#' The \pkg{topicmodels} package expects corpora in the form of a
#' \code{\link[tm]{DocumentTermMatrix}} (defined by the \pkg{tm} package). This
#' function converts a data frame of word counts to this form.
#'
#' @seealso \code{\link{wordcounts_Matrix}}
#'
#' @export
wordcounts_DocumentTermMatrix <- function (counts) {
    if (!requireNamespace("tm", quietly=TRUE)) {
        stop("Please install tm from CRAN first.")
    }

    dtm <- slam::as.simple_triplet_matrix(wordcounts_Matrix(counts))
    # tm::weightTf is the Text-Mining-Infrastructure-for-R (tm)-branded
    # identity function, nonetheless required here
    tm::as.DocumentTermMatrix(dtm, weighting=tm::weightTf)
}

#' Glue for results from other topic-modeling packages
#'
#' Besides this package and \pkg{mallet}, which it builds on, there are several
#' other topic-modeling packages for R. \pkg{topicmodels} provides a
#' topic-modeling infrastructure as well as supplying functions for estimating
#' both ordinary LDA and Correlated Topic Models several ways.  I have tried to
#' make it possible to use at least some of \pkg{dfrtopics}'s functions with
#' \pkg{topicmodels} results from the \code{\link[topicmodels]{LDA}} function. I
#' have also wished to make it possible to interface with the \pkg{stm} package
#' and its Structural Topic Model (\code{\link[stm]{stm}}). Given a model from
#' one of these two packages, apply \code{foreign_model} to obtain an object
#' that can be used with (some of) the functions in \pkg{dfrtopics}.
#'
#' Most of this package emerged out of my particular need to wrangle MALLET, and
#' as a result I did not take account of the \pkg{topicmodels} infrastructure
#' (which, furthermore, has been refined over time). I wish I had, since that
#' infrastructure is elegant and extensible, using S4 rather than S3. For now, I
#' am not going to overhaul my own class structure. As a stopgap, the strategy
#' adopted here is to provide "wrapper" objects for
#' \code{\link[topicmodels]{TopicModel-class}} and \code{\link[stm]{stm}}
#' objects that can respond to many of the same messages as
#' \code{\link{mallet_model}} does. This is not the best way to do things, but
#' it's straightforward.
#'
#' Not all functionality is supported. Note too that \code{doc_topics} and
#' \code{topic_words} applied to a \code{TopicModel} or an \code{stm} return
#' \emph{parameter estimates} of the probabilities of topics in documents or
#' words in topics. In MALLET terminology these are "smoothed and normalized,"
#' not raw sampling weights. For this reason \code{\link{hyperparameters}} does
#' \emph{not} return true hyperparameter values for these models---which are, in
#' any case, defined variously for the various estimation procedures. Instead,
#' \code{hyperparameters} returns dummy values of zero so that
#' \code{\link{tw_smooth_normalize}} and \code{\link{dt_smooth_normalize}} will
#' not incorrectly add anything to the posteriors. The actual hyperparameters
#' should be retrieved from the underlying model if needed.
#'
#' @param x model for translation from \pkg{topicmodels} or \pkg{stm}
#'
#' @param metadata metadata frame to attach to model (row sequence must match
#'   document sequence used by \code{x})
#'
#' @return a wrapper object which will work with most functions of an object of
#'   class \code{mallet_model}.
#'
#' @export
foreign_model <- function (x, metadata=NULL) {
    if (is(x, "LDA")) {
        result <- structure(list(LDA=x),
            class=c("TopicModel_glue", "mallet_model"))
    } else if (inherits(x, "stm")) {
        result <- x
        if (!is.null(metadata)) {
            if (nrow(metadata) != nrow(x$theta)) {
                warning(
"Number of metadata entries does not match number of modeled documents. No
metadata will be stored.")
                metadata <- NULL
            } else {
                result$doc_ids <- metadata$id
            }
        }
        class(result) <- c("stm_glue", "TopicModel_glue",
                           "mallet_model", "stm")
    } else {
        stop("No conversion method is available for models of this type.")
    }

    metadata(result) <- metadata
    result
}

#' @export
print.TopicModel_glue <- function (x, ...) {
    cat("A dfrtopics wrapper around a topic model from another package:\n")
    print(x$LDA, ...)
}

#' @export
summary.TopicModel_glue <- function (x, ...) {
    structure(list(s=summary(x$LDA, ...),
                   c=class(x$LDA)),
              class="TopicModel_glue_summary")
}

#' @export
print.TopicModel_glue_summary <- function (x) {
    cat("A dfrtopics wrapper around a topic model from another package:\n")
    cat("Source type: ", x$c, "\n")
    print(x$s)
}

#' @export
summary.stm_glue <- function (x, ...) {
    structure(list(s=summary.STM(x, ...),
                   c="STM"),
              class="TopicModel_glue_summary")
}

#' @export
doc_topics.TopicModel_glue <- function (x)
    topicmodels::posterior(x$LDA)$topics

#' @export
topic_words.TopicModel_glue<- function (x)
    topicmodels::posterior(x$LDA)$terms

#' @export
vocabulary.TopicModel_glue <- function (x) x$LDA@terms

#' @export
n_topics.TopicModel_glue <- function (x) x$LDA@k

#' @export
n_docs.TopicModel_glue <- function (x) nrow(x$LDA@gamma)

#' @export
doc_ids.TopicModel_glue <- function (x) x$LDA@documents

#' @export
hyperparameters.TopicModel_glue <- function (x) {
    list(alpha=rep(0, n_topics(x)), beta=0)
    # these are not true hyperparameters, just dummies to reflect the fact
    # that the only place hyperparameters() is used here is in smoothing
    # estimated doc-topic / topic-word matrices. But these matrices
    # are already posterior estimates (already "smoothed") in this case.
    # TODO topic_words and doc_topics in general need those smoothing
    # parameters after all (sigh)

    # actual hyperparameters should be accessed natively. These are
    # NOT estimates (unlike in MALLET with hyperparam. optimization)
    # in x@control for TopicModel
    # in x$settings for stm
}

#' @export
top_words.TopicModel_glue <- function (x, n) {
    tw <- topic_words(x)
    ij <- top_n_row(tw, n)
    dplyr::data_frame_(list(
        topic=~ ij[ , 1],
        word=~ vocabulary(x)[ij[ , 2]],
        weight=~ tw[ij]
    ))
}

#' @export
ParallelTopicModel.TopicModel_glue <- function (x) NULL

#' @export
RTopicModel.TopicModel_glue <- function (x) NULL

#' @export
instances.TopicModel_glue <- function (x) NULL

#' @export
doc_topics.stm_glue <- function (x) x$theta

#' @export
topic_words.stm_glue<- function (x) exp(x$beta[[1]][[1]])

#' @export
vocabulary.stm_glue <- function (x) x$vocab

#' @export
n_topics.stm_glue <- function (x) ncol(x$theta)

#' @export
n_docs.stm_glue <- function (x) nrow(x$theta)

#' @export
doc_ids.stm_glue <- function (x) x$doc_ids
