#' Convert word counts data frame to Structural Topic Model inputs
#'
#' This is a helper function for transforming word-counts-type data into the
#' form expected by \code{\link[stm]{stm}} from the \pkg{stm} package. Given a
#' data frame from \code{\link{read_wordcounts}} (and optionally a frame of
#' metadata), it yields a list whose components can be passed as arguments to
#' \code{stm}. See below for example usage.
#'
#' Some memory-management might be a good idea here. Once you have the list of
#' inputs from this function, you no longer need the \code{counts} object.
#'
#' @param counts data frame as returned from \code{\link{read_wordcounts}}
#' @param meta optional metadata data frame (with an \code{id} column to be
#'   matched against \code{counts$id}).
#'
#' @return a list with elements \code{documents}, \code{vocab}, \code{data} (if
#'   \code{meta} has been supplied). These elements are suitable to be passed as
#'   the parameters of those names to \code{\link[stm]{stm}}.
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
#' @seealso \code{\link{foreign_model}} for using this package's
#'   model-exploration utilities in conjunction with an STM.
#'
#' @export
#'
wordcounts_stm_inputs <- function (counts, meta=NULL) {
    if (!requireNamespace("stm", quietly=TRUE)) {
        stop("Please install stm from CRAN first.")
    }
    dtm <- wordcounts_Matrix(counts)
    result <- list(
        documents=stm::readCorpus(dtm, type="Matrix")$documents,
        vocab=colnames(dtm)
    )
    if (!is.null(meta)) {
        result$data <- as.data.frame(meta[match(rownames(dtm), meta$id), ])
    }
    result
}

#' Convert word counts data frame to a DocumentTermMatrix
#'
#' The \pkg{topicmodels} package expects corpora in the form of a
#' \code{\link[tm]{DocumentTermMatrix}} (defined by the \pkg{tm} package). This
#' function converts a data frame of word counts to this form.
#'
#' @seealso \code{\link{foreign_model}} for using this package's other functions
#'   on a model from \pkg{topicmodels}, \code{\link{wordcounts_Matrix}} which
#'   underlies this function.
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
#' results from \pkg{topicmodels}' \code{\link[topicmodels]{LDA}} and
#' \code{\link[topicmodels]{CTM}} functions. I have also wished to make it
#' possible to interface with the \pkg{stm} package and its Structural Topic
#' Model (\code{\link[stm]{stm}}). Given a model from one of these two packages,
#' apply \code{foreign_model} to obtain an object that can be used with (some
#' of) the functions in \pkg{dfrtopics}. Use \code{unwrap} to get back the
#' original model object.
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
#' Not all functionality is supported. Anything that requires MALLET's
#' assignments of topics to individual words (the "sampling state") does not at
#' present work. Note too that \code{doc_topics} and \code{topic_words} applied
#' to a \code{TopicModel} or an \code{stm} return \emph{parameter estimates} of
#' the probabilities of topics in documents or words in topics. In MALLET
#' terminology these are "smoothed and normalized," not raw sampling weights.
#' For this reason \code{\link{hyperparameters}} does \emph{not} return true
#' hyperparameter values for these models---which are, in any case, defined
#' variously for the various estimation procedures. Instead,
#' \code{hyperparameters} returns dummy values of zero so that
#' \code{\link{tw_smooth_normalize}} and \code{\link{dt_smooth_normalize}} will
#' not incorrectly add anything to the posteriors. The actual hyperparameters
#' should be retrieved from the underlying model if needed.
#'
#' \code{\link{align_topics}} will work with glue objects and should help
#' compare variant models and estimation strategies.
#'
#' It is possible to apply \code{\link{dfr_browser}} to a glue object to
#' explore a model, with two caveats. First, the implication of using the
#' normalized posteriors is that all documents are given equal weight in the
#' display, whereas the display of a model from mallet by default weights
#' documents by their lengths; for a more comparable display of a mallet model
#' \code{m}, use \code{dfr_browser(m, proper=T)}. Second, at present the
#' display of an \code{stm} object will not use any explicit estimates of the
#' effects of time covariates. It just takes the average estimated topic
#' proportion of all documents in each year. To examine the actual estimates,
#' together with uncertainties, the \code{\link[stm]{estimateEffect}} method
#' should be used, or the interactive visualization provided by the
#' \pkg{stmBrowser} package, for which the kludges here are no substitute.
#'
#' @param x model for translation from \pkg{topicmodels} or \pkg{stm}
#'
#' @param metadata metadata frame to attach to model. For converting from
#'   \code{stm}, supply the same metadata as was given to \code{stm}. Conversion
#'   from \code{LDA} can use a superset of the document metadata, provided the
#'   rownames of the modeled \code{DocumentTermMatrix} can be matched against
#'   \code{metadata$id}.
#'
#' @return A wrapper object which will work with most functions of an object of
#'   class \code{mallet_model}.
#'
#' @seealso \code{\link{wordcounts_DocumentTermMatrix}} and
#'   \code{\link{wordcounts_stm_inputs}} to prepare wordcount data for input to
#'   these other packages' modeling procedures.
#'
#' @examples
#'
#' \dontrun{
#' # aligning three models from three packages
#'
#' counts <- read_wordcounts(...) # etc.
#' meta <- read_dfr_metadata(...) # etc.
#'
#' library(stm)
#' corpus <- wordcounts_stm_inputs(counts, meta)
#' m_stm <- stm(documents=corpus$documents,
#'     vocab=corpus$vocab,
#'     data=corpus$data,
#'     K=25, prevalence= ~ s(journaltitle))
#' m_stm_glue <- foreign_model(m_stm, corp$data)
#'
#' library(topicmodels)
#' dtm <- wordcounts_DocumentTermMatrix(counts)
#' m_lda <- LDA(dtm,
#'     k=25, control=list(alpha=0.1))
#' m_lda_glue <- foreign_model(m_lda, meta)
#'
#' insts <- wordcounts_instances(counts)
#' m_mallet <- train_model(insts, n_topics=25,
#'     metadata=meta)
#'
#' model_distances(list(m_stm_glue, m_lda_glue, m_mallet), 100) %>%
#' align_topics() %>%
#' alignment_frame()
#' }
#'
#' @export
foreign_model <- function (x, metadata=NULL) {
    if (is(x, "TopicModel")) {
        result <- structure(list(m=x),
            class=c("TopicModel_glue", "mallet_model"))
    } else if (inherits(x, "STM")) {
        result <- structure(list(m=x),
            class=c("stm_glue", "TopicModel_glue", "mallet_model"))

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

#' @rdname foreign_model
#' @export
unwrap <- function (x) UseMethod("unwrap")

#' @export
unwrap.TopicModel_glue <- function (x) x$m

#' @export
print.TopicModel_glue <- function (x, ...) {
    cat("A dfrtopics wrapper around a topic model from another package:\n")
    print(x$m, ...)
}

#' @export
summary.TopicModel_glue <- function (x, ...) {
    structure(list(s=summary(x$m, ...),
                   c=class(x$m)),
              class="TopicModel_glue_summary")
}

#' @export
summary.stm_glue <- function (x, ...) {
    # summary.STM `cat`s some of its output, so we capture it
    structure(list(s=capture.output(print(summary(x$m, ...))),
                   c=class(x$m)),
              class="TopicModel_glue_summary")
}

#' @export
print.TopicModel_glue_summary <- function (x) {
    cat("A dfrtopics wrapper around a topic model from another package:\n")
    cat("Source type:", x$c, "\n")
    print(x$s)
}

#' @export
doc_topics.TopicModel_glue <- function (x)
    topicmodels::posterior(x$m)$topics

#' @export
topic_words.TopicModel_glue<- function (x)
    topicmodels::posterior(x$m)$terms

#' @export
vocabulary.TopicModel_glue <- function (x) x$m@terms

#' @export
n_topics.TopicModel_glue <- function (x) x$m@k

#' @export
n_docs.TopicModel_glue <- function (x) nrow(x$m@gamma)

#' @export
doc_ids.TopicModel_glue <- function (x) x$m@documents

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
ParallelTopicModel.TopicModel_glue <- function (x) NULL

#' @export
RTopicModel.TopicModel_glue <- function (x) NULL

#' @export
instances.TopicModel_glue <- function (x) NULL

#' @export
doc_topics.stm_glue <- function (x) x$m$theta

#' @export
topic_words.stm_glue<- function (x) exp(x$m$beta[[1]][[1]])

#' @export
vocabulary.stm_glue <- function (x) x$m$vocab

#' @export
n_topics.stm_glue <- function (x) ncol(x$m$theta)

#' @export
n_docs.stm_glue <- function (x) nrow(x$m$theta)

#' @export
doc_ids.stm_glue <- function (x) x$doc_ids
