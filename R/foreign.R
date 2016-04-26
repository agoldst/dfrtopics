#' Convert word counts data frame to Structural Topic Model inputs
#'
#' This is a helper function for transforming word-counts-type data into the
#' form expected by \code{\link[stm]{stm}} from the \pkg{stm} package. Given a
#' data frame from \code{\link[dfrtopics]{read_wordcounts}} (and optionally a
#' frame of metadata), it yields a list whose components can be passed as
#' arguments to \code{stm}. See below for example usage.
#'
#' At present, dfrtopics functions for exploring topic models cannot be applied
#' to \code{stm} objects. That may change in future, but \pkg{stm} supplies a
#' rich array of exploratory and inferential functions itself.
#'
#' Some memory-management might be a good idea here. Once you have the list of
#' inputs from this function, you no longer need the \code{counts} object.
#'
#' @param counts data frame as returned from
#'   \code{\link[dfrtopics{read_wordcounts}}
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

