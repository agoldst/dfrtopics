#' Explore and analyze topic models
#' 
#' This package supplies functions for creating and analyzing topic models using
#' \href{http://mallet.cs.umass.edu}{MALLET}. It has numerous functions to help
#' processing data from one particular ssource, JSTOR's
#' \href{http://dfr.jstor.org}{JSTOR Data for Research} service, but the
#' modeling and exploration functions are independent of that source. It uses
#' \code{\link[mallet]{mallet}}, the R wrapper for MALLET, to control the
#' modeling process.
#' 
#' A detailed walkthrough of the package can be found in the "Introduction"
#' vignette.
#' 
#' A companion project, \href{http://agoldst.github.io/dfr-browser}{dfr-browser},
#' uses JavaScript to create an interactive model browser in a web browser. To 
#' save the model outputs produced in this package into files formatted as 
#' \code{dfr-browser} requires, use \code{\link{export_browser_data}}.
#' 
#' 
#' @examples
#' \dontrun{
#' # Please see vignette("introduction", "dfrtopics") for more examples
#' # But to play with a first model:
#' # Put a DfR download in "dfr-data" and run:
#' m <- model_dfr_documents(
#'     citations_file="dfr-data/citations.tsv",
#'     wordcounts_dirs="dfr-data/wordcounts",
#'     n_topics=20  # or any other initial topic guess
#' )
#' topic_labels(m)
#' plot_series(topic_series(m))
#' }
#' 
#' @name dfrtopics
#' @import Matrix
#' @docType package
#'   
NULL


#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# Package loading: check the Java heap allocation
.onAttach <- function (libname, pkgname) {
    jheap <- grep("-Xmx\\w+", options("java.parameters"), value=TRUE)
    heap_ok <- TRUE

    if (length(jheap) == 0) {
        # shouldn't get here since rJava loading should set java.parameters
        # with a default value of "-Xmx512m"
        message(
            "You are using rJava's default Java heap setting (512MB).")
        heap_ok <- FALSE
    } else {
        size_str <- substring(jheap, first=5)
        size_num <- gsub("\\D", "", size_str)
        size_unit <- switch(gsub("\\d", "", size_str), 
                            k=2^10, K=2^10, 
                            m=2^20, M=2^20, 
                            g=2^30, G=2^30, 
                            t=2^40, T=2^40)
        jheap_bytes <- as.numeric(size_num) * size_unit

        if(is.na(jheap_bytes) | jheap_bytes < 2^31) {
            message("Your current Java heap setting is ", size_str, ".")
            heap_ok <- FALSE
        }
    }

    if (!heap_ok) {
        message(
'I recommend giving Java at least 2GB of heap space. To do this, put the
following command in your scripts *before* loading this package:

    options(java.parameters="-Xmx2g")

If you change this option in this session, you must then detach and
reload this package, mallet, and rJava. You can also simply restart R,
set the option, and then load this package. I apologize for this design
flaw in R and rJava.' 
        )
    }
}
