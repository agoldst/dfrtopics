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
#' @useDynLib dfrtopics
#' @importFrom Rcpp sourceCpp
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

dfrtopics_flags <- new.env(parent=emptyenv())
dfrtopics_flags$mallet_loaded <- FALSE

load_mallet <- function () {
    if (dfrtopics_flags$mallet_loaded) {
        return()
    }

    javap <- options("java.parameters")[[1]]
    jheap <- grep("-Xmx\\w+", javap, value=TRUE)

    if (length(jheap) == 0) {
        message(
"Using a Java heap allocation of 2GB, which I recommend over the rJava
default of 512MB. To change this allocation, put the following command in your
scripts *before* loading this package:

    options(java.parameters=\"-Xmx4g\")

If you change this option in this session, you must then detach and
reload this package, mallet, and rJava. You can also simply restart R,
set the option, and then load this package. I apologize for this design
flaw in R and rJava."
        )

        options(java.parameters="-Xmx2g")
        on.exit(options(java.parameters=javap), add=TRUE)
    }

    if (requireNamespace("mallet", quietly=TRUE)) {
        dfrtopics_flags$mallet_loaded <- TRUE
    } else {
        stop(
"Unable to load mallet and rJava. Ensure mallet is installed with
install.packages(\"mallet\"). If you still have problems, you may have to
adjust environment variables."
        )
    }
}

# Package unload
.onUnload <- function (libpath) {
    library.dynam.unload("dfrtopics", libpath)
}
