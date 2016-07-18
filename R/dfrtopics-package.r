#' Explore and analyze topic models
#'
#' This package supplies functions for creating and analyzing topic models using
#' \href{http://mallet.cs.umass.edu}{MALLET}. It has numerous functions to help
#' process data from one particular source, JSTOR's
#' \href{http://dfr.jstor.org}{Data for Research} service, but the
#' modeling and exploration functions are independent of that source. It uses
#' \code{\link[mallet]{mallet}}, the R wrapper for MALLET, for modeling.
#' A detailed walkthrough of the package can be found in the "Introduction"
#' vignette.
#' A companion project,
#' \href{http://agoldst.github.io/dfr-browser}{dfr-browser},
#' uses JavaScript to create an interactive model browser in a web browser.
#' To explore a model with dfr-browser, use the \code{\link{dfr_browser}}
#' function. To save the model outputs produced in this package into files
#' formatted as \code{dfr-browser} requires, use
#' \code{\link{export_browser_data}}.
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
#' dfr_browser(m)
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

    javap <- getOption("java.parameters")
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

    # Process mallet logging options by choosing a config file.  This is pretty
    # lame, but I can't seem to get mallet logging to respect settings I give
    # with J("java.lang.System")$setProperty after the JVM is launched. So our
    # only chance is to preempt the default configuration at initialization.
    logging <- getOption("dfrtopics.mallet_logging")
    if (identical(logging, "default") || identical(logging, "console")) {
        logconfig <- NULL
    } else if (identical(logging, "file")) {
        logconfig <- system.file("javaconfig",
            "logging-file.properties", package="dfrtopics")
    } else if (identical(sort(logging), c("console", "file"))) {
        logconfig <- system.file("javaconfig",
            "logging-console-file.properties", package="dfrtopics")
    } else if (identical(logging, "none") || identical(logging, "off")) {
        logconfig <- system.file("javaconfig",
            "logging-none.properties", package="dfrtopics")
    } else if (file.exists(logging))  {
        # custom file
        logconfig <- normalizePath(logging)
    } else {
        logconfig <- NULL
    }

    # now set the logging config file option
    if (!is.null(logconfig)) {
        options(java.parameters=c(
            getOption("java.parameters"),
            paste0("-Djava.util.logging.config.file=", logconfig)
        ))
        on.exit(options(java.parameters=javap), add=TRUE)
    }

    if (requireNamespace("mallet", quietly=TRUE)) {
        if (packageVersion("mallet") < "1.1") {
            # Old R mallet needs to find rJava on the search path
            # We're not supposed to use require in a package
            # but this is what we need to be compatible
            # with both CRAN 1.0 and github 1.1+ versions of mallet
            dfrtopics_flags$mallet_loaded <- require("rJava", quietly=TRUE)
        } else {
            dfrtopics_flags$mallet_loaded <- TRUE
        }
    }

    if (!dfrtopics_flags$mallet_loaded) {
        stop(
"Unable to load mallet and rJava. Ensure mallet is installed,
either from CRAN with
install.packages(\"mallet\")
or the development version
devtools::install_github(\"mimno/RMallet\", subdir=\"mallet\")
If you still have problems, you may have to adjust environment variables
to get Java working."
        )
    }
}

#' MALLET logging options
#'
#' By default, MALLET provides a quite verbose continual report on the progress
#' of the modeling process. This is normally useful, but sometimes you might
#' prefer for the log to be saved to disk or suppressed altogether.
#' Unfortunately the interaction between R and MALLET makes this difficult to
#' configure: console output from rJava cannot be easily captured or
#' redirected. But MALLET has built-in logging configuration options. The
#' package option \code{dfrtopics.mallet_logging} can be set to change MALLET's
#' behavior in this regard. The option must be set \emph{before} any MALLET
#' functions are invoked (e.g. via \code{\link{make_instances}} or
#' \code{\link{train_model}}). The option has the following possible settings:
#' \describe{
#' \item{\code{"default"} or \code{"console"}}{
#' The MALLET default (verbose console output).
#' }\item{\code{"file"}}{
#' Output \emph{appended} to a file named \code{mallet0.log} in the working
#' directory (this file is created if necessary). No limit is placed on the
#' size of the log file, which might therefore grow very large.
#' }\item{\code{"none"} or \code{"off"}}{
#' Logging suppressed.
#' }\item{\code{c("console", "file")}}{
#' Both file and console logging.
#' }\item{A file path}{
#' The path is normalized and given to Java as the value of the system property
#' \code{java.util.logging.config.file}. The file should be a Java Properties
#' file that adjusts Java's logging settings. For examples, see
#' \code{list.files(system.file("javaconfig", package="dfrtopics"))}, or
#' extract the mallet default \code{logging.properties} to the working
#' directory as follows: \code{unzip(system.file("java", "mallet.jar",
#' package="mallet"), "cc/mallet/util/resources/logging.properties",
#' junkpaths=T)}.
#' }}
#'
#' @name mallet-logging
NULL

# package load
.onLoad <- function (libname, pkgname) {
    op <- options()
    op_ours <- list(
        dfrtopics.browser_info=list(
title="Model Browser",
meta_info=paste0(
    "<p>Topic model browser generated for ",
    Sys.getenv("USER"), "</p>"
),
VIS=list(
    condition=list(
        type="time",
        spec=list(
            field="date",
            "unit"="year",
            n=1
        )
    )
)
        ),
        dfrtopics.mallet_logging="default",
        dfrtopics.state_chunk_size=10000L
    )

    to_set <- !(names(op_ours) %in% names(op))
    if (any(to_set)) options(op_ours[to_set])
    invisible()
}


# Package unload
.onUnload <- function (libpath) {
    library.dynam.unload("dfrtopics", libpath)
}
