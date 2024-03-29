% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/browser_export.R
\name{dfr_browser}
\alias{dfr_browser}
\title{Create and launch a model browser}
\usage{
dfr_browser(m, ...)
}
\arguments{
\item{m}{\code{mallet_model} object from \code{\link{train_model}} or
\code{\link{load_mallet_model}}, or a list of such models, or the results of
\code{\link{align_topics}}; see Details.}

\item{...}{passed on to \code{\link{export_browser_data}}, q.v., especially
the parameters \code{overwrite}, \code{n_scaled_words}, \code{info}, 
\code{proper}, and \code{permute}}

\item{out_dir}{directory for output. By default, files are saved to a
temporary directory.}

\item{browse}{if TRUE, launch web browser after export for viewing (requires
\pkg{servr} package).}

\item{internalize}{always set to FALSE. If TRUE, model data is in the
browser home page rather than separate files, but this behavior is
deprecated. See Details.}

\item{overwrite}{if TRUE (default), overwrite any pre-existing files.}

\item{condition}{dfr-browser displays topic proportions conditioned on (bins
of) a chosen metadata variable (by default, publication date). Any variable
in \code{metadata(m)} may be specified. A vector or list of such variables
may also be supplied (see Details). To adjust the binning of continuous or
time covariates, change the dfr-browser configuration via \code{info} or
\code{sub_info}.}

\item{info}{dfr-browser configuration object (see
\code{\link{export_browser_data}}).}

\item{sub_info}{list of lists of browser configurations (multiple-model
display only). The elements of this list should have names corresponding to
\code{ids}, and each of the elements of its elements should have names
corresponding to \code{condition}. If NULL, default values are used.}

\item{metadata_header}{if TRUE (FALSE is default), the exported metadata CSV
will have a header row (not expected by dfr-browser by default, but see the
example below)}

\item{ids}{character vector. If multiple models are specified in \code{m},
the corresponding element of \code{ids} is used as a model ID in
dfr-browser.}

\item{insert_alignment_table}{if \code{m} is a \code{topic_alignment} object
and this is TRUE, the "About" page of the generated browser will include a
table of the topics in each model with aligned topics in the same row.}
}
\description{
Export model data and all supporting files needed to browse a model or
models interactively using
\href{http://agoldst.github.io/dfr-browser}{dfr-browser}, then (optionally)
open a web browser. It is also possible to browse a list of models.
}
\details{
If \code{browse=T}, the function attempts to start a local webserver and
open a web browser pointing to the appropriate URL. This functionality
requires the \code{servr} package. 

For more control over the export, including the option to export data files
only, if for example you have modified the HTML/CSS/JS of an existing
dfr-browser, use \code{\link{export_browser_data}} (q.v.).
}
\section{Browsing multiple models at once}{


dfr-browser can be configured to retrieve data from more than one model, or
to show topic proportions in a single model conditioned on more than one
metadata variable, or both. \code{dfr_browser} can generate the necessary
data and configuration files. Passing \code{dfr_browser} a list of model
objects will generate a single dfr-browser with a menu for swapping among
the models. Passing a single \code{mallet_model} and a vector of variable
names as \code{condition} will generate a dfr-browser with a menu for
swapping among covariates (internally, dfr-browser treats these two
possibilities the same). Passing both a list of models and a vector of
variable names will generate all the model-covariate pairs. Passing a list
of models and a list of the same length of variable names will use the
corresponding element of \code{condition} as the chosen covariate options
for the given model. Finally, in place of a list of models, you can supply
the result of \code{\link{align_topics}}. In that case the topics in each
model are relabeled according to their assigned cluster number. It is
possible to achieve the same result more manually by passing a list of
models and specifying a \code{topic_ids} vector for each model in the
\code{sub_info} parameter.
}

\section{Deprecated option}{


\code{internalize=TRUE} will cause all model data to be stored in the
index.html file, whence it can be loaded by dfr-browser. This behavior is no
longer useful; the model data should always be exported as separate files
for asynchronous loading by dfr-browser. (In former days one could avoid
having to launch a web server and simply point a browser at the index.html
file. dfr-browser no longer works in this circumstance because it relies on
a Web Worker, which modern web browsers will refuse to load from a
\code{file:///} URL.)
}

\examples{

\dontrun{
m <- model_dfr_documents("citations.CSV", "wordcounts",
    "stoplist.txt", n_topics=40)
# launch browser
dfr_browser(m)

# generate a second model and align its topics with the first for more
# convenient comparisons
m2 <- model_dfr_documents("citations.CSV", "wordcounts",
    "stoplist.txt", n_topics=40)
cl <- model_distances(list(m, m2), n_words=40) \%>\% align_topics()
dfr_browser(m2, permute=match(cl$clusters[[1]], cl$clusters[[2]])))
# or simply:
dfr_browser(cl)

# create a browser that allows you to toggle among 2 metadata variables
dfr_browser(m, condition=c("pubdate", "journaltitle"))

# create a browser for metadata in a non-DfR format; note info specification

dfr_browser(m, condition="something_else",
     metadata_header=T,
     info=list(VIS=list(
         metadata=list(type="base"),
         bib=list(type="base"),
         bib_view=list(major="all", minor="raw")
     )),
)
}

}
\seealso{
\code{\link{export_browser_data}} which does the work of exporting
  files, \code{\link{model_dfr_documents}}, \code{\link{train_model}},
  \code{\link{topic_scaled_2d}}
}
