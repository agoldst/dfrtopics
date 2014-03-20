#' Explore and analyze topic models of JSTOR Data for Research
#'
#' This package supplies functions for creating and analyzing topic
#' models of scholarly journals using the word-count files supplied by
#' the \href{http://dfr.jstor.org}{JSTOR Data for Research} service. It
#' uses \code{\link[mallet]{mallet}}, the R wrapper for MALLET, to do
#' the modeling.
#'
#' @examples
#'
#' \dontrun{
#' # Say you've unzipped the results of a DfR request to ~/dfr/test/:
#' # metadata in ~/dfr/test/citations.CSV
#' # wordcounts per document in ~/dfr/test/wordcounts/*.CSV
#' library(dfrtopics)
#' # Run the model (40 topics)
#' m <- model_documents(citations_files="~/dfr/test/citations.CSV",
#'          dirs="~/dfr/test/wordcounts/",stoplist_file="stoplist/long.txt",
#'          num_topics=40)
#' # Get doc-topic matrix joined with metadata
#' dtw <- doc_topics_wide(m$doc_topics,m$metadata)
#' # Convert that into a data frame of topic yearly time series
#' series <- topic_proportions_series_frame(topic_year_matrix(dtw))
#' # Make a faceted plot of topics over time
#' topic_yearly_lineplot(series,facet=T)
#' }
#'
#'
#'
#' @section Metadata processing:
#'
#' The core function is \code{\link{read_metadata}} for reading and
#' combining \code{citations.CSV} files. Of some use is the simple
#' \code{\link{pubdate_Date}}, which converts the \code{pubdate}
#' field to \code{\link[base]{Date}} objects. For browsing,
#' \code{\link{cite_articles}} turns rows of metadata into a
#' human-readable citation.
#'
#' @section Making topic models:
#'
#' To produce MALLET-ready data from DfR files, use
#' \code{\link{read_dfr_wordcounts}} to get a dataframe and pass
#' that to \code{\link{make_instances}}. The resulting instances
#' can be saved to disk with \code{\link{write_instances}}. The
#' package provides a number of functions (with names of form
#' \code{instances_*}) for extracting information from MALLET
#' instances, including a function for extracting a term-document
#' matrix: \code{\link{instances_term_document_matrix}}.
#'
#' The models themselves are created by running MALLET from R using the
#' \code{\link{train_model}} function. After MALLET has run, access the
#' model information with functions like \code{\link{doc_topics_frame}}
#' and \code{\link{weighted_keys_frame}}, which are thin wrappers
#' around similar functions provided in the \code{\link[mallet]{mallet}}
#' package itself. For sustained analysis, it makes more sense to
#' write results to disk: the package provides a convenience function
#' for extracting and saving a lot of model information all at once:
#' \code{\link{output_model}}.
#'
#' The fullest information comes from the final Gibbs sampling
#' state of the topic model, but this requires some postprocessing
#' before it can be handled in R. Use \code{\link{simplify_state}}
#' and then \code{\link{read_simplified_state}} for this
#' task, and then analyze the results with functions
#' like \code{\link{term_document_topic_matrix}} and
#' \code{\link{term_year_topic_matrix}}.
#'
#'
#' @section Topics and time:
#'
#' Though the LDA algorithm run by MALLET here makes no use of the
#' time metadata, it is often instructive to see how the modeled
#' topics are spread over time in a corpus of JSTOR articles. Thus I
#' provide a number of functions for simplifying access to these time
#' series, of which the key two are \code{\link{topic_year_matrix}} and
#' \code{\link{topic_proportions_series_frame}}.
#'
#' @section Visualizations:
#'
#' \code{\link{topic_report}} generates a folder of images (multiple
#' plots each) giving an overview of each topic in the model.
#'
#' \code{\link{topic_keyword_plot}} gives a way of looking at "top key
#' words" for a topic that preserves information about their weights.
#'
#' Most of the visualization functions focus on time series.
#' The basic "topic over time" plot functions are convenient
#' ways of plotting \code{\link{topic_proportions_series_frame}}
#' results: \code{\link{topic_yearly_lineplot}} and
#' \code{\link{topic_yearly_barplot}} provide two simple possibilities,
#' and \code{\link{topic_time_boxplots}} tries to give more information.
#'
#' You can obtain a simple plot of topic-topic relations with
#' \code{\link{topic_dist_plot}}.
#'
#'
#' There are also basic visualizations of the words in the
#' corpus, which need various slices of the term-document matrix
#' and the term-document-topic array: \code{\link{term_yearly_lineplot}} and
#' \code{\link{term_yearly_topic_lineplot}}.
#'
#' @section Interactive visualization:
#' 
#' A companion project, \url{http://agoldst.github.io/dfr-browser}{dfr-browser},
#' uses JavaScript to create an interactive model browser in a web browser. To
#' save the model outputs produced in this package into files formatted as 
#' \code{dfr-browser} requires, use \code{\link{export_browser_data}}.
#'
#' @name dfrtopics
#' @docType package
#'
NULL

.onAttach <- function(libname,pkgname) {
    jheap <- grep("-Xmx\\w+",options("java.parameters"),value=T)
    heap_ok <- T

    if(length(jheap) == 0) {
        packageStartupMessage(
            "You are using the default Java heap setting (512 MB).")
        heap_ok <- F
    } else {
        size <- substring(jheap,first=5)
        if(!grepl("([2-9]|\\d\\d\\d*)(g|G)",size)) {
            packageStartupMessage("Your current Java heap setting is ",
                                  size,".")
            heap_ok <- F
        }
    }

    if(!heap_ok) {
        packageStartupMessage(
'I recommend giving Java at least 2GB of heap space. To do this, put the
following command in your scripts *before* loading this package:

    options(java.parameters="-Xmx2g")

If you change this option in this session, you must then detach and
reload this package, mallet, and rJava. You can also simply restart
R and then set the option. I apologize for this design flaw in R and
rJava.'
        )
    }
}
