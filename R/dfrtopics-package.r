#' Explore and analyze topic models of JSTOR Data for Research
#'
#' This package supplies functions for creating and analyzing topic models of scholarly 
#' journals using 
#' the word-count files supplied by the \href{http://dfr.jstor.org}{JSTOR Data for 
#' Research} service. It uses \pkg{mallet}, the R wrapper for MALLET, to do the 
#' modeling.
#'
#' An example of basic usage
#'
#' Let's imagine you have downloaded and unzipped the results of a DfR request to 
#' \code{~/dfr/test/}: you have metadata in \code{~/dfr/test/citations.CSV}
#' and wordcounts per document in \code{~/dfr/test/wordcounts/*.CSV}. Then
#' \code{
#' library(dfrtopics)
#' m <- model_documents(citations.file="~/dfr/test/citations.CSV",
#'          dirs="~/dfr/test/wordcounts/",stoplist.file="stoplist/long.txt",
#'          num.topics=40)
#' dtw <- doc_topics_wide(m$doc_topics,m$metadata)
#' topic_yearly_line_plot(dtw=dtw,facet=T)
#' }
#' will estimate a 40-topic model and render a faceted plot of each of the topics 
#' over time.
#'
#' @name dfrtopics
#' @docType package
#'
NULL
