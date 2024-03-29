% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inferencer.R
\name{merge.mallet_model}
\alias{merge.mallet_model}
\title{Combine a topic model with inferred document topics}
\usage{
\method{merge}{mallet_model}(x, y, weighting_dtx = identity, weighting_dty = identity)
}
\arguments{
\item{x, }{y models with the same topic-word matrices and vocabulary}
}
\value{
a further pseudo-model, inheriting the topic-word matrix of the
  arguments, and with a document-topic matrix formed by stacking the
  corresponding matrices of the arguments. Note that merging an inferred
  topic model means that the document-topic matrix must be row-normalized.
}
\description{
This function joins together two models that share enough parameters for this
operation to make some sense, for example, an ordinary topic model and a
model of further documents created by \code{\link{infer_topics}}.
}
