\name{remove_rare}
\alias{remove_rare}
\title{Throw out words whose frequency in the corpus is below a threshold}
\usage{
remove_rare(counts, freq_threshold = NULL, rank_threshold = NULL,
  .overall = NULL)
}
\arguments{
  \item{counts}{long-form dataframe as returned by
  \code{\link{read_dfr}}}

  \item{freq_threshold}{frequency threshold (NULL, or a
  number between 0 and 1)}

  \item{rank_threshold}{rank threshold (natural number):
  used if \code{freq_threshold} is NULL}

  \item{.overall}{precalculated overall counts (if NULL,
  \code{\link{overall_counts}} is invoked)}
}
\value{
A filtered feature-counts dataframe
}
\description{
For filtering wordcounts before building MALLET instances.
}

