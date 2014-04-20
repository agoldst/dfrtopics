\name{topic_yearly_top_words}
\alias{topic_yearly_top_words}
\title{Get top words for a given topic, conditioned by year}
\usage{
topic_yearly_top_words(tytm, yseq, vocab, n_words = 5)
}
\arguments{
  \item{tytm}{matrix with words in rows and years in
  columns (assumed conditional on a topic, though this
  doesn't figure into the calculation)}

  \item{yseq}{character vector mapping columns of
  \code{tytm} to dates}

  \item{vocab}{character vector mapping rows of \code{tytm}
  to words}

  \item{n_words}{number of top words per date to report}
}
\value{
a vector of pasted-together words, with the dates as
element names
}
\description{
This is a convenience function for summarizing the results
of \code{\link{term_year_topic_matrix}}.
}
\seealso{
\code{\link{term_year_topic_matrix}},
\code{\link{weighted_keys_frame}}
}

