\name{term_yearly_lineplot}
\alias{term_yearly_lineplot}
\title{Plot individual word frequencies over time}
\usage{
term_yearly_lineplot(words, term_year, year_seq, vocab, plot_freq = T,
  plot_total = F, smoothing = F, denominator = NULL)
}
\arguments{
  \item{words}{a vector of words}

  \item{term_year}{term-year matrix, from
  \code{\link{term_year_matrix}}}

  \item{year_seq}{map from column indices of
  \code{term_year} to dates. Also returned in the list from
  \code{\link{term_year_matrix}}}

  \item{vocab}{map from matrix rows to terms, matched
  against \code{words}}

  \item{plot_freq}{if TRUE, plot yearly fractions of the
  total; if FALSE, plot raw counts}

  \item{plot_total}{if TRUE, plot sum of weights of all
  \code{words}}

  \item{smoothing}{if TRUE, add a smoothing line to the
  plot}
}
\description{
The MALLET 1-gram viewer! Use date metadata to show you the
changing frequency of terms over time in the processed
corpus.
}
\details{
This is useful when exploring topic frequencies over time.
For example, compare
\code{\link{topic_yearly_lineplot}(topic,... )} with
\code{\link{term_yearly_lineplot}(\link{topic_top_words}(topic,n=50,...))}
to discover whether the top words of a topic really move up
and down in the same way that a topic does (which may or
may not be significant).
}
\seealso{
\code{\link{term_year_series_frame}}
\code{\link{term_year_matrix}}
}

