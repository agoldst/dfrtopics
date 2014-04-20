\name{term_year_series_frame}
\alias{term_year_series_frame}
\title{Convert a term-year matrix to a dataframe for plotting}
\usage{
term_year_series_frame(words, term_year, year_seq, vocab, raw_counts = F,
  total = F, denominator = NULL)
}
\arguments{
  \item{words}{which terms to pick out}

  \item{term_year}{matrix with terms in rows and
  time-slices in columns}

  \item{year_seq}{character vector mapping columns of
  \code{term_year} to dates, as strings in ISO format (the
  \code{yseq} component of results from
  \code{\link{term_year_matrix}} or
  \code{\link{term_year_topic_matrix}}}

  \item{vocab}{character vector mapping rows of
  \code{term_year} to terms}

  \item{raw_counts}{if FALSE (the default), divide counts
  through by column totals or \code{denominator}}

  \item{total}{if TRUE, tally up the total incidences of
  all words in \code{words}}

  \item{denominator}{\code{raw_counts} is FALSE, counts are
  normalized by this. If this is NULL, column totals are
  used. Use this parameter if you are passing in a
  \code{\link{term_year_topic_matrix}} but you still want
  the yearly proportion out of all words in the corpus, in
  which case set \code{denominator=term_year_matrix()$tym}.
  The columns of \code{denominator} are assumed to
  correspond to the same years as those of
  \code{term_year}, so be careful of dropped years.}
}
\value{
a dataframe with three columns, \code{word,year,weight}. If
\code{total} is TRUE, this will have only one row.
}
\description{
This function makes a dataframe, suitable for plotting, of
entries from a term-year matrix (conditional on topic or
not).
}
\seealso{
\code{\link{term_year_topic_matrix}},
\code{\link{term_year_matrix}}
}

