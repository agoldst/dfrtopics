\name{read_topic_words}
\alias{read_topic_words}
\title{Read in the topic-word matrix}
\usage{
read_topic_words(tw_file, what = integer())
}
\arguments{
  \item{tw_file}{CSV filename, for example
  \code{topic_words.csv}.}

  \item{what}{datatype to read in (passed on to
  \code{\link[base]{scan}}). \code{\link[base]{integer}()}
  by default; use \code{\link[base]{numeric}()} if the
  datafile has proportions.}
}
\value{
A \code{\link[Matrix]{sparseMatrix}} with topics in rows
and columns in order of the vocabulary as known to the
mallet instances)
}
\description{
Get the matrix with topics in rows and word counts in
columns. Returns a \code{\link[Matrix]{sparseMatrix}}.
}
\details{
Formerly known as \code{read_topic_words_matrix}.
}
\seealso{
\code{\link{write_topic_words}},
\code{\link[mallet]{mallet.topic.words}} for online access
to the same matrix.
}

