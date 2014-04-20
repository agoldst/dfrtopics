\name{g2}
\alias{g2}
\title{Dunning's log-likelihood}
\usage{
g2(a, b)
}
\arguments{
  \item{a,}{b vectors where a_i and b_i are taken to be
  frequencies of term i}
}
\value{
a vector whose ith element is the LL score for that term.
}
\description{
Calculate G^2, the log-likelihood statistic, for comparing
two term frequency vectors. Normally used to compare
corpora (i.e. row-sums of term- document matrices. Included
here only for convenience.
}
\seealso{
\code{\link{tf_idf}}
}

