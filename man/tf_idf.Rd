\name{tf_idf}
\alias{tf_idf}
\title{Calculate tf*idf scores}
\usage{
tf_idf(term, doc, tdm)
}
\arguments{
  \item{term}{numeric index into rows of \code{tdm} (can be
  a vector)}

  \item{doc}{numeric index into columns of \code{tdm} (can
  be a vector)}

  \item{tdm}{term-document
  \code{\link[Matrix]{sparseMatrix}}}
}
\description{
Calculates tf*idf scores from a term-document
\code{\link[Matrix]{sparseMatrix}}.
}
\details{
May not be optimal for speed for calculating scores for the
whole tdm.
}
\seealso{
\code{\link{instances_term_document_matrix}}
}

