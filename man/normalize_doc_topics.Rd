\name{normalize_doc_topics}
\alias{normalize_doc_topics}
\title{Normalize columns to sum to one}
\usage{
normalize_doc_topics(dtm)
}
\arguments{
  \item{dtm}{the doc_topics matrix (or any other matrix
  with weights in columns)}
}
\value{
a matrix
}
\description{
Normalize columns to sum to one
}
\details{
A convenience function for expressing the process of
normalizing the doc-topic matrix. Actually valid for any
matrix in which no column is entirely zero.
}

