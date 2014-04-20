\name{top_topics}
\alias{top_topics}
\title{Get top topics for a document}
\usage{
top_topics(id, doc_topics, id_map, n = 5)
}
\arguments{
  \item{id}{a JSTOR document ID, matched against
  \code{id_map}}

  \item{doc_topics}{a matrix with documents in rows and
  topic scores in columns}

  \item{id_map}{mapping from rows of \code{doc_topics} to
  id's}

  \item{n}{number of topics to extract}
}
\value{
a dataframe with \code{n} rows and two columns,
\code{topic} and \code{weight}.
}
\description{
This function extracts the most salient topics for a
document from a document-topic matrix.
}
\details{
The result means different things, depending on whether the
matrix is normalized per topic. If not, one gets the topics
that have been assigned the largest number of words in a
document. But if of the matrix is column-normalized, then
one gets the topics for which the document is comparatively
most prominent within that topic.
}
\seealso{
\code{\link{doc_topics_matrix}},
\code{\link{doc_topics_frame}}
}

