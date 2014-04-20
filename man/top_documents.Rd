\name{top_documents}
\alias{top_documents}
\title{Get the "top" documents for a topic}
\usage{
top_documents(topic, doc_topics, id_map, n = 5, method = "raw")
}
\arguments{
  \item{topic}{The topic in question}

  \item{doc_topics}{matrix with documents in rows and topic
  weights in columns}

  \item{id_map}{character vector mapping rows of
  \code{doc_topic} to JSTOR doc id's: with the
  \code{doc_topics_frame}, simply use the \code{id} column}

  \item{n}{number of top documents to extract}

  \item{method}{the notion of a "top" document is not
  well-specified. Currently this function knows only two
  simple methods: \describe{ \item{\code{"raw"}}{maximum
  scores in the topic-column of the \code{doc_topic}}
  \item{\code{"max_frac"}}{maximum when document-topic
  scores are normalized to sum to 1. Note that a topic may
  reach its maximum proportion in a document and yet that
  document may yet have a larger proportion of another
  topic.}}}
}
\value{
a data frame with \code{n} rows ordered from weightiest to
least weighty, and two columns: \code{id} and
\code{weight}. The id's can be passed to, e.g.
\code{\link{cite_articles}}.
}
\description{
Constructs a dataframe with the id's and weights with the
"top" documents for a topic.
}
\seealso{
\code{\link{doc_topics_matrix}},
\code{\link{doc_topics_frame}}, \code{\link{cite_articles}}
}

