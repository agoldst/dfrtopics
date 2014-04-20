\name{doc_topics_wide}
\alias{doc_topics_wide}
\title{Get a wide document-topic-metadata frame}
\usage{
doc_topics_wide(doctops, metadata, meta_keep = "pubdate")
}
\arguments{
  \item{doctops}{frame with document-topic weights and an
  id column}

  \item{metadata}{frame}

  \item{meta_keep}{vector of names of columns of metadata
  to keep}
}
\value{
a data frame with an id column, one column for each element
of \code{meta_keep}, and \code{topic<n>} columns with topic
weights
}
\description{
Synthesizes a \code{\link{doc_topics_frame}} with metadata
into a "wide" format
}
\seealso{
\code{\link[reshape2]{melt}},
\code{\link{doc_topics_frame}},
\code{\link{doc_topics_long}}, \code{\link{read_metadata}}
}

