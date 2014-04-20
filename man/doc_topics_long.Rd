\name{doc_topics_long}
\alias{doc_topics_long}
\title{Get a melted document-topic-metadata frame}
\usage{
doc_topics_long(doctops, metadata, meta_keep = c("pubdate", "journaltitle"))
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
of \code{meta_keep}, a \code{variable} column with
\code{topic<n>} labeling the topic, and a \code{value}
column with the topic score
}
\description{
Synthesizes a \code{\link{doc_topics_frame}} with metadata
into a "long" format
}
\seealso{
\code{\link[reshape2]{melt}},
\code{\link{doc_topics_frame}},
\code{\link{doc_topics_wide}}, \code{\link{read_metadata}}
}

