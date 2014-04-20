\name{term_document_topic_matrix}
\alias{term_document_topic_matrix}
\title{The term-document matrix for a topic}
\usage{
term_document_topic_matrix(topic, ss, id_map, vocab)
}
\arguments{
  \item{ss}{a \code{big.matrix} holding the "simplified
  state" as returned by
  \code{\link{read_simplified_state}}. Operated on using
  \code{\link[bigmemory]{mwhich}}.}

  \item{id_map}{a character vector mapping document numbers
  in \code{ss} to JSTOR id's}

  \item{vocab}{a character vector mapping word numbers in
  \code{ss} to words as strings}
}
\value{
a \code{\link[Matrix]{sparseMatrix}} with terms in rows
(\code{vocab} order) and documents in columns
(\code{id_map} order). No normalization or smoothing is
applied to word counts.
}
\description{
Extracts a matrix of counts of words assigned to a given
topic in each document from the "simplified" sampling
state.
}
\seealso{
\code{\link{read_simplified_state}},
\code{\link{term_year_matrix}},
\code{\link{term_year_topic_matrix}}
}

