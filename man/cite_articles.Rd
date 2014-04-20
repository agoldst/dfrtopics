\name{cite_articles}
\alias{cite_articles}
\title{Generate simple citation strings from metadata}
\usage{
cite_articles(metadata, ids = NULL)
}
\arguments{
  \item{metadata}{data frame (from e.g.
  \code{\link{read_metadata}})}

  \item{ids}{character vector of document id's to generate
  citations for. If NULL, generate citations for all rows
  of \code{metadata}}
}
\value{
a character vector of citations
}
\description{
Given a dataframe with metadata and (optionally) document
id's, return a character vector of citations.
}
\details{
The generated citations are meant for quick reference, not
formal use. They do not handle quotations with quotations
correctly and make no effort to scrub the cruft found in
some journals' metadata on JSTOR. Authors are simply
concatenated into a long "A and B and C" list.
}

