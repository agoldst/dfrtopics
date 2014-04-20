\name{overall_counts}
\alias{overall_counts}
\title{Calculate total corpus-wide feature counts}
\usage{
overall_counts(counts)
}
\arguments{
  \item{counts}{The dataframe from \code{\link{read_dfr}}}
}
\value{
a 1D table, i.e. a vector of counts with word types as the
element names
}
\description{
Given a wordcounts long-format dataframe returned by
\code{\link{read_dfr}}, calculate total corpus-wide counts
for each word type
}
\seealso{
\code{\link{read_dfr}},
\code{\link{instances_term_document_matrix}} for feature
counts \emph{after} stopword removal (etc.).
}

