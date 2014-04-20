\name{docs_frame}
\alias{docs_frame}
\title{Convert long-format feature-counts into documents}
\usage{
docs_frame(counts)
}
\arguments{
  \item{counts}{long-format data frame like that returned
  by \code{\link{read_dfr}}}
}
\value{
a dataframe with two columns: \code{id}, the document id;
\code{text}, the full document text (but with the words in
meaningless order)
}
\description{
Naively "inflates" feature counts into a bag of words, for
sending to MALLET.
}
\seealso{
\code{\link{read_dfr_wordcounts}}
}

