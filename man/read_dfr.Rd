\name{read_dfr}
\alias{read_dfr}
\title{Convert DfR wordcount files to a long-format data frame}
\usage{
read_dfr(dirs = NULL, files = NULL, report_interval = 100)
}
\arguments{
  \item{dirs}{character vector of directories containing
  \code{wordcounts*.CSV} files}

  \item{files}{individual filenames to read.}
}
\value{
A dataframe with three columns: \code{id}, the document ID;
\code{WORDCOUNTS}, a feature counted by JSTOR (i.e. a word
type); \code{WEIGHT}, the count.
}
\description{
Reads in a bunch of \code{wordcounts*.CSV} files and stacks
them up in a single "long format" dataframe. Invoked by
\code{\link{read_dfr_wordcounts}}.
}
\details{
Empty documents are skipped; DfR supplies wordcounts files
for documents that have no wordcount data. These will be in
DfR's metadata but not in the output dataframe here.

This is slow. An outboard script in python or Perl is
faster, but this keeps us in R and does everything in
memory. Memory usage: for N typical journal articles, the
resulting dataframe needs about 20N K of memory. So R will
hit its limits somewhere around 100K articles of typical
length.
}
\seealso{
\code{\link{read_dfr_wordcounts}},
\code{\link{instances_term_document_matrix}} for feature
counts \emph{after} stopword removal (etc.).
}

