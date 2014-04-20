\name{read_metadata}
\alias{read_metadata}
\title{Make a dataframe from \code{citations.CSV} files}
\usage{
read_metadata(filenames, ...)
}
\arguments{
  \item{filenames}{vector of \code{citations.CSV}
  filenames}

  \item{...}{passed on to \code{\link{read.csv}}; the most
  useful may be \code{strip.white}, which determines
  whether \code{\link{read.csv}} will strip leading and
  trailing whitespace from each field.}
}
\value{
A dataframe with deduplicated metadata. The function issues
a warning if there are lines that have the same id field
but are not identical.
}
\description{
Reads in metadata from JSTOR's metadata files and returns a
combined dataframe.
}
\details{
This function assumes that each file has a trailing comma
at the end of every line, so we expect
\code{\link{read.csv}} to find an extra dummy field. DfR
has changed their output data format before, so check
results carefully.

NB. JSTOR puts a trailing tab at the end of each entry in
fields that can contain multiple tab-separated entries
(e.g. author)---even if there is only a single entry.
}
\seealso{
\code{\link{pubdate_Date}}
}

