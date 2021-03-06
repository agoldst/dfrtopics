% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{dfr_filename_id}
\alias{dfr_filename_id}
\title{Convert wordcount filenames to JSTOR document id's}
\usage{
dfr_filename_id(filename)
}
\arguments{
\item{filename}{a character vector of filenames}
}
\value{
id a character vector of document id's
}
\description{
Convenience function for turning a file path like 
\code{path/to/wordcounts_10.2307_3175328.CSV} into an id like 
\code{10.2307/3175328}.
}
\details{
The file extension can be anything alphabetic.
}
\examples{
dfr_filename_id("path/to/wordcounts_10.2307_3175328.CSV")
}
\seealso{
\code{\link{id_dfr_filename}}
}
