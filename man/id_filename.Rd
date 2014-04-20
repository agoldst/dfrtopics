\name{id_filename}
\alias{id_filename}
\title{Convert JSTOR document id's to \code{wordcounts*.CSV} filenames}
\usage{
id_filename(id)
}
\arguments{
  \item{id}{a character vector of document id's}
}
\value{
a character vector of filenames
}
\description{
Convenience function for turning an ID like
\code{10.2307/3175328} into a DfR wordcount filename like
\code{wordcounts_10.2307_3175328.CSV}.
}
\examples{
id_filename("10.2307/3175328")
}
\seealso{
\code{\link{filename_id}}
}

