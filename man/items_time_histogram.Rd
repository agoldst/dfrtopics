\name{items_time_histogram}
\alias{items_time_histogram}
\title{Histogram of items by year}
\usage{
items_time_histogram(metadata, time_interval = "year")
}
\arguments{
  \item{metadata}{the metadata frame}

  \item{time_interval}{passed on to
  \code{\link[base]{cut.Date}} to bin dates}
}
\value{
A \link[ggplot2]{ggplot} object.
}
\description{
Use the metadata to see the time distribution of the
corpus. Not a very fancy plot.
}
\details{
Facets on item types (often useful, but note that JSTOR's
metadata is imperfect).
}

