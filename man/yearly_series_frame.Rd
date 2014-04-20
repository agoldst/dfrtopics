\name{yearly_series_frame}
\alias{yearly_series_frame}
\title{Convert a matrix of time series to a dataframe}
\usage{
yearly_series_frame(yearly, total = F, year_seq = colnames(yearly),
  var_seq = rownames(yearly), series_names = c("word", "year", "weight"))
}
\arguments{
  \item{yearly}{matrix with variables in rows and time
  measurements in columns}

  \item{total}{if TRUE, sum over rows to produce a single
  total series}

  \item{year_seq}{character vector giving the dates the
  columns of \code{yearly} correspond to (by default,
  assume these are stored in
  \code{\link{colnames}(yearly)})}

  \item{var_seq}{character vector giving names of variables
  in rows of \code{yearly}}

  \item{series_names}{column names for final dataframe}
}
\value{
three-column data frame ("long" form)
}
\description{
Utility wrapper for \code{\link[reshape2]{melt}} on
matrices with parallel time series in rows.
}
\seealso{
TODO vis, \code{\link{topic_proportions_series_frame}}
}

