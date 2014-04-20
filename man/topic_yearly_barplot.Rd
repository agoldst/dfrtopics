\name{topic_yearly_barplot}
\alias{topic_yearly_barplot}
\title{Plot time series of yearly topic proportions as bars}
\usage{
topic_yearly_barplot(series, topic_label = function(n) {     sprintf("\%03d",
  n) })
}
\arguments{
  \item{series}{three-column data frame of dates, topics,
  and weights, from
  \code{\link{topic_proportions_series_frame}}}

  \item{topic_label}{function mapping topic numbers to
  labels. Recommended: pass the result of
  \code{\link{topic_labeller}(wkf,...)}. By default just
  the topic number is used.}
}
\value{
A \link[ggplot2]{ggplot} object.
}
\description{
Plot time series of topics as bars (faceted for multiple
topics).
}
\details{
This is a convenience function for quickly visualizing
output from \code{\link{topic_proportions_series_frame}}.
It does not offer fine-grained control over the plot: for
that, I recommend making plots yourself from the data
frame.
}
\seealso{
\code{\link{topic_proportions_series_frame}} to generate
the data needed for the plot, \code{\link{topic_labeller}}
for generating facet titles,
\code{\link{topic_yearly_lineplot}} for bars instead of
lines showing the same data.
}

