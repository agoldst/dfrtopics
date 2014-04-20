\name{topic_time_boxplots}
\alias{topic_time_boxplots}
\title{Plot time-sliced distributions of topic weights in documents}
\usage{
topic_time_boxplots(doctops_long, time_breaks = "5 years", log_scale = T)
}
\arguments{
  \item{doctops_long}{long-form document-topic data frame
  with added pubdate metadata (from
  \code{\link{doc_topics_long}(doctops,metadata,meta_keep="pubdate")})}

  \item{time_breaks}{intervals in which to plot doc-topic
  distributions. For the specification, see
  \code{\link[base]{cut.Date}}. The default is five-year
  intervals.}

  \item{log_scale}{if TRUE (the default), log the y axis.
  Set to FALSE if there are zeroes in the doc-topic
  proportions (i.e. no smoothing).}
}
\value{
A \link[ggplot2]{ggplot} object.
}
\description{
In order to visualize how a topic is "spread out" in
documents over time, this plot shows you boxplots of the
document-weight distribution for a topic, sliced by a
specified time interval, with a smoothing line
superimposed.
}
\seealso{
\code{\link{topic_yearly_barplot}},
\code{\link{topic_yearly_lineplot}},
\code{\link{topic_report}}
}

