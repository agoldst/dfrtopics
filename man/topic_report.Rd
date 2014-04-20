\name{topic_report}
\alias{topic_report}
\title{Overview visualization of multiple topics}
\usage{
topic_report(doc_topics, wkf, metadata, topics = 1:(ncol(doc_topics) - 1),
  time_breaks = "5 years", log_scale = F, output_dir = "topic_report",
  w = 1200, h = 800)
}
\arguments{
  \item{doc_topics}{document-topic frame from
  \code{\link{doc_topics_frame}}}

  \item{wkf}{topic key words frame from
  \code{\link{weighted_keys_frame}}}

  \item{metadata}{metadata frame from
  \code{\link{read_metadata}}}

  \item{topic}{a vector of topics to generate plots for (by
  default, all)}

  \item{time_breaks}{time intervals by which to slice
  document-topic distributions for the box plots: passed on
  to \code{\link{topic_time_boxplots}}.}

  \item{log-scale}{if TRUE, show topic proportions on a log
  scale. For unsmoothed models, set to FALSE so as not to
  take \code{log(0)}.}

  \item{output_dir}{directory to save plots to}

  \item{w}{PNG width (pixels)}

  \item{h}{PNG height (pixels)}
}
\description{
This function is intended for surveying a topic model. It
generates three plots for each topic and saves the three
plots in a single PNG file.
}
\details{
For each topic, the PNG topic has three plots: \enumerate{
\item{The weights of the most probable words in each
topic.} \item{Boxplots of the distributions of
document-topic counts/proportions within short
time-slices.} \item{The yearly proportion of
words/documents in the topic.} } The meaning of the latter
two plots depends on whether the doc-topic scores are
normalized or not.

The plots are saved to \code{output_dir/<NNN>.png}. If you
have more than 1000 topics, other parts of this package
will probably break too.
}
\seealso{
\code{\link{topic_keyword_plot}},
\code{\link{topic_time_boxplots}},
\code{\link{topic_yearly_lineplot}}
}

