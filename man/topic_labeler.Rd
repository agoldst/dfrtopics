\name{topic_labeler}
\alias{topic_labeler}
\title{Get a function to label topics}
\usage{
topic_labeler(keys, ...)
}
\arguments{
  \item{keys}{the weighted key-word frame from
  \code{\link{weighted_keys_frame}}}

  \item{...}{passed on to \code{\link{topic_name}}}
}
\value{
a function of a single variable, mapping topic numbers to
labels
}
\description{
Convenience wrapper for currying \code{\link{topic_name}}
to give a function of a single argument, to be used in
conjunction with some plotting functions. Can also be
spelled \code{\link{topic_labeller}}.
}
\seealso{
\code{\link{topic_name}}, \code{\link{topic_top_words}},
\code{\link{weighted_keys_frame}},
\code{\link{topic_yearly_lineplot}},
\code{\link{topic_yearly_barplot}},
\code{\link{topic_dist_plot}}
}

