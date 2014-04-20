\name{topic_name}
\alias{topic_name}
\title{Get a label for a topic}
\usage{
topic_name(keys, topic = unique(keys$topic), fmt = "\%03d \%s", ...)
}
\arguments{
  \item{topic}{the topic number (one-based)}

  \item{keys}{the weighted key-word frame from
  \code{\link{weighted_keys_frame}}}

  \item{fmt}{\code{\link[base]{sprintf}} format string with
  one slot for the topic number and one for a string}

  \item{...}{passed on to \code{\link{topic_top_words}}:
  for example, \code{n}.}
}
\value{
a string
}
\description{
Returns a label for a topic based on the most prominent
words
}
\seealso{
\code{\link{topic_top_words}},
\code{\link{weighted_keys_frame}}
}

