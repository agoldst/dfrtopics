\name{topic_top_words}
\alias{topic_top_words}
\title{Get the "top" words in a topic}
\usage{
topic_top_words(keys, topic = keys$topic, n = 4)
}
\arguments{
  \item{keys}{the weighted key-word frame from
  \code{\link{weighted_keys_frame}}}

  \item{topic}{the topic number (one-based). Vectorized in
  topics. By default, all topics are used.}

  \item{n}{the number of words to take.}
}
\value{
a character matrix with the top words in rows and topics in
columns. The rownames give the topic numbers (as strings).
}
\description{
Extracts the most salient words from the weighted key-words
frame.
}
\details{
The "top" words are those with the maximum weightings for
the topic in \code{keys}. In addition to raw ranking (the
default for \code{\link{weighted_keys_frame}}, consider the
"salience" score calculated by
\code{\link{topic_word_scores}}.
}
\seealso{
\code{\link{topic_name}}, \code{\link{topic_names}},
\code{\link{topic_labeller}},
\code{\link{weighted_keys_frame}}
}

