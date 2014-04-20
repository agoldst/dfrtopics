\name{weighted_keys_frame}
\alias{weighted_keys_frame}
\title{Topic key words with weights}
\usage{
weighted_keys_frame(trainer, n_top_words = 50, smoothed = F,
  normalized = F)
}
\arguments{
  \item{trainer}{reference to the \code{RTopicModel}
  object}

  \item{n_top_words}{number of "top words" for each topic}

  \item{smoothed}{if TRUE, smooth document-topic
  distribution using hyperparameters}

  \item{normalized}{if TRUE, weights sum to 1}
}
\value{
a data frame with \code{n} rows for each topic and four
columns, \code{alpha, topic,word,weight}. \code{alpha} is
repeated \code{n} times in each topic: it gives the topic
hyperparameter \eqn{\alpha_k}. \code{topic} is numbered
from 1. The returned words are in rank order within each
topic.
}
\description{
A more informative topic key-words data frame, in "long"
format.
}
\details{
"Key words" are identified simply by rank order of weight
within the topic. The result also gives the estimated alpha
hyperparameter value for each topic.
}
\seealso{
\code{\link[mallet]{mallet.topic.words}}
\code{\link{tw_wkf}}
}

