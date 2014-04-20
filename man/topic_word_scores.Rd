\name{topic_word_scores}
\alias{topic_word_scores}
\title{Convert raw topic-word counts to weighted scores}
\usage{
topic_word_scores(tw, b, method = "blei_lafferty")
}
\arguments{
  \item{tw}{the topic-word matrix (assumed to be raw counts
  of word-topic assignments)}

  \item{b}{\eqn{beta}, the smoothing over words (zero
  counts are replaced with this value)}

  \item{method}{The scoring method. Only the default value,
  \code{blei_lafferty}, is currently supported.}
}
\value{
a matrix of weighted scores with topics in rows and words
in columns.
}
\description{
Given an unweighted final sampling state of the assignment
of words to topics, produce a topic-word matrix with
weighted scores instead.
}
\details{
The only supported method is that given by Blei and
Lafferty: the score for word {v} in topic \eqn{t} is
\deqn{p(t,v)\textrm{log}(p(t,v) / \prod_k p(k,v)^1/K)}
where \eqn{K} is the number of topics. The score gives more
weight to words which are ranked highly in fewer topics.

To generate "key words" from the new scoring, apply
\code{\link{tw_wkf}} to the results of this function.
}
\references{
D. Blei and J. Lafferty. Topic Models. In A. Srivastava and
M. Sahami, editors, \emph{Text Mining: Classification,
Clustering, and Applications}. Chapman & Hall/CRC Data
Mining and Knowledge Discovery Series, 2009.
\url{http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf}.
}
\seealso{
\code{\link{tw_wkf}}, \code{\link{weighted_keys_frame}}
}

