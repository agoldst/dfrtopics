\name{model_params}
\alias{model_params}
\title{Get miscellaneous model parameters}
\usage{
model_params(trainer)
}
\arguments{
  \item{trainer}{the \code{RTopicModel} object}
}
\value{
a data frame with one row and three columns,
\code{beta,n_tokens,LL}.
}
\description{
Collect together some overall parameters of a model into a
one-row data frame.
}
\details{
The topic model is also estimates a \eqn{\beta} parameter.
We can also remember the final log-likelihood of the model
and the total number of tokens in the instances mallet
operated on.
}

