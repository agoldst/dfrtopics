\name{train_model}
\alias{train_model}
\title{Train a topic model}
\usage{
train_model(instances, n_topics, alpha_sum = 5, beta = 0.01,
  n_iters = 200, n_max_iters = 10, optimize_hyperparameters = T,
  n_hyper_iters = 20, n_burn_in = 50, symmetric_alpha = F, threads = 4L,
  seed = NULL)
}
\arguments{
  \item{instances}{either an rJava reference to an
  \code{InstanceList} object or the name of a MALLET
  instances file}

  \item{n_topics}{how many topics to train?}

  \item{alpha_sum}{initial sum of hyperparameters
  \eqn{alpha_k}: priors of topics over document}

  \item{beta}{initial value of hyperparameter \eqn{\beta}:
  prior of topics over words}

  \item{n_iters}{number of Gibbs sampling iterations to
  run}

  \item{n_max_iters}{number of "iterated conditional
  modes"}

  \item{optimize_hyperparameters}{if TRUE (the default),
  optimize \eqn{\alpha_k} and \eqn{\beta}. If FALSE, the
  value of \code{symmetric_alpha} is ignored.}

  \item{n_hyper_iters}{how often to do hyperparameter
  optimization}

  \item{n_burn_in}{number of initial "burn-in" iterations
  before hyperparameter optimization}

  \item{symmetric_alpha}{if FALSE (the default), allow the
  \eqn{\alpha_k} to be different from one another. If TRUE
  when \code{optimize_hyperparameters} is TRUE, then the
  sum of the alphas will still be varied by the algorithm,
  but all the \eqn{\alpha_k} will be the same.}

  \item{threads}{number of threads to run in parallel.}

  \item{seed}{MALLET's random number seed: set this to
  ensure a reproducible model. instances: can either be a
  mallet instances object or the name of a mallet instances
  file}
}
\value{
the trainer object, which holds a reference to the
\code{RTopicModel} object constructed by
\code{\link[mallet]{MalletLDA}}. In order to access the
full MALLET Java API, use this object's reference to the
\code{ParallelTopicModel}, which is simply
\code{trainer$model}. You can then call all the
\code{ParallelTopicModel} methods, though Java's strict
typing means you'll have some funtimes with
\code{\link[rJava]{.jcall}}. To index into array-like
objects from Java, apply \code{\link[base]{as.integer}} to
parameters.
}
\description{
Invokes MALLET's parallel topic modeling algorithm.
}
\details{
If java gives out-of-memory errors, try increasing the Java
heap size to a large value, like 4GB, by setting
\code{options(java.parameters="-Xmx4g")} \emph{before}
loading this package (or rJava).
}
\seealso{
\code{\link{make_instances}}, \code{\link{make_instances}},
\code{\link{model_documents}}, \code{\link{output_model}}
}

