
#' Draw from multinomial distributions
#'
#' According to the generative model of LDA, documents are drawn from
#' mixtures of multinomial distributions over the vocabulary. When we
#' simulate from the posterior, our task in practice is: given the
#' number of words \eqn{n} allocated to topic \eqn{k} in document
#' \eqn{d}, generate the result of \eqn{n} multinomial trials with word
#' probabilities given from topic \eqn{k}. This function tries to do
#' this efficiently given a vector of \eqn{n} values and a vector of
#' topic weights.
#'
#' R's built-in \code{\link[stats]{rmultinom}} has two disadvantages
#' here. First, it is set up to generate many samples, each with
#' the same number of trials. But we require varying the number of
#' trials to correspond to our varying numbers of words allocated
#' to the given topic, so we would have to call \code{rmultinom}
#' once for each document and then \code{rbind} the results. Second,
#' because the vocabulary can be large and topics typically allocate
#' most of the probability to only a few words, most elements of
#' each sample vector will be zero. But the built-in function cannot
#' take advantage of this sparsity and will require space for a full
#' simulated term-document matrix. This function, by contrast, returns a
#' sparse \code{\link[Matrix]{Matrix}}.
#'
#' Note that the parameters are not the same as \code{rmultinom}'s.
#' The equivalent of \code{rmultinom(n, size, prob)} is
#' \code{rmultinom_sparse(rep(size, n), prob)}.
#'
#' @param nn vector of trial sizes: \code{nn[i]} gives the number of
#' words to draw in the \code{i}th trial.
#'
#' @param probs vector of word weights: \code{probs[j]/sum(probs[j])}   
#' gives the probability of word \code{j} in a single trial. It need    
#' not be normalized.                                                   
#'
#' @return sparse \code{\link[Matrix]{Matrix}} of sampled term-document
#' counts, with terms in rows and documents in columns. Notice that
#' this means individual multinomial samples are \emph{columns} of the
#' returned matrix.  
#'
#' @seealso \code{\link{imi_check}} and \code{\link{mi_check}} which use this
#' @export
#'
rmultinom_sparse <- function (nn, probs) {
    lst <- draw_multinom(nn, probs)
    Matrix::sparseMatrix(i=lst$i, p=lst$p, x=lst$x,
                         dims=c(length(probs), length(nn)),
                         index1=FALSE)
}

