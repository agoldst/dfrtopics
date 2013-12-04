# Some functions for comparing topics by finding distances between them.

#' Jensen-Shannon divergence between two vectors
#'
#' This function computes the Jensen-Shannon divergence between two vectors, understood as
#' distributions over the index.
#'
#' @param P,Q vectors representing the distributions. Must be of same length.
#'
#' @return 
#' \deqn{\sum_j \frac{1}{2}P(j)\textrm{log}\left(\frac{2P(j)}{P(j) + Q(j)}\right)
#' + \frac{1}{2}Q(j)\textrm{log}\left(\frac{2P(j)}{P(j) + Q(j)}\right)}
#'
#' @seealso
#' \code{\link{topic_divergences}}
#'
#' @export
#'
JS_divergence <- function(P,Q) {
    PQ_mean = (P + Q) / 2
    sum((1/2) * (P * log(P / PQ_mean) + Q * log(Q / PQ_mean)))

}

#' Measure distances between topics
#'
#' Given a matrix with topics in rows, calculate a distance matrix between the rows using 
#' one of the common heuristics for taking topic distances.
#'
#' The generic name is a reminder that there are multiple possible
#' applications to a hierarchical model: we can consider topics as distributions over 
#' documents as well as over words.
#'
#' @param M matrix, with rows representing topics considered as distributiosn over the 
#' column index
#' @param method \code{JS} for the Jensen-
#' Shannon divergence from \code{\link{JS_divergence}}, or \code{pearson} for the Pearson 
#' correlation coefficient (recommended: take 
#' logs first) or \code{spearman} for Spearman's \eqn{rho}, or 
#'
#' @seealso
#' \code{\link{JS_divergence}},
#' \code{\link{doc_topic_cor}},
#' \code{\link{topic_divergences}}
#'
#' @export
#'
row_dists <- function(M,method="JS") {
    if(method=="pearson" || method=="spearman") {
        return(cor(t(M),method=method))
    } else if(method=="JS") {
        # FIXME failure to vectorize. Ugh.

        n <- nrow(M)
        result <- matrix(0,nrow=n,ncol=n)

        for(i in seq(n)) {
            for(j in i:n) {
                result[i,j] <- JS_divergence(M[i,],M[j,])
            }
        }
        # at least take advantage of the symmetry
        result[lower.tri(result)] <- t(result)[lower.tri(result)]
        return(result)
    } else {
        stop("Unknown method.")
    }
}

#' Take topic-topic correlations over documents
#'
#' Calculates correlations between topics according to their log proportions in documents.
#'
#' @param doctops The document-topic matrix or dataframe (from 
#' \code{\link{doc_topics_frame}}), assumed to be smoothed and normalized.
#'
#' @return a matrix of correlations between the series of log-document proportions.
#'
doc_topic_cor <- function(doctops) {
    # copy on modify
    doctops$id <- NULL
    row_dists(log(t(doctops)),method="pearson")
}

#' Topic-topic divergences over words
#'
#' Calculates the J-S divergences between topics considered as
#' distributions over words.
#'
#' Actually, nothing stops you setting \code{twm} to be the topic-\emph{document} matrix 
#' and \code{b} to be the  vector of \eqn{\alpha_k}. That gives the distances among topics 
#' as distributions over 
#' documents.
#'
#' @param twm the topic-word matrix
#'
#' @param b the estimated \eqn{\beta} value of the model
#'
#' @references
#' Mimno, D. 2012. Computational historiography: Data mining in
#' a century of classics journals. \emph{ACM J. Comput. Cult. Herit.} 5, no. 1
#' (April 2012): article 3. \url{http://doi.acm.org/10.1145/2160165.2160168}.
#'
#' @seealso
#' \code{\link{row_dists}},
#' \code{\link{JS_divergence}}
#'
#' @export
#'
topic_divergences <- function(twm,b) {
    # smoothing
    twm <- twm + b
    # normalization
    twm <- diag(1 / rowSums(twm)) %*% twm
    row_dists(twm,method="JS")
}
