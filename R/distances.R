# Some functions for comparing topics by finding distances between them.

#' Jensen-Shannon divergence between two vectors
#'
#' This function computes the Jensen-Shannon divergence between two vectors,
#' understood as distributions over the index.
#'
#' @param P,Q vectors representing the distributions. Must be of same length.
#'
#' @return \deqn{\sum_j \frac{1}{2}P(j) log\left(\frac{2P(j)}{P(j) +
#' Q(j)}\right) + \frac{1}{2}Q(j) log\left(\frac{2P(j)}{P(j) +
#' Q(j)}\right)}
#'
#' @seealso \code{\link{topic_divergences}}
#'
#' @export
#'
JS_divergence <- function(P, Q) {
    PQ_mean = (P + Q) / 2
    sum((P * log(P / PQ_mean) + Q * log(Q / PQ_mean)) / 2)
}

#' Measure matrix row distances
#'
#' In topic modeling, we generally deal with matrices whose rows
#' represent probability distributions. To find the distance between
#' such distributions, we normally do not use the Euclidean distance or
#' the other options supplied by \code{\link[stats]{dist}}. This is a
#' utility function for taking a matrix with \eqn{K} rows and producing
#' the matrix of distances between those rows, given an arbitrary
#' metric. It is not fast. On matrices with very many columns, R may
#' thrash endlessly; it is recommended that you drop most columns before
#' calculating distances with this (usually keeping only, say, the
#' thousand columns with the largest total weight does not affect the
#' results unduly).
#'
#' The \pkg{flexmix} package supplies a K-L divergence function \code{KLdiv},
#' but I have not found an implementation of the symmetrized Jensen-Shannon
#' divergence, so I have supplied one in \code{\link{JS_divergence}}.
#'
#' @param x matrix
#' @param g metric (function of two vectors). J-S divergence by default
#'
#' @return matrix of distances between rows
#'
#' @seealso \code{\link{topic_divergences}}, \code{\link{JS_divergence}}, \code{\link{doc_topic_cor}},
#'
#' @export
#'
row_dists <- function (x, g=JS_divergence) {
    # FIXME failure to vectorize. Ugh. Needs Rcpp

    n <- nrow(x)

    gs <- numeric(choose(n, 2))
    tx <- t(x)  # column-subscripting is a little faster

    k <- 0L
    for (j in seq.int(2L, n)) {
        r <- tx[ , j]        # store "row"

        for (i in seq.int(1L, j - 1L)) {
            gs[[k + i]] <- g(tx[ , i], r)
        }
        k <- k + j - 1L
    }
    result <- matrix(0, nrow=n, ncol=n)
    result[upper.tri(result)] <- gs

    # at least take advantage of the symmetry
    result[lower.tri(result)] <- t(result)[lower.tri(result)]
    result
}

#' Topic distance functions
#'
#' Two methods for extracting a matrix of topic-topic distances.
#'
#' @param m \code{mallet_model} model object
#' @param n_words Number of columns of the topic-word matrix to use
#'   in calculation. The words with the top \code{n_words} \emph{total}
#'   weight in the corpus are used. Set to \code{Inf} or to
#'   \code{ncol(topic_words(m))} to use all the words, but be warned
#'   that R rapidly approaches its limits with large vocabularies.
#'   Ordinarily, for a vocabulary of tens of thousands of features,
#'   \code{n_words=1000} will be a fine approximation.
#'
#' @return For \code{doc_topic_cor}, a matrix of correlations between the series
#'   of log-document proportions; for \code{topic_divergences}, a matrix of J-S
#'   divergences between topic distributions over words.
#'
#' @seealso \code{\link{row_dists}}, \code{\link{topic_scaled_2d}}
#'
#' @export
topic_divergences <- function (m, n_words=1000) {
    if (is.null(topic_words(m))) {
        stop("The topic-word matrix must be loaded first. Use load_topic_words.")
    }

    tw <- topic_words(m)
    tw_normed <- tw_smooth_normalize(m)(tw)

    V <- ncol(tw)
    n_words <- min(n_words, V)
    if (n_words < V) {
        w <- order(Matrix::colSums(tw), decreasing=TRUE)[seq(n_words)]
        tw_normed <- tw_normed[ , w]
    }

    row_dists(tw_normed)
}

#' @export
#' @rdname topic_divergences
doc_topic_cor <- function (m) {
    x <- dt_smooth_normalize(m)(doc_topics(m))
    cor(log(x))
}

#' Scaled topic coordinates in 2D space
#'
#' Use multidimensional scaling to obtain two-dimensional coordinates for each
#' topic in a model.
#'
#' The coordinates are derived by finding the Jensen-Shannon divergences between
#' topics considered as distributions over words and then scaling this matrix to
#' two dimensions.
#'
#' @param m \code{mallet_model} model object
#'
#' @param n_words Number of columns of the topic-word matrix to use
#'   in calculation. Passed on to \code{link{topic_divergences}}.
#'
#' @return a matrix with 2 columns and as many rows as \code{m}.
#'
#' @seealso \code{\link{row_dists}}, \code{\link{topic_divergences}}
#'
#' @references Mimno, D. 2012. Computational historiography: Data mining in a
#' century of classics journals. \emph{ACM J. Comput. Cult. Herit.} 5, no. 1
#' (April 2012): article 3. \url{http://doi.acm.org/10.1145/2160165.2160168}.
#'
#' @export
#'
topic_scaled_2d <- function (m, n_words=1000) {
    d <- topic_divergences(m, n_words)
    cmdscale(d, k=2)
}

