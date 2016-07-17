
#' Calculate topic dissimilarity across models
#'
#' This function calculates dissimilarities between topic-word
#' distributions over a list of models. The result can be used to align
#' topics in different models of the same (or similar) corpora: see
#' \code{\link{align_topics}}.
#'
#' The models in \code{ms} need not have the same number of topics.
#'
#' @param ms list of \code{\link{mallet_model}} objects
#'
#' @param n_words number of top words from each topic to consider
#'
#' @param g dissimilarity function taking two topic-word matrices and
#' returning the matrix of dissimilarities between rows, \eqn{d_{ij} =
#' g(\theta_i, \theta_j)}. By default, the Jensen-Shannon divergence
#' is used (\code{\link{JS_divergence}}). Or you might try the cosine
#' distance (\code{\link{cosine_distance}}). If you have a function
#' \code{f} of two vectors, you can lift it to matrix rows, at a speed
#' penalty, as \code{function (X, Y) apply(Y, 1, function (y) apply(X,
#' 1, f, y))} (N.B. the transpose is necessary).
#'
#' @return a \code{model_distances} object, which is a list
#' including elements \code{d}, a list of lists of matrices representing the
#' upper block-triangle of distances, and {ms, n_words, g} storing
#' the arguments. If \code{x} is the result of the
#' function, the dissimilarity between topic \code{i} from model
#' \code{m1} and topic \code{j} from model \code{m2 > m1} is found at
#' \code{x$d[[m1]][[m2 - m1]][i, j]}. For convenience, this can be
#' expressed as \code{x[m1, m2, i, j]}.
#'
#' @seealso \code{\link{align_topics}}
#'
#' @export
model_distances <- function (ms, n_words, g=JS_divergence) {
    n <- length(ms)
    tws <- lapply(ms, topic_words)

    vocs <- vector("list", n)
    for (i in seq_along(ms)) {
        m <- ms[[i]]
        ij <- top_n_row(tws[[i]], n_words)
        vocs[[i]] <- unique(vocabulary(m)[ij[ , 2]])
    }

    result <- vector("list", n - 1)
    for (i in seq(1, n - 1)) {
        result[[i]] <- vector("list", n - i)
        for (j in seq(i + 1, length(tws))) {
            v_i <- vocabulary(ms[[i]])
            v_j <- vocabulary(ms[[j]])
            shared_i <- which(vocs[[i]] %in% vocs[[j]])
            unshared_i <- ! (vocs[[i]] %in% vocs[[j]])
            shared_j <- match(vocs[[i]][shared_i], v_j)
            unshared_j <- ! (vocs[[j]] %in% vocs[[i]])


            x <- tws[[i]]
            x <- cbind(
                x[ , match(
                    vocs[[i]][c(shared_i, which(unshared_i))],
                    v_i)
                ] + hyperparameters(ms[[i]])$beta,
                matrix(0, nrow(x), sum(unshared_j))
            )
            x <- normalize_rows(x)

            y <- tws[[j]]
            y <- cbind(
                y[ , shared_j] + hyperparameters(ms[[j]])$beta,
                matrix(0, nrow(y), sum(unshared_i)),
                y[ , match(
                    vocs[[j]][unshared_j],
                    v_j)
                ] + hyperparameters(ms[[j]])$beta
            )
            y <- normalize_rows(y)

            if (xor(is(x, "Matrix"), is(y, "Matrix"))) {
                # coerce to ordinary matrix if exactly one is Matrix
                # to avoid annoying errors with g
                x <- as.matrix(x)
                y <- as.matrix(y)
            }
            # the resulting matrix is g_ij
            result[[i]][[j - i]] <- g(x, y)
        }
    }

    structure(
        list(
            d=result,
            ms=ms,
            n_words=n_words,
            g=g
        ),
        class="model_distances"
    )
}

#' @rdname model_distances
#' @export
`[.model_distances` <- function (x, m1, m2, i, j) {
    if (missing(m1) || missing(m2)) {
        stop("Missing model index")
    }
    if (m1 == m2) {
        stop("Model indices must differ")
    }
    if (m1 < m2) {
        x$d[[m1]][[m2 - m1]][i, j]
    } else {
        x$d[[m2]][[m1 - m2]][j, i]
    }
}

#' @rdname model_distances
#' @export
print.model_distances <- function (x) {
    cat("Distances between topics from model_distances\n")
    cat("Number of top words used: ", x$n_words, "\n")
    cat("Model: ",
        sprintf("%4.4s", seq(length(x$d[[1]]) + 1)))
    cat("\nTopics:",
        sprintf("%4.4s", c(nrow(x$d[[1]][[1]]), vapply(x$d[[1]], ncol, 0))))
}


# utility function: flatten a model_distances into a vector ordered as expected by naive_cluster
unnest_model_distances <- function (dst) {
    do.call(c,
        lapply(dst$d,
            function (d) {
                # unrolling by columns, so transpose
                do.call(c, lapply(d, function (x) as.numeric(t(x))))
           }
        )
    )
}

#' Align topics across models
#'
#' Given information about the dissimilarities among topics across
#' a set of models, this function attempts to identify groups of
#' similar topics from each model. In particular, it greedily seeks
#' the single-link clustering in which no two topics from the same
#' model are found in the same cluster ("up-to-one mapping"). The idea
#' is from (Chuang et al., 2015). The implementation is my own (slow,
#' unverified, \emph{experimental}) one. To prepare topic dissimilarities to
#' supply to this function, use \code{\link{model_distances}}.
#'
#' @param dst result from \code{\link{model_distances}} (q.v.)
#'
#' @param threshold maximum dissimilarity allowed between merging clusters. By
#'   default, the threshold is set so that any two topics from different models
#'   may ultimately join a cluster. More aggressive thresholding is
#'   recommended, in order to expose isolated topics.
#'
#' @return a \code{topic_alignment} object, which is a list of:
#' \describe{
#'   \item{\code{clusters}}{list of vectors, one for each model, giving cluster
#'     numbers of the topics in the model}
#'   \item{\code{distances}}{list of vectors, one for each model, giving the
#'     distance at which the given topic merged into its cluster. Because
#'     single-link clustering (if I've even implemented it correctly) is
#'     subject to "chaining," this is not necessarily an indication of the
#'     quality of a cluster, but it may give some hints.}
#'   \item{\code{model_distances}}{The supplied \code{model_distances}}
#'   \item{\code{threshold}}{The threshold used}
#' }
#' To explore the result, \code{\link{alignment_frame}} may be useful.
#'
#' @seealso \code{\link{model_distances}}, \code{\link{alignment_frame}}
#'
#' @references Chuang, J, et al. 2015. "TopicCheck: Interactive
#' Alignment for Assessing Topic Model Stability." NAACL HLT.
#' \url{http://scholar.princeton.edu/bstewart/publications/topiccheck-interactive-alignment-assessing-topic-model-stability}.
#'
#' @examples
#'
#' \dontrun{
#' # assume m1, m2, m3 are models
#' dists <- model_distances(list(m1, m2, m3), n_words=40)
#' clusters <- align_topics(dists, threshold=0.5)
#' # data frame readout
#' alignment_frame(clusters)
#' }
#'
#' @export
align_topics <- function (dst, threshold) {
    if(!inherits(dst, "model_distances")) {
        stop("dst must be a model_distances object. Use model_distances() to
derive one from a list of models.")
    }
    K <- c(nrow(dst$d[[1]][[1]]),
           vapply(dst$d[[1]], ncol, integer(1)))

    dst_flat <- unnest_model_distances(dst)

    if (missing(threshold)) {
        threshold <- max(dst_flat) + 1
    }

    cl <- naive_cluster(dst_flat, K, threshold)

    structure(
        list(
            # relabel clusters as sequential numbers from 1
             clusters=lapply(cl$clusters, match,
                 sort(unique(unlist(cl$clusters)))),
             distances=cl$distances,
             model_distances=dst,
             threshold=threshold
        ),
        class="topic_alignment"
    )
}

#' @rdname align_topics
#' @export
print.topic_alignment <- function (x) {
    cat("A topic clustering from align_topics\n")
    cat("Cluster assignments:\n")
    print(x$clusters)
}

#' Organize alignment results for inspection
#'
#' Once you've clustered topics with \code{\link{align_topics}}, this function
#' summarizes the results as a data frame grouping topic labels by cluster.
#' Many other ways of investigating a clustering are of course possible.
#'
#' @param clusters from \code{\link{align_topics}}
#'
#' @return A data frame. \code{d} is the distance between clusters at merge.
#'
#' @seealso \code{\link{align_topics}}, \code{\link{widths}}
#'
#' @export
alignment_frame <- function (clusters) {
    K <- vapply(clusters$clusters, length, 0)
    result <- data.frame(model=rep(seq_along(K), times=K),
               topic=unlist(lapply(K, seq)),
               cluster=unlist(clusters$clusters))
    result$d <- unlist(clusters$distances)
    result <- dplyr::group_by_(result, ~ cluster)
    result <- dplyr::mutate_(result, size=~ length(cluster))
    result <- dplyr::ungroup(result)
    result <- dplyr::group_by_(result, ~ model)
    mut <- lazyeval::interp(~ topic_labels(x[[model[1]]])[topic],
        x=clusters$model_distances$ms)
    result <- dplyr::mutate_(result, label=mut)
    result <- dplyr::ungroup(result)
    result <- dplyr::arrange_(result,
        ~ dplyr::desc(size), ~ cluster, ~ model, ~ topic)
    dplyr::select_(result, ~ cluster, ~ model, ~ topic, ~ label, ~ d)
}

#' Aligned-topic cluster widths
#'
#' Finds the maximum pairwise distance within each cluster. These "widths" may
#' help to diagnose cluster quality.
#'
#' @param x result from \code{\link{align_topics}}
#'
#' @return a vector whose \code{i}th element is the width of cluster \code{i}.
#' If there is no cluster with that number, the corresponding element is
#' \code{NA}. Single-member clusters have a width of zero.
#'
#' @seealso \code{\link{align_topics}}, \code{\link{alignment_frame}}
#'
#' @export
widths <- function (x) UseMethod("widths")

#' @export
widths.topic_alignment <- function (x) {
    naive_cluster_width(x$clusters,
        unnest_model_distances(x$model_distances)
    )
}

# The naivest cluster algorithm
#
# For testing purposes, this function implements the single-linkage clustering
# algorithm described in
# \href{https://en.wikipedia.org/wiki/Single-linkage_clustering}{Wikipedia}.
# It should yield the same clustering as \code{\link{align_topics}} (for the
# sketch of a proof, see comments on the source code in \code{cluster.cpp}).
#
naivest_cluster <- function (dst, threshold=Inf, verbose=FALSE) {
    K <- c(nrow(dst$d[[1]][[1]]),
           vapply(dst$d[[1]], ncol, integer(1)))
    M <- length(K)
    # model membership indicator for topic sequence
    ms <- rep(seq_along(K), times=K)
    # topic indicator
    ks <- do.call(c, lapply(K, seq))

    # construct upper-tri distance matrix D (probably faster ways to do this)
    D <- matrix(NA, nrow=sum(K), ncol=sum(K))
    cumK <- c(0, cumsum(K))
    for (m1 in 1:(M - 1))
        for (m2 in (m1 + 1):M)
            D[(1 + cumK[m1]):cumK[m1 + 1],
              (1 + cumK[m2]):cumK[m2 + 1]] <- dst[m1, m2]
    # copy to lower-tri
    D[lower.tri(D)] <- t(D)[lower.tri(D)]

    # initial singleton clusters
    cl <- as.list(seq(sum(K)))

    allowable <- function (ds)
        length(intersect(ms[cl[[ds[1]]]], ms[cl[[ds[2]]]])) == 0

    if (verbose) {
        fmt <- function (i) paste(ms[cl[[i]]] - 1, ks[cl[[i]]] - 1,
                                  sep=":", collapse=" ")
        # emit logging information in form comparable to naive_cluster
        blurt <- function (cl1, cl2, d)
            message(fmt(cl1), " | ", fmt(cl2), " [", signif(d, 4), "] ",
                    cl1 - 1, "/", cl2 - 1)
    } else
        blurt <- function (...) { }

    done <- F
    while (!done) {
        done <- T
        for (i in order(D)) {
            if (D[i] > threshold || is.na(D[i]))
                break
            ds <- arrayInd(i, dim(D))
            if (allowable(ds)) {
                done <- F
                break
            }
        }
        if (!done) {
            ds <- sort(ds) # ensure ds[1] is the smaller index
            blurt(ds[1], ds[2], D[i])
            # merge
            cl[[ds[1]]] <- c(cl[[ds[1]]], cl[[ds[2]]])
            cl[[ds[2]]] <- NULL
            D[ds[1], ] <- pmin(D[ds[1], ], D[ds[2], ])
            D <- D[-ds[2], -ds[2]]

            # TODO heights
        }
    }

    # unravel cl
    result_flat <- numeric(sum(K))
    for (i in seq_along(cl)) {
        result_flat[cl[[i]]] <- i
    }

    lapply(seq(M), function (m)
        result_flat[(1 + cumK[m]):cumK[m + 1]]
    )

}


