
#' Calculate topic dissimilarity across models
#'
#' This function calculates dissimilarities between topic-word
#' distributions over a list of models. The result can be used to align
#' topics in different models of the same (or similar) corpora: see
#' \code{\link{align_topics}}.
#'
#' @param ms list of \code{\link{mallet_model}} objects
#'
#' @param n_words number of top words from each topic to consider
#'
#' @param g dissimilarity function taking two topic-word matrices and
#' returning the matrix of dissimilarities between rows, \eqn{d_{ij} =
#' g(\theta_i, \theta_j)}. By default, the Jensen-Shannon divergence
#' is used (\code{\link{JS_divergence}}). Or you might try the cosine
#' distance (\code{\link{cosine_distance}}). If you have
#' a function \code{f} of two vectors, you can lift it to matrix rows, at
#' a speed penalty, as \code{function (X, Y) apply(Y, 1, function (y)
#' apply(X, 1, f, y))} (N.B. the transpose is necessary).
#'
#' @return a \code{model_distances} object, which is simply a list
#' of lists of matrices representing the upper block-triangle of
#' distances. Specifically, if \code{x} is the result of the function,
#' the dissimilarity between topic \code{i} from model \code{m1} and
#' topic \code{j} from model \code{m2 > m1} is found at \code{x[[m1]][[m2 -
#' m1]][i, j]}. For convenience, this can be expressed as \code{x[m1,
#' m2, i, j]}.
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
                ],
                matrix(0, nrow(x), sum(unshared_j))
            ) + hyperparameters(ms[[i]])$beta
            x <- normalize_rows(x)

            y <- tws[[j]]
            y <- cbind(
                y[ , shared_j],
                matrix(0, nrow(y), sum(unshared_i)),
                y[ , match(
                    vocs[[j]][unshared_j],
                    v_j)
                ]
            ) + hyperparameters(ms[[j]])$beta
            y <- normalize_rows(y)

            # the resulting matrix is g_ij
            result[[i]][[j - i]] <- g(x, y)
        }
    }

    structure(result,
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
        x[[m1]][[m2 - m1]][i, j]
    } else {
        x[[m2]][[m1 - m2]][j, i]
    }
}

# utility function: flatten a model_distances into a vector ordered as expected by naive_cluster
unnest_model_distances <- function (dst) {
    do.call(c,
        lapply(dst,
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
#' unverified) one. To prepare topic dissimilarities to supply to this
#' function, use \code{\link{model_distances}}.
#'
#' Alignment is currently implemented only for models having the same
#' number of topics.
#'
#' @param dst result from \code{\link{model_distances}} (q.v.)
#'
#' @param threshold maximum dissimilarity allowed between cluster members. By
#'   default, the threshold is set so that any two topics from different models
#'   may ultimately join a cluster. More aggressive thresholding is
#'   recommended, in order to expose isolated topics.
#'
#' @return a matrix of cluster assignments, where the \eqn{i,j} element is the
#'   cluster number of topic \eqn{j} in model \eqn{i}. This matrix also
#'   receives a \code{distances} attribute which gives the distance at which
#'   the given element was merged into its cluster. Because single-link
#'   clustering (if I've even implemented it correctly) is subject to
#'   "chaining," this is not necessarily an indication of the quality of a
#'   cluster, but it may give some hints. To explore the result,
#'   \code{\link{alignment_frame}} or simply \code{\link{gather_matrix}} may be
#'   useful.
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
#' gather_matrix(clusters, col_names=c("model", "topic", "cluster"))
#' }
#'
#' @export
align_topics <- function (dst, threshold) {
    if(!inherits(dst, "model_distances")) {
        stop("dst must be a model_distances object. Use model_distances() to
derive one from a list of models.")
    }
    # TODO don't really need to enforce same K for all models
    K <- nrow(dst[[1]][[1]])
    M <- length(dst) + 1

    dst_flat <- unnest_model_distances(dst)

    if (missing(threshold)) {
        threshold <- max(dst_flat) + 1
    }

    cl <- naive_cluster(dst_flat, M, K, threshold)

    structure(
        # naive_cluster numbers clusters from 0
        matrix(cl$clusters + 1, nrow=M, byrow=T),
        distances=matrix(cl$distances, nrow=M, byrow=T)
    )
}

#' Organize alignment results for inspection
#'
#' Once you've clustered topics with \code{\link{align_topics}}, this function
#' summarizes the results as a data frame grouping topic labels by cluster.
#' Many other ways of investigating a clustering are of course possible.
#'
#' @param clusters from \code{\link{align_topics}}
#' @param dst from \code{\link{model_distances}}
#' @param ms list of models as supplied to \code{\link{model_distances}}
#'
#' @return a data frame. \code{d} is the distance between clusters at merge.
#'
#' @export
alignment_frame <- function (clusters, dst, ms) {
    result <- gather_matrix(clusters, col_names=c("model", "topic", "cluster"))
    result$d <- as.numeric(t(attr(clusters, "distances")))
    result <- dplyr::group_by_(result, ~ cluster)
    result <- dplyr::mutate_(result, size=~ length(cluster))
    result <- dplyr::ungroup(result)
    result <- dplyr::group_by_(result, ~ model)
    mut <- lazyeval::interp(~ topic_labels(x[[model[1]]])[topic], x=ms)
    result <- dplyr::mutate_(result, label=mut)
    result <- dplyr::ungroup(result)
    result <- dplyr::arrange_(result,
        ~ dplyr::desc(size), ~ cluster, ~ model, ~ topic)
    dplyr::select_(result, ~ cluster, ~ model, ~ topic, ~ label, ~ d)
}
