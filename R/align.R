
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
#' @param g dissimilarity function taking two equal-length vectors and
#' returning a number. By default, the Jensen-Shannon divergence is used
#' (\code{\link{JS_divergence}}).
#'
#' @return a \code{model_distances} object, which is simply a list
#' of lists of matrices representing the upper block-triangle of
#' distances. Specifically, if \code{x} is the result of the function,
#' the dissimilarity between topic \code{i} from model \code{m1} and
#' topic \code{j} from model \code{m2} is found at \code{x[[m1]][[m2 -
#' m1]][j, i]}. For convenience, this can be expressed as \code{x[m1,
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

            # N.B. the resulting matrix is g_ji (transposed)
            result[[i]][[j - i]] <- apply(x, 1, function (topic) {
                apply(y, 1, g, topic)
            })
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
    if (m1 > m2) {
        tmp <- m1
        m1 <- m2
        m2 <- tmp
        tmp <- i
        i <- j
        j <- tmp
    }
    x[[m1]][[m2 - m1]][j, i]
}

# utility function: flatten a model_distances into a vector ordered as expected by naive_cluster
unnest_model_distances <- function (dst) {
    do.call(c,
        lapply(dst,
            function (d) {
                # unroll by columns, undoing the transpose in model_distances
                do.call(c, lapply(d, as.numeric))
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
#'   cluster number of topic \eqn{j} in model \eqn{i}. Cluster distances are
#'   not returned. \code{\link{alignment_frame}}, or, more generally,
#'   \code{\link{gather_matrix}} may be useful for getting the result into
#'   conveniently explorable form.
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

    # naive_cluster numbers clusters from 0
    matrix(cl + 1, nrow=M, byrow=T)
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
#' @return a data frame
#'
#' @export
alignment_frame <- function (clusters, dst, ms) {
    result <- gather_matrix(clusters, col_names=c("model", "topic", "cluster"))
    result <- group_by_(result, ~ cluster)
    result <- mutate_(result, size=~ length(cluster))
    result <- ungroup(result)
    result <- group_by_(result, ~ model)
    mut <- lazyeval::interp(~ topic_labels(x[[model[1]]])[topic], x=ms)
    result <- mutate_(result, label=mut)
    result <- ungroup(result)
    result <- arrange_(result, ~ desc(size), ~ cluster, ~ model, ~ topic)
    select_(result, ~ cluster, ~ model, ~ topic, ~ label)
}
