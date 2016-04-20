
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
#' This function attempts to produce groups of topics from each model
#' that are close to one another. In particular, it greedily seeks the
#' single-link clustering in which no two topics from the same model
#' are found in the same cluster ("up-to-one mapping"). The idea is
#' from (Chuang et al., 2015). The implementation is my own (slow,
#' unverified) one.
#'
#' Alignment is currently implemented only for models having the same
#' number of topics.
#'
#' @param dst result from \code{\link{model_distances}} (q.v.)
#' @param threshold maximum dissimilarity allowed between cluster members
#'
#' @return a data frame with three columns: \code{model}, \code{topic},
#'   and \code{cluster}, the cluster assignment, designated by an arbitrary
#'   number counted up from 1. Cluster distances are not returned.
#'
#' @seealso \code{\link{model_distances}}
#'
#' @references Chuang, J, et al. 2015. "TopicCheck: Interactive Alignment for Assessing Topic Model Stability." NAACL HLT. \url{http://scholar.princeton.edu/bstewart/publications/topiccheck-interactive-alignment-assessing-topic-model-stability}.
#'
#' @export
align_topics <- function (dst, threshold=1) {
    if(!inherits(dst, "model_distances")) {
        stop("dst must be a model_distances object. Use model_distances() to
derive one from a list of models.")
    }
    # TODO don't really need to enforce same K for all models
    K <- nrow(dst[[1]][[1]])
    M <- length(dst) + 1

    cl <- naive_cluster(unnest_model_distances(dst),
                        M, K, threshold)

    data.frame(
        model=rep(1:M, each=K),
        topic=rep(1:K, times=M),
        cluster=cl + 1 # naive_cluster numbers clusters from 0
    )
}



summary.topic_alignment <- function (x, cluster_size) {
    x$sizes <- table(x$cluster)[x$cluster]
    mask <- x$sizes >= cluster_size
    result <- data.frame(model=x$model[mask],
                         topic=x$topic[mask],
                         cluster=x$cluster[mask],
                         size=x$sizes[mask])
    result <- group_by_(result, ~ cluster)
    stop("incomplete")
}

