
#' Aggregate (word/topic) counts by time period
#'
#' This convenience function transforms a topic-document or term-document matrix
#' into a topic (term) time-period matrix. This is meant for the common
#' application in which document date metadata will be used to generate time
#' series. Values are normalized so that they total to 1 in each time period.
#' Any matrix can be transformed in this way, however, as long as its columns
#' can be matched against date data.
#'
#' N.B. that though topics are the most obvious row variable and documents are
#' the most obvious column variable, it may also make sense to preaggregate
#' multiple words or topics into some larger construct. Similarly, if the
#' documents can be grouped into aggregates with their own periodicity (e.g.
#' periodical issues), there is no reason not to set \code{tdm} to a matrix with
#' columns already summed together. You can of course also do this summing
#' post-hoc, but then it's important to be careful about normalization.
#' Naturally nothing stops you from supplying a slice of the topic-document
#' matrix to study series of proportions within some subset of topics/documents,
#' rather than the whole. Again interpreting normalized proportions will require
#' some care.
#'
#'
#' @param tdm a matrix (or Matrix) with some feature (e.g. topics or words) in
#'   rows and datable  in columns
#' @param dates a Date vector, one for each column of \code{tdm}
#' @param breaks passed on to \code{link[base]}{cut.Date} (q.v.): what interval
#'   should the time series use?
#'
#' @return A matrix where each row is a time series and each column sums to 1.
#'   If you wish to generate a time series without normalization or with rolling
#'   means or other smoothing, use the \code{\link{sum_col_groups}} function in
#'   conjunction with \code{\link[base]{cut.Date}}.
#'
#' @seealso \code{\link{topic_series}} for the common case in which you
#'   ultimately want a "long" data frame with topic proportions,
#'   \code{\link{sum_col_groups}} and \code{\link{normalize_cols}}, which this
#'   wraps together, \code{\link{gather_matrix}} for converting the result to a
#'   data frame, and \code{\link{doc_topics}} (but you need its
#'   \emph{transpose}), \code{\link{tdm_topic}}, and
#'   \code{\link{instances_Matrix}} for possible matrices to supply here
#'
#' @examples
#' \dontrun{
#' # time series within topic 10 of "solid", "flesh", "melt"
#' # after loading sampling state on model m
#' sm10 <- tdm_topic(m, 10) %>%
#'    word_series_matrix(metadata(m)$pubdate) %>%
#' gather_matrix(sm10[word_ids(c("solid", "flesh", "melt")), ],
#'               col_names=c("word", "year", "weight"))
#' }
#' @export
#'
word_series_matrix <- function (tdm, dates, breaks="years") {
    m_g <- sum_col_groups(tdm, cut(dates, breaks=breaks, ordered=TRUE))
    normalize_cols(m_g)
}

#' Topic time series
#'
#' Generate a data frame with topics over time (based on document metadata).
#' Since it is frequently useful to look at time series of topics, this function
#' provides a short-cut for generating a data frame in a format suitable for
#' plotting from the information contained in a model.
#'
#' Topic weights are smoothed with the estimated hyperparameter \eqn{\beta}
#' before summing and normalizing.
#'
#' See the package vignette for an example of this function in use.
#'
#' @param m \code{mallet_model} object
#' @param breaks passed on to \code{\link[base]{cut.Date}}: what interval
#'   should the time series use?
#'
#' @return a data frame with three columns, \code{topic, pubdate, weight}. The
#'   weights are normalized to sum to 1 in each time period.
#'
#' @export
#'
topic_series <- function (m, breaks="years") {
    tdm <- t(dt_smooth(m)(doc_topics(m)))
    m_s <- word_series_matrix(tdm, metadata(m)$pubdate, breaks)
    result <- gather_matrix(m_s, col_names=c("topic", "pubdate", "weight"))
    result$pubdate <- as.Date(result$pubdate)
    result
}

#' Normalize columns to sum to one
#'
#' A convenience function for a frequent operation of normalizing the columns of
#' a matrix. The typical application in document modeling is to to ensure that
#' the columns sum to one (L1 normalization). Sometimes it is convenient instead
#' to set the columns to have a unit Euclidean norm (L2 normalization).
#'
#' @param x a matrix or Matrix
#' @param norm Either \code{"L1"}, the default (the sum of the absolute value of
#'   weights), or \code{"L2"}, the Euclidean norm
#' @param stopzero If FALSE (the default), columns with norm zero are left
#'   as-is. If this is TRUE, an error will be thrown instead.
#'
#' @return the column-normalized matrix (except for any zero columns)
#'
#' @seealso \code{\link{normalize_cols}}
#' @export
#'
normalize_cols <- function (x, norm="L1", stopzero=FALSE) {
    if (norm == "L1") {
        norms <- Matrix::colSums(abs(x))
    } else if (norm == "L2") {
        norms <- sqrt(Matrix::colSums(x * x))
    } else {
        stop("norm must be L1 or L2")
    }

    if (!stopzero) {
        norms[norms == 0] <- Inf
    } else if (any(norms == 0)) {
        stop("The matrix has columns of all zeroes, which cannot be normalized.")
    }

    rescale_cols(x, 1 / norms)
}

#' Normalize rows to sum to one
#'
#' A convenience function for a frequent operation of normalizing the rows of
#' a matrix. The typical application in document modeling is to to ensure that
#' the rows sum to one (L1 normalization). Sometimes it is convenient instead
#' to set the rows to have a unit Euclidean norm (L2 normalization).
#'
#' @param x a matrix or Matrix
#' @param norm Either \code{"L1"}, the default (the sum of the absolute value of
#'   weights), or \code{"L2"}, the Euclidean norm
#' @param stopzero If FALSE (the default), rows with norm zero are left
#'   as-is. If this is TRUE, an error will be thrown instead.
#'
#' @return the row-normalized matrix (except for any zero rows)
#'
#' @seealso \code{\link{normalize_cols}}
#' @export
#'
normalize_rows <- function (x, norm="L1", stopzero=FALSE) {
    if (norm == "L1") {
        norms <- Matrix::rowSums(abs(x))
    } else if (norm == "L2") {
        norms <- sqrt(Matrix::rowSums(x * x))
    } else {
        stop("norm must be L1 or L2")
    }

    if (!stopzero) {
        norms[norms == 0] <- Inf
    } else if (any(norms == 0)) {
        stop("The matrix has rows of all zeroes, which cannot be normalized.")
    }

    rescale_rows(x, 1 / norms)
}

#' Rescale the columns of a matrix
#'
#' Just a mnemonic for matrix multiplication.
#'
#' @param m matrix or Matrix
#' @param x vector; \code{m[ , j]} is multiplied by \code{x[j]}.
#'
#' @return a matrix of the same dimensions as \code{m}
#'
#' @seealso \code{\link{rescale_rows}}
#'
#' @export
rescale_cols <- function (m, x) {
    if (is(m, "Matrix")) {
        s <- Matrix::Diagonal(x=x)
    } else {
        s <- diag(x)
    }
    result <- m %*% s
    if (!is.null(dimnames(m))) {
        dimnames(result) <- dimnames(m)
    }
    result
}

#' Rescale the rows of a matrix
#'
#' Just a mnemonic for matrix multiplication.
#'
#' @param m matrix or Matrix
#' @param x vector; \code{m[j, ]} is multiplied by \code{x[j]}.
#'
#' @return a matrix of the same dimensions as \code{m}
#'
#' @seealso \code{\link{rescale_cols}}
#' @export
rescale_rows <- function (m, x) {
    if (is(m, "Matrix")) {
        s <- Matrix::Diagonal(x=x)
    } else {
        s <- diag(x)
    }
    result <- s %*% m
    if (!is.null(dimnames(m))) {
        dimnames(result) <- dimnames(m)
    }
    result
}

#' Scoring methods for words in topics
#'
#' The "raw" final sampling state of words in topics may be transformed into
#' either estimated probabilities or other kinds of salience scores. These
#' methods produce \emph{functions} that operate on a topic-word matrix. They
#' can be passed as the \code{weighting} parameter to \code{\link{top_words}}.
#'
#' The basic method (\code{tw_smooth_normalize}) is to recast the sampled word
#' counts as probabilities by adding the estimated hyperparameter \eqn{\beta}
#' and then normalizing rows so they add to 1. This is equivalent to
#' \code{\link[mallet]{mallet.topic.words}} with \code{smooth} and
#' \code{normalize} set to TRUE. Naturally this will not change the relative
#' ordering of words within topics.
#'
#' \code{tw_smooth} simply adds \eqn{\beta} but does not normalize.
#'
#' A method that can re-rank words has been given by Blei and Lafferty: the
#' score for word \eqn{v} in topic \eqn{t} is \deqn{p(t,v) log(p(t,v) /
#' \prod_k p(k,v)^(1/K))} where \eqn{K} is the number of topics. The score gives
#' more weight to words which are ranked highly in fewer topics.
#'
#' Another method is the "relevance" score of Sievert and Shirley: in this case
#' the score is given by \deqn{\lambda log(p(t,v) + (1 - \lambda) log(p(t,v) /
#' p(v)} where \eqn{\lambda} is a weighting parameter which is by default set to
#' 0.6 and which determines the amount by which words common in the whole corpus
#' are penalized.
#'
#' @param m a \code{\link{mallet_model}} object
#' @param lambda For \code{sievert_shirley}, the weighting parameter
#'   \eqn{\lambda}, by default 0.6.
#' @return a function of one variable, to be applied to the topic-word sparse
#'   matrix.
#'
#' @references D. Blei and J. Lafferty. Topic Models. In A. Srivastava and M.
#'   Sahami, editors, \emph{Text Mining: Classification, Clustering, and
#'   Applications}. Chapman & Hall/CRC Data Mining and Knowledge Discovery
#'   Series, 2009.
#'   \url{http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf}.
#'
#'   C. Sievert and K.E. Shirley. LDAvis: A method for visualizing and
#'   interpreting topics.
#'   \url{http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf}.
#'
#'
#' @examples
#' \dontrun{top_words(m, n=10, weighting=tw_blei_lafferty(x))}
#' \dontrun{tw_smooth_normalize(m)(topic_words(m))}
#'
#' @export
#'
tw_smooth_normalize <- function (m) {
    b <- hyperparameters(m)$beta

    function (tw) {
        Matrix::Diagonal(x=1 /
            (Matrix::rowSums(tw) + b * ncol(tw))) %*% (tw + b)
    }
}

#' @export
#' @rdname tw_smooth_normalize
tw_smooth <- function (m) {
    b <- hyperparameters(m)$beta

    function (tw) {
        tw + b
    }
}

#' @export
#' @rdname tw_smooth_normalize
tw_blei_lafferty <- function (m) {

    # score(t,v) = p(t,v) log (p(t,v) / Prod_k p(k,v) ^ 1 / K)
    #            = p(t,v) ( log p(t,v) - (1 / K) log( Prod_k p(k,v) ) )
    #            = p(t,v) ( log p(t,v) - (1 / K) Sum_k (log p(k,v) ) )

    sn <- tw_smooth_normalize(m)

    function (tw) {
        tw <- sn(tw)
        n <- nrow(tw)
        log_tw <- log(tw)

        # calculate down-weighting factor for each word.
        # for some unknown reason I seem to need to explicitly dispatch to
        # the Matrix method here
        word_factor <- tw %*% Matrix::Diagonal(x=Matrix::colSums(log_tw) / n)

        tw * (log_tw) - word_factor
    }
}

#' @export
#' @rdname tw_smooth_normalize
tw_sievert_shirley <- function(m, lambda=0.6) {
    # score(t,v) = lambda log p(t,v) + (1 - lambda) log (p(t,v) / p(v))

    b <- hyperparameters(m)$beta

    function (tw) {
        V <- ncol(tw)

        # smooth + normalize weights
        topic_totals <- Matrix::rowSums(tw) + V * b
        tw <- tw + b

        pw <- Matrix::colSums(tw) / sum(tw)

        tw <- rescale_rows(tw, 1 / topic_totals)

        log_tw <- log(tw)

        lift <- log(rescale_cols(tw, 1 / pw))

        lambda * log_tw + (1 - lambda) * lift
    }
}

#' Read in a numeric matrix
#'
#' Since R does not supply a matrix-reading function, here's one.
#'
#' @return For \code{read_matrix_csv}, an ordinary matrix; for
#'   \code{read_Matrix_csv}, a \code{\link[Matrix]{sparseMatrix}}
#'
#' @param f CSV filename, for example \code{topic_words.csv}.
#'
#' @param what datatype to read in (passed on to \code{\link[base]{scan}}).
#'   \code{\link[base]{integer}()} by default; use \code{\link[base]{numeric}()}
#'   if the datafile has proportions.
#'
#' @export
#'
read_matrix_csv <- function (f, what=integer()) {
    m <- scan(f, what=what, sep=",")
    n <- length(scan(f, what=what, sep=",", nlines=1, quiet=TRUE))
    matrix(m, byrow=TRUE, ncol=n)
}

#' @export
#' @rdname read_matrix_csv
read_Matrix_csv <- function (f, what=integer()) {
    as(read_matrix_csv(f, what), "sparseMatrix")
}

#' Write out a numeric matrix to a text file
#'
#' Convenience function for saving numeric matrices as text files (not a
#' particularly space-efficient format).
#'
#' @param m matrix or Matrix (e.g. topic-words or document-topics)
#' @param f file connection to write to
#'
#' @export
write_matrix_csv <- function (m, f) {
    write.table(as.matrix(m), f, sep=",",
                row.names=FALSE, col.names=FALSE)
}

#' @export
#' @rdname write_matrix_csv
write_Matrix_csv <- write_matrix_csv

#' Normalizing the document-topic matrix
#'
#' This package assumes that \code{doc_topics(x)} is the "raw" sampled weights
#' of topics in documents. To represent the estimated probability of observing a
#' topic in a particular document, these values should be smoothed and
#' normalized. This function yields a \emph{function} which should in turn be
#' applied to \code{doc_topics(x)}. The idea is to minimize the possibility of
#' confusion over whether you are operating on smoothed weights or not.
#'
#' @param m \code{mallet_model} object
#' @return a function which operates on document-topic matrix. Smoothing means adding \eqn{alpha_k} to document weights for topic \eqn{k}, normalizing means ensuring each document has total weight 1.
#'
#' @seealso \code{\link{doc_topics}}, \code{\link[mallet]{mallet.doc.topics}}
#'
#' @examples \dontrun{dt_smooth_normalize(x)(doc_topics(x))}
#'
#' @export
dt_smooth_normalize <- function (m) {
    a <- hyperparameters(m)$alpha

    sm <- dt_smooth(m)
    function (dtm) {
        dtm <- sm(dtm)

        rescale_rows(dtm, 1 / rowSums(dtm))
    }
}

#' @export
#' @rdname dt_smooth_normalize
dt_smooth <- function (m) {
    a <- hyperparameters(m)$alpha

    function (dtm) {
        dtm + matrix(rep(a, each=nrow(dtm)), nrow=nrow(dtm))
    }
}

#' @export
#' @rdname dt_smooth_normalize
dt_normalize <- function (m) {
    function (dtm) rescale_rows(dtm, 1 / rowSums(dtm))
}


#' Utility functions for finding top-ranking row/column elements
#'
#' One often wants to know which are the largest n elements in each of the rows
#' or columns of a matrix. These functions extract the indices of these elements
#' (using naive ranking).
#'
#' @param m matrix
#' @param n number of elements to extract. Unlike dplyr's
#'   \code{\link[dplyr]{top_n}}, no account is taken here of the possibility
#'   that the \code{n}th element is tied with the \code{(n + 1)}th (etc).
#'
#' @return a \emph{two-column subscript matrix} with row indices in the first
#'   column and column indices in the second. This can be used as a single
#'   subscript to the input matrix \code{m} to yield a vector.
#'
#' @examples
#' m <- matrix(1:9, ncol=3)
#' ij_row <- top_n_row(m, 2)
#' ij_col <- top_n_col(m, 2)
#'
#' # note the resulting grouping by rows/cols
#' m[ij_row]
#' m[ij_col]
#' data.frame(rownum=ij_row[ , 1], value=m[ij_row])
#'
#' @export
#'
top_n_row <- function (m, n) {
    # TODO Rcpp
    stopifnot(n <= ncol(m))

    # apply pastes vectorial arguments back together as columns even when
    # applying over rows: genius!
    o <- apply(m, 1, order, decreasing=TRUE)
    i <- rep(1:nrow(m), each=n)

    matrix(c(i, as.numeric(o[1:n, ])), ncol=2)
}

#' @export
#' @rdname top_n_row
top_n_col <- function (m, n) {
    # TODO Rcpp
    stopifnot(n <= nrow(m))

    o <- apply(m, 2, order, decreasing=TRUE)
    j <- rep(1:ncol(m), each=n)

    matrix(c(as.numeric(o[1:n, ]), j), ncol=2)
}

#' Sum ragged groups of matrix rows or columns
#'
#' Given a matrix and a factor, yields a matrix where all rows
#' (\code{sum_row_groups}) or columns (\code{sum_col_groups}) corresponding to
#' the same factor level have been summed. This is just a convenience wrapper
#' for matrix multiplication. However, this is a frequent operation with models
#' of this kind (e.g. analyzing topic distributions over metadata categories),
#' and with the large matrices one often deals with in these cases, converting
#' to a data frame and using ordinary \code{tapply} or \pkg{dplyr} grouping can
#' be laborious.
#'
#' Note that ordinary \code{rowSums} corresponds to \code{sum_col_groups} with a
#' one-level factor, and conversely.
#'
#' @param m matrix (or Matrix)
#' @param f factor or something coercible to one. Must have as many elements as
#'   \code{m} has rows (for \code{sum_row_groups}) or columns (for
#'   \code{sum_col_groups}).
#' @param row_names used to name the rows of the result of
#'   \code{sum_row_groups}. By default, the levels of \code{f} are used. Set to
#'   NULL to blank these out
#' @param col_names Similarly, for \code{sum_col_groups}
#' @return a matrix (or Matrix) with the same number of columns (rows) as
#'   \code{m}, but rows (columns) corresponding to the same level of \code{f}
#'   summed. These rows (columns) are in the same order as \code{levels(f)}.
#'
#' @seealso \code{\link[stats]{model.matrix}} and
#'   \code{\link[Matrix]{sparse.model.matrix}}, which this function uses to
#'   transform the factor \code{f} into an indicator matrix to multiply by
#'
#' @export
sum_row_groups <- function (m, f, row_names=levels(f)) {
    result <- indicator_matrix(f, is(m, "Matrix"), transpose=TRUE) %*% m
    if (is.null(row_names) && is.null(colnames(m))) {
        result <- unname(result)
    } else {
        rownames(result) <- row_names
    }
    result
}

#' @export
#' @rdname sum_row_groups
#'
sum_col_groups <- function (m, f, col_names=levels(f)) {
    result <- m %*% indicator_matrix(f, is(m, "Matrix"))
    if (is.null(col_names) && is.null(rownames(m))) {
        result <- unname(result)
    } else {
        colnames(result) <- col_names
    }
    result
}

# utility function for converting a factor to an indicator matrix
#
# Has levels in columns factor values in rows, or the transpose if transpose is
# TRUE

indicator_matrix <- function (f, sparse, transpose=FALSE) {
    f <- as.factor(f)
    if (sparse) {
        result <- Matrix::sparse.model.matrix(~ f - 1,
            contrasts.arg=list(f="contr.treatment"),
            row.names=FALSE,
            transpose=transpose) # for efficiency (ha)
    } else {
        result <- model.matrix(~ f - 1,
            contrasts.arg=list(f="contr.treatment"))
        if (transpose) {
            result <- t(result)
        }
    }
    result
}


#' Transform a matrix into a long ("tidy") data frame
#'
#' Converts a matrix into a data frame with one row for each matrix entry and
#' three columns. This is the same idea as the \code{\link[tidyr]{gather}}
#' function from the \pkg{tidyr} package, but for the matrix special case. The
#' result is in row-major order by default. An ordinary matrix \eqn{m} is
#' unrolled into triplets \eqn{i, j, m_{ij}}. If row or column names are present
#' they are swapped in for the numeric indices, or you can supply these
#' directly.
#'
#' @param m matrix (or Matrix: but sparse matrices will be filled in)
#' @param row_values values corresponding to row indices. By default the row
#'   names of \code{m} are used, or, if these are missing, the row indices
#'   themselves.
#' @param col_values similarly, for columns.
#' @param col_names names for the columns of the resulting data frame
#' @param row_major unroll \code{m} row by row or column by column? By row is
#'   the default, though by column may be faster.
#'
#' @return a data frame with three columns and one row for every entry of
#'   \code{m}.
#'
#' @examples
#' m <- matrix(1:4, ncol=2, byrow=TRUE)
#' gather_matrix(m)
#'
#' @export
#'
gather_matrix <- function (m, row_values=rownames(m),
                           col_values=colnames(m),
                           col_names=c("row_key", "col_key", "value"),
                           row_major=TRUE) {
    if (is.null(row_values)) row_values <- seq(nrow(m))
    if (is.null(col_values)) col_values <- seq(ncol(m))

    stopifnot(length(row_values) == nrow(m))
    stopifnot(length(col_values) == ncol(m))
    stopifnot(length(col_names) == 3)

    if (row_major) {
        result <- data.frame(
            rkey=rep(row_values, each=ncol(m)),
            ckey=rep(col_values, times=nrow(m)),
            value=as.numeric(Matrix::t(m)),
            stringsAsFactors=FALSE
        )
    } else {
        result <- data.frame(
            rkey=rep(row_values, times=ncol(m)),
            ckey=rep(col_values, each=nrow(m)),
            value=as.numeric(m),
            stringsAsFactors=FALSE
        )
    }

    names(result) <- col_names
    result
}

