
#' Aggregate word counts by years
#' 
#' Aggregates word counts in a term-document matrix by document years.
#' 
#' @param metadata the metadata frame
#' @param tdm term-document sparseMatrix (from result of
#'   \code{\link{instances_term_document_matrix}})
#' @param instances supply a reference to an \code{InstanceList} and the routine
#'   will retrieve the id list and vocabulary automatically. Ignored if
#'   \code{id_map} and \code{vocabulary} are specified.
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#'   
#' @return A two-element list: \describe{ \item{\code{tym}}{the term-year-matrix
#'   (as \code{\link[Matrix]{sparseMatrix}})} \item{\code{yseq}}{a map from
#'   column indices to dates. Should be sequential but may not be evenly spaced
#'   if any year is missing from the data.} }
#'   
#' @seealso \code{\link{instances_term_document_matrix}} 
#' \code{\link{instances_ids}} \code{\link{instances_vocabulary}} 
#' \code{\link{term_year_topic_matrix}}
#' 
#' @export
#' 
term_year_matrix <- function(metadata,
                             tdm,
                             instances=NULL,
                             id_map=instances_ids(instances),
                             vocabulary=instances_vocabulary(instances)) {
    metadata <- metadata[metadata$id %in% id_map,]
    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    # years as indicator matrix
    Y <- sparse.model.matrix(~ years - 1)

    result <- tdm %*% Y

    list(tym=result,yseq=levels(years))
}

#' Convert a term-year matrix to a dataframe for plotting
#' 
#' This function makes a dataframe, suitable for plotting, of entries from a
#' term-year matrix (conditional on topic or not).
#' 
#' @param words which terms to pick out
#'   
#' @param term_year matrix with terms in rows and time-slices in columns
#'   
#' @param year_seq character vector mapping columns of \code{term_year} to
#'   dates, as strings in ISO format (the \code{yseq} component of results from
#'   \code{\link{term_year_matrix}} or \code{\link{term_year_topic_matrix}}
#' @param vocab character vector mapping rows of \code{term_year} to terms
#'   
#' @param raw_counts if FALSE (the default), divide counts through by column
#'   totals or \code{denominator}
#'   
#' @param total if TRUE, tally up the total incidences of all words in
#'   \code{words}
#'   
#' @param denominator \code{raw_counts} is FALSE, counts are normalized by this.
#'   If this is NULL, column totals are used. Use this parameter if you are
#'   passing in a \code{\link{term_year_topic_matrix}} but you still want the 
#'   yearly proportion out of all words in the corpus, in which case set
#'   \code{denominator=term_year_matrix()$tym}. The columns of 
#'   \code{denominator} are assumed to correspond to the same years as those of 
#'   \code{term_year}, so be careful of dropped years.
#'   
#' @return a dataframe with three columns, \code{word,year,weight}. If
#'   \code{total} is TRUE, this will have only one row.
#'   
#' @seealso \code{\link{term_year_topic_matrix}}, \code{\link{term_year_matrix}}
#' 
#' @export
#' 
term_year_series_frame <- function(words,term_year,year_seq,vocab,
                                   raw_counts=F,
                                   total=F,
                                   denominator=NULL) {
    w <- match(words,vocab)
    if(any(is.na(w))) {
        message("Dropping words missing from vocabulary: ",
                 paste(words[is.na(w)],collapse=" "))
        words <- words[!is.na(w)]
        w <- w[!is.na(w)]
     }

    # TODO use yearly_series_frame to factor this out
    wts <- term_year[w,,drop=F]
    if(!raw_counts) {
        if(is.null(denominator)) {
            denominator <- colSums(term_year)
        }
        wts <- wts %*% diag(1 / denominator) 
    }

    if (total) {
        wts <- matrix(colSums(wts),nrow=1)
        if(length(words) > 5) {
            words <- paste(words[1:5],collapse=" ")
            words <- paste('Total of "',words,'," etc.',sep="")
        } else {
            words <- paste(words,collapse=" ")
            words <- paste("Total of ",words,sep="")
        }
    }

    # TODO use yearly_series_frame to factor this out too
    rownames(wts) <- words
    colnames(wts) <- year_seq
    series <- melt(as.matrix(wts))
    names(series) <- c("word","year","weight")
    series$year <- as.Date(series$year)

    series
}

#' Derive a term-year counts conditioned on a journal
#' 
#' Convenience wrapper for \code{\link{term_year_matrix}}, summing counts only
#' for documents in a given journal
#' 
#' @param journal the journal, matched against \code{metadata$journaltitle} (so
#'   be careful about trailing tabs in JSTOR metadata fields)
#' @param metadata the metadata frame
#' @param the term-document matrix from
#'   \code{\link{instances_term_document_matrix}}
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#' @return a two-element \code{list(tym=...,yseq...)} as in
#'   \code{\link{term_year_matrix}}
#'   
#' @seealso \code{\link{instances_term_document_matrix}} 
#' \code{\link{instances_ids}} \code{\link{instances_vocabulary}} 
#' \code{\link{term_year_matrix}}
#' 
#' @export
#' 
term_year_matrix_journal <- function(journal,
                                     metadata,
                                     tdm,
                                     id_map,
                                     vocabulary) {

    metadata <- metadata[metadata$id %in% id_map,]
    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    jm <- Diagonal(n=ncol(tdm),x=(journals==journal))

    term_year_matrix(metadata,
                     tdm=tdm %*% jm,
                     id_map=id_map,
                     vocabulary=vocabulary,
                     big=T)
}

#' Sum a term-document matrix over journals and years
#' 
#' Calculates the total wordcounts per journal per year from a term-document
#' matrix. Can be usefully applied not just to the result of 
#' \code{\link{instances_term_document_matrix}} but also to a 
#' \code{\link{term_document_topic_matrix}}.
#' 
#' @param tdm matrix or \code{\link[Matrix]{sparseMatrix}} with terms in rows
#'   and documents in columns
#' @param metadata metadata frame
#' @param id_map character vector mapping \code{tdm} columns to
#'   \code{metadata$id} values
#' @return an ordinary matrix with journals in rows and years in columns; the 
#'   \code{\link[base]{rownames}} of the result give the \code{journaltitle}
#'   values and the \code{\link[base]{colnames}} give the dates as strings
#'   
#' @seealso
#' 
#' \code{\link{instances_term_document_matrix}}, 
#' \code{\link{term_document_topic_matrix}}, \code{\link{term_year_matrix}}, 
#' \code{\link{term_year_topic_matrix}}
#' 
#' @export
#' 
journal_year_matrix <- function(tdm,metadata,id_map) {
    metadata <- metadata[metadata$id %in% id_map,]

    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    Y <- Matrix(0,nrow=length(years),ncol=nlevels(years)) 
    Y[cbind(seq_along(years),years)] <- 1

    result <- matrix(nrow=nlevels(journals),ncol=nlevels(years))

    # The fully algebraic way would be to construct some block matrices
    # to do these sums, but since the number of journals is small, it's
    # not worth it

    Csum <- Matrix(rep(1,nrow(tdm)),nrow=1)

    for(i in seq_along(levels(journals))) {
        jrnl <- levels(journals)[i]

        jm <- Diagonal(n=ncol(tdm),x=(journals==jrnl))
        m <- tdm %*% jm
        tym <- m %*% Y

        result[i,] <- drop(Csum %*% tym)
    }

    rownames(result) <- levels(journals)
    colnames(result) <- levels(years)
    result
}

#' Calculate tf*idf scores
#'
#' Calculates tf*idf scores from a term-document \code{\link[Matrix]{sparseMatrix}}.
#'
#' May not be optimal for speed for calculating scores for the whole tdm.
#'
#' @param term numeric index into rows of \code{tdm} (can be a vector)
#' @param doc numeric index into columns of \code{tdm} (can be a vector)
#' @param tdm term-document \code{\link[Matrix]{sparseMatrix}}
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#'
#' @export
#'
tf_idf <- function(term,doc,tdm) {
    idf <- log(ncol(tdm) / rowSums(tdm[term, , drop=F] != 0))
    Diagonal(n=length(term), x=idf) %*% tdm[term,doc]
}



#' Normalize columns to sum to one
#' 
#' A convenience function for a frequent operation of normalizing the columns of
#' a matrix. The typical application in document modeling is to to ensure that
#' the columns sum to one (L1 normalization). Sometimes it is convenient instead
#' to set the columns to have a unit Euclidean norm (L2 normalization).
#' 
#' @param m a matrix or Matrix
#' @param norm Either \code{"L1"}, the default (the sum of the absolute value of
#'   terms), or \code{"L2"}, the Euclidean norm
#'   
#' @return the column-normalized matrix
#'   
#' @export
#' 
normalize_cols <- function (m, norm="L1") {
    if (is(m, "Matrix")) {
        dg <- function (x) Diagonal(x=x)
    } else {
        dg <- diag
    }

    if (norm == "L1") { 
        m %*% dg(1 / colSums(abs(m)))
    } else if (norm == "L2") {
        m %*% dg(1 / colSums(sqrt(m * m)))
    } else {
        stop("norm must be L1 or L2")
    }
}



#' Scoring methods for words in topics
#' 
#' The "raw" final sampling state of words in topics may be transformed into
#' either estimated probabilities or other kinds of salience scores. These
#' methods produce \emph{functions} that operate on a topic-word matrix. They
#' can be passed as the \code{weighting} parameter to \code{\link{top_words}}.
#' 
#' The basic method is to recast the sampled word counts as probabilities by
#' adding the estimated hyperparameter \eqn{\beta} and then normalizing rows so
#' they add to 1. This is equivalent to \code{\link[mallet]{mallet.topic.words}}
#' with \code{smooth} and \code{normalize} set to TRUE. Naturally this will not
#' change the relative ordering of words within topics.
#' 
#' A method that can re-rank words has been given by Blei and Lafferty: the
#' score for word \eqn{v} in topic \eqn{t} is \deqn{p(t,v)\textrm{log}(p(t,v) /
#' \prod_k p(k,v)^1/K)} where \eqn{K} is the number of topics. The score gives
#' more weight to words which are ranked highly in fewer topics.
#' 
#' Another method is the "relevance" score of Sievert and Shirley: in this case 
#' the score is given by \deqn{\lambda log (p(t,v) + (1 - \lambda) log (p(t,v) /
#' p(v)} where \eqn{\lambda} is a weighting parameter which is by default set to
#' 0.6 and which determines the amount by which words common in the whole corpus
#' are penalized.
#' 
#' @param x a \code{\link{dfr_lda}} object
#' @param l For \code{sievert_shirley}, the weighting parameter \eqn{\lambda},
#'   by default 0.6.
#' @return a function of one variable, to be applied to the topic-word sparse
#'   matrix.
#'   
#' @references D. Blei and J. Lafferty. Topic Models. In A. Srivastava and M.
#' Sahami, editors, \emph{Text Mining: Classification, Clustering, and
#' Applications}. Chapman & Hall/CRC Data Mining and Knowledge Discovery Series,
#' 2009. \url{http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf}.
#' 
#' C. Sievert and K.E. Shirley. LDAvis: A method for visualizing and
#' interpreting topics.
#' \url{http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf}.
#' 
#' @examples
#' \dontrun{top_words(x, n=10, weighting=tw_blei_lafferty(x))}
#' \dontrun{tw_smooth_normalize(x)(topic_words(x))}
#' 
#' @export
#' 
tw_smooth_normalize <- function (x) {
    b <- hyperparameters(x)$beta

    function (tw) {
        Diagonal(x=1 / (Matrix::rowSums(tw) + b * ncol(tw))) %*% (tw + b)
    }
}

#' @export
#' @rdname tw_smooth_normalize
tw_blei_lafferty <- function (x) {
    
    # score(t,v) = p(t,v) log (p(t,v) / Prod_k p(k,v) ^ 1 / K)
    #            = p(t,v) ( log p(t,v) - (1 / K) log( Prod_k p(k,v) ) )
    #            = p(t,v) ( log p(t,v) - (1 / K) Sum_k (log p(k,v) ) )

    sn <- tw_smooth_normalize(x)

    function (tw) {
        tw <- sn(tw)
        n <- nrow(tw)
        log_tw <- log(tw)

        # calculate down-weighting factor for each word.
        # for some unknown reason I seem to need to explicitly dispatch to
        # the Matrix method here
        word_factor <- tw %*% Diagonal(x=Matrix::colSums(log_tw) / n)

        tw * (log_tw) - word_factor
    }
}

#' @export
#' @rdname tw_smooth_normalize
tw_sievert_shirley <- function(x, lambda=0.6) {
    # score(t,v) = lambda log p(t,v) + (1 - lambda) log (p(t,v) / p(v))

    b <- hyperparameters(x)$beta
    
    function (tw) {
        V <- ncol(tw)

        # smooth + normalize weights
        topic_totals <- rowSums(tw) + V * b
        tw <- tw + b

        pw <- Matrix::colSums(tw) / sum(tw)

        tw <- Diagonal(x=1 / topic_totals) %*% tw
        
        log_tw <- log(tw)

        # TODO not sure this works right
        lambda * log_tw + (1 - lambda) * t(apply(log_tw, 1, '/', pw)) 
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
    n <- length(scan(f, what=what, sep=",", nlines=1, quiet=T))
    matrix(m, byrow=T, ncol=n)
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
                row.names=F, col.names=F) 
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
#' @param x \code{dfr_lda} object
#' @return a function which operates on document-topic matrix
#'   
#' @seealso \code{\link{doc_topics}}, \code{\link[mallet]{mallet.doc.topics}}
#'   
#' @examples \dontrun{dt_smooth_normalize(x)(doc_topics(x))}
#' 
#' @export
dt_smooth_normalize <- function (x) {
    a <- hyperparameters(x)$alpha

    function (m) {
        m <- m + matrix(rep(a, each=nrow(m)), nrow=nrow(m))

        diag(1 / rowSums(m)) %*% m
    }
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

    # apply pastes vectorial arguments back together as columns even when
    # applying over rows: genius!
    o <- apply(m, 1, order, decreasing=T)
    i <- rep(1:nrow(m), each=n)

    matrix(c(i, as.numeric(o[1:n, ])), ncol=2)
}

#' @export
#' @rdname top_n_row
top_n_col <- function (m, n) {
    # TODO Rcpp
    o <- apply(m, 2, order, decreasing=T)
    j <- rep(1:ncol(m), each=n)

    matrix(c(as.numeric(o[1:n, ]), j), ncol=2)
}
