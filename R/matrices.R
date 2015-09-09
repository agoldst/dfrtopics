
#' Aggregate word counts by years
#'
#' Aggregates word counts in a term-document matrix by document years.
#'
#' @param metadata the metadata frame
#' @param tdm term-document sparseMatrix (from result of \code{\link{instances_term_document_matrix}})
#' @param instances supply a reference to an \code{InstanceList} and the routine will 
#' retrieve the id list and vocabulary automatically. Ignored if \code{id_map} and 
#' \code{vocabulary} are specified.
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#'
#' @return A two-element list: \describe{
#'      \item{\code{tym}}{the term-year-matrix (as \code{\link[Matrix]{sparseMatrix}})} 
#' \item{\code{yseq}}{a map from column indices to dates. Should be
#' sequential but may not be evenly spaced if any year
#' is missing from the data.}
#' }
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#' \code{\link{instances_ids}}
#' \code{\link{instances_vocabulary}}
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
#' This function makes a dataframe, suitable for plotting, of entries
#' from a term-year matrix (conditional on topic or not).
#'
#' @param words which terms to pick out
#'
#' @param term_year matrix with terms in rows and time-slices in columns
#'
#' @param year_seq character vector mapping columns of \code{term_year}
#' to dates, as strings in ISO format (the \code{yseq}
#' component of results from \code{\link{term_year_matrix}} or
#' \code{\link{term_year_topic_matrix}}
#' @param vocab character vector mapping rows of \code{term_year} to terms
#'
#' @param raw_counts if FALSE (the default), divide counts through by column totals or 
#' \code{denominator}
#' 
#' @param total if TRUE, tally up the total incidences of all words in \code{words}
#'
#' @param denominator \code{raw_counts} is FALSE, counts are normalized by this. If this 
#' is NULL, column totals are used. Use this parameter if you
#' are passing in a \code{\link{term_year_topic_matrix}} but you still want the
#' yearly proportion out of all words in the corpus, in which case
#' set \code{denominator=term_year_matrix()$tym}. The columns of
#' \code{denominator} are assumed to correspond to the same years as those of 
#' \code{term_year}, so be careful of dropped years.
#'
#' @return a dataframe with three columns, \code{word,year,weight}. If \code{total} is 
#' TRUE, this will have only one row.
#'
#' @seealso
#' \code{\link{term_year_topic_matrix}},
#' \code{\link{term_year_matrix}}
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
#' Convenience wrapper for \code{\link{term_year_matrix}}, summing counts 
#' only for documents in a given journal
#'
#' @param journal the journal, matched against \code{metadata$journaltitle} (so be careful 
#' about trailing tabs in JSTOR metadata fields)
#' @param metadata the metadata frame
#' @param the term-document matrix from \code{\link{instances_term_document_matrix}}
#' @param id_map character vector mapping \code{tdm} columns to documents
#' @param vocabulary character vector mapping \code{tdm} rows to terms
#' @return a two-element \code{list(tym=...,yseq...)} as in \code{\link{term_year_matrix}}
#'
#' @seealso
#' \code{\link{instances_term_document_matrix}}
#' \code{\link{instances_ids}}
#' \code{\link{instances_vocabulary}}
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
#' Calculates the total wordcounts per journal per year from a term-document matrix. Can 
#' be usefully applied not just to the result of 
#' \code{\link{instances_term_document_matrix}} but also to a 
#' \code{\link{term_document_topic_matrix}}.
#'
#' @param tdm matrix or \code{\link[Matrix]{sparseMatrix}} with terms in rows and documents 
#' in columns
#' @param metadata metadata frame
#' @param id_map character vector mapping \code{tdm} columns to \code{metadata$id} values
#' @return an ordinary matrix with journals in rows and years in columns; the 
#' \code{\link[base]{rownames}} of the result give the \code{journaltitle} values and the 
#' \code{\link[base]{colnames}} give the dates as strings
#'
#' @seealso
#'
#' \code{\link{instances_term_document_matrix}},
#' \code{\link{term_document_topic_matrix}},
#' \code{\link{term_year_matrix}},
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
#' A convenience function for a frequent operation of normalizing the columns of a matrix. The typical application in document modeling is to to ensure that the columns sum to one (L1 normalization). Sometimes it is convenient instead to set the columns to have a unit Euclidean norm (L2 normalization).
#'
#' @param m a matrix or Matrix
#' @param norm Either \code{"L1"}, the default (the sum of the absolute value of terms), or \code{"L2"}, the Euclidean norm
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
