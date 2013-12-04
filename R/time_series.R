# Functions for calculating yearly time series of topic weights from the document-topic 
# matrix and metadata.

#' The yearly totals of topic weights
#'
#' Tots up the total of each topic for each year, given documents and date metadata
#'
#' Either a long-form or a
#' wide-form data frame may be passed in; the wide form can be handled much
#' (orders of magnitude) faster. Formerly called \code{tm_yearly_totals}, but that's silly.
#'
#' @param dt_wide doc_topics frame with attached metadata from 
#' \code{\link{doc_topics_wide}}
#' @param dt_long doc_topics frame with attached metadata from 
#' \code{\link{doc_topics_long}}
#' @return a matrix with: rows containing topic totals, columns
#' containing years present in the data, colnames with strings
#' representing dates.
#'
#' @export
#'
#' @seealso
#' \code{\link{doc_topics_wide}},
#' \code{\link{doc_topics_long}}
#'
topic_year_matrix <- function(dt_wide=NULL,dt_long=NULL) {
    if(!is.null(dt_wide)) {
        dt_wide$pubdate <- cut(pubdate_Date(dt_wide$pubdate),breaks="years")
        dt_wide$pubdate <- droplevels(dt_wide$pubdate)
        dt_wide$id <- NULL

        # Here, assume that the wide matrix has topic scores in all but
        # the last column, which holds the pubdate. daply will stick the
        # pubdate back on the front when it splits the frame by years.
        # The result will have topics in columns, so transpose.

        topic_sum <- function (d) {
            colSums(subset(d,select=-pubdate))
        }
        t(daply(dt_wide,"pubdate",topic_sum))
    }
    else if(!is.null(dt_long)) {
        # copy on modify
        dt_long$pubdate <- cut(pubdate_Date(dt_long$pubdate),breaks="years")
        dt_long$pubdate <- droplevels(dt_long$pubdate)
        dt_long$id <- NULL
        totals <- ddply(dt_long,c("variable","pubdate"),summarize,
                        total=sum(value))
        acast(totals,variable ~ pubdate,value.var="total")
    }
    else {
        stop("Supply either long or wide-form document-topic matrix")
    }
} 

#' Get a dataframe suitable for plotting time series of topics
#'
#' Produces a dataframe suitable for plotting time series of topics on the basis of the 
#' yearly totals of topics.
#'
#' @param yearly the topic-year matrix, with dates as colnames
#' @param topics which topics to keep in the dataframe: by default, all are retained
#' @param denominator the totals to divide the yearly series by. By default, the sums of 
#' the columns of \code{yearly} are used, so that the resulting dataframe gives the 
#' proportion of \emph{words} in the corpus for that year assigned to the topic in 
#' question. 
#' @param rolling_window the number of years to take rolling averages over (default 1)
#' @return a dataframe with columns \code{topic,year,weight}
#' @export
#'
topic_proportions_series_frame <- function(yearly,
                                           topics=1:nrow(yearly),
                                           denominator=colSums(yearly),
                                           rolling_window=1) {
    yseq <- colnames(yearly)

    z <- series_rolling(series=yearly_zoo(yearly[topics,,drop=F]),
                       totals=zoo(denominator,as.Date(yseq)),
                       k=rolling_window)

    yearly_series_frame(yearly=zoo_yearly(z),
                        var_seq=topics,
                        series_names=c("topic","year","weight"))

}

#' Convert a matrix of time series to a dataframe
#'
#' Utility wrapper for \code{\link[reshape2]{melt}} on matrices with parallel time series in 
#' rows.
#'
#' @param yearly matrix with variables in rows and time measurements in columns
#' @param total if TRUE, sum over rows to produce a single total series
#' @param year_seq character vector giving the dates the columns of \code{yearly} 
#' correspond to (by default, assume these are stored in \code{\link{colnames}(yearly)})
#' @param var_seq character vector giving names of variables in rows of \code{yearly}
#' @param series_names column names for final dataframe
#' @return three-column data frame ("long" form)
#' @seealso TODO vis,
#' \code{\link{topic_proportions_series_frame}}
#'
#' @export
#'
yearly_series_frame <- function(yearly,
                                total=F,
                                year_seq=colnames(yearly),
                                var_seq=rownames(yearly),
                                series_names=c("word","year","weight")) {

    if (total) {
        yearly <- matrix(colSums(yearly),nrow=1)
        if(length(var_seq) != 1) {
            message('Name for total not well-specified; using "total"')
            var_seq <- "total"
        }
    }

    rownames(yearly) <- var_seq
    colnames(yearly) <- year_seq
    series <- melt(as.matrix(yearly)) # Force conversion from any sparseMatrix
    names(series) <- series_names
    series$year <- as.Date(series[,2])

    series
}

#' Convert a yearly series data frame into a zoo object
#'
#' Utility function for swapping between the long form dataframe of 
#' \code{\link{yearly_series_frame}} and a \pkg{zoo} object.
#'
#' @param s three-column data frame
#' @param date_col index of date column (default 2)
#' @param value_col index of value column (default 3)
#' @return a \code{zoo} time series object
#'
#' @seealso
#' \code{\link{yearly_series_frame}}, \pkg{zoo}
#'
#' @export
#'
series_frame_zoo <- function(s,date_col=2,value_col=3) {
    val_var <- names(s)[value_col]
    date_var <- names(s)[date_col]
    category_var <- names(s)[-c(value_col,date_col)]

    s_m <- acast(s,as.formula(paste(category_var,"~",date_var)),
                 value.var=val_var)
    
    zoo(t(s_m),as.Date(colnames(s_m)))
}

#' Convert a matrix of yearly values into a zoo object
#'
#' @param yearly a matrix with variables in rows and dates in string form in its 
#' \code{colnames}
#' @return a \code{\link[zoo]{zoo}} object
#'
#' @seealso \code{\link{zoo_yearly}}
yearly_zoo <- function(yearly) {
    zoo(t(yearly),as.Date(colnames(yearly)))
}

#' Convert a zoo object into a matrix with variables in rows
#'
#' @param z a \code{\link[zoo]{zoo}} object
#' @return a matrix with variables in rows and dates in string form in its 
#' \code{colnames}
#'
#' @seealso \code{\link{yearly_zoo}}
#'
#' @export
zoo_yearly <- function(z) {
    as.matrix(t(z))
}

#' Convert a zoo object into a "long" data frame
#'
#' @param z a \code{\link[zoo]{zoo}} object
#' @param series_names the names for the three columns of the result
#' @return the data frame from \code{\link{yearly_series_frame}}
#' @seealso \code{\link{yearly_series_frame}},
#' \code{\link{zoo_yearly}}
#'
#' @export
zoo_series_frame <- function(z,series_names=c("word","year","weight")) {
    yearly_series_frame(zoo_yearly(z),series_names=series_names)
}

#' Take rolling weighted averages
#'
#' This takes rolling averages where adjacent time-slices are not given equal weights.
#'
#' @param the series (a \code{\link[zoo]{zoo}} object, which can contain one or many 
#' variables)
#' @param totals the denominators used in weighting (must be parallel to \code{series})
#' @param k the window to take the rolling average in
#' @return a \code{\link[zoo]{zoo}} object with the averaged values
#'
#' @seealso \code{\link[zoo]{rollapply}}
series_rolling <- function(series,totals,k) {
    rollapply(series,k,sum) / rollapply(totals,k,sum)
}

#' Tally up document-topic proportions, conditional on metadata
#'
#' Like \code{\link{topic_year_matrix}}, this tallies up document-topic proportions over #' yearly time-slices but allows you 
#' to split out the yearly totals by, e.g., \code{journaltitle}
#'
#' @param doctops the document-topic data frame with id column (from 
#' \code{\link{doc_topics_frame}}
#'
#' @param metadata the metadata frame, or a subset of its columns
#'
#' @param topic_year the result of \code{\link{topic_year_matrix}}, used for normalizing 
#' within each year. Pass \code{NULL} if you want raw counts instead.
#'
#' @param vars metadata columns to split by; by default, use all supplied metadata columns
#'
#' @return a data frame suitable for plotting, where each row gives yearly 
#' totals for each topic for a given metadata combination. The topic 
#' proportion columns are called \code{topic1}, \code{topic2}, etc.
#'
#' @export
#' @seealso \code{\link{topic_year_matrix}}
#'
topic_year_meta <- function(doctops,metadata,
                            topic_year=NULL,vars=NULL) { 
    if(is.null(vars)) {
        vars <- names(metadata)
    }
    vars <- unique(c("id","pubdate",vars))
    doctops <- merge(doctops,metadata[,vars],by="id")
    doctops$pubdate <- cut(pubdate_Date(doctops$pubdate),breaks="years")
    doctops$pubdate <- droplevels(doctops$pubdate)
    doctops$id <- NULL
    vars <- vars[vars != "id"]

    drop_cols <- match(vars,names(doctops))

    tally <- function (d) {
        colSums(d[,-drop_cols])
    }

    if(is.null(topic_year)) {
        ply_fun <- tally
    }
    else {
        ply_fun <- function(d) {
            yr_col <- match(d$pubdate[1],colnames(topic_year))
            tally(d) / sum(topic_year[,yr_col])
        }
    }

    ddply(doctops,vars,.fun=ply_fun)
}
