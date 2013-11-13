#' Overview visualization of multiple topics
#'
#' This function is intended for surveying a topic model. It generates three plots for 
#' each topic and saves the three plots in a single PNG file.
#'
#' For each topic, the PNG topic has three plots:
#' \enumerate{
#' \item{The weights of the
#' most probable words in each topic.}
#' \item{Boxplots of the distributions of
#' document-topic counts/proportions within short time-slices.}
#' \item{The yearly proportion of words/documents in the topic.}
#' }
#' The meaning of the latter two plots depends on whether the doc-topic
#' scores are normalized or not. 
#'
#' The plots are saved to \code{output_dir/<NNN>.png}. If you have more than 1000 topics, 
#' other parts of this package will probably break too.
#'
#' @param doctopics document-topic frame from \code{\link{doc_topics_frame}}
#' @param wkf topic key words frame from \code{\link{weighted_keys_frame}}
#' @param metadata metadata frame from \code{\link{read_metadata}}
#'
#' @param topic: a vector of topics to generate plots for (by default, all)
#'
#' @param time_breaks time intervals by which to slice document-topic distributions for 
#' the box plots: passed on to \code{\link{topic_time_boxplots}}.
#'
#' @param log-scale if TRUE, show topic proportions on a log scale. For
#' unsmoothed models, set to FALSE so as not to take \code{log(0)}.
#'
#' @param output_dir directory to save plots to
#'
#' @param w PNG width (pixels)
#' @param h PNG height (pixels)
#'
#' @seealso
#' \code{\link{topic_keyword_plot}},
#' \code{\link{topic_time_boxplots}},
#' \code{\link{topic_yearly_lineplot}}
#' 
#' @export
#'
topic_report <- function(doctopics,wkf,metadata,
                         topics=1:(ncol(doctopics - 1)),
                         time_breaks="5 years",
                         log_scale=F,
                         output_dir="topic_report",
                         w=1200,h=800) {

    dt_wide <- doc_topics_wide(doctops,metadata,meta_keep="pubdate")
    dt_long <- doc_topics_long(doctops,metadata,meta_keep="pubdate")

    # scale so that keyword weight bars are appropriately colored
    color_scale <- scale_color_gradient(limits=range(wkf$alpha))

    # only calculate yearly totals once for all topics

    yearly <- tm_yearly_totals(tm_wide=dt_wide)

    for(topic in topics) {
        filename <- file.path(output_dir,
                              sprintf("%03d.png",topic))
        png(file=filename,width=w,height=h)
        message("Saving ",filename)
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(2,2)))

        print(topic_keyword_plot(wkf,topic,color_scale),
              vp=viewport(layout.pos.row=c(1,2),layout.pos.col=1))

        print(tm_yearly_line_plot(.yearly_totals=yearly,
                                  topics=topic,raw_counts=raw_counts),
              vp=viewport(layout.pos.row=1,layout.pos.col=2))

        print(tm_time_boxplots(subset(dt_long,
                                      variable==paste("topic",topic,
                                                      sep="")),
                               time_breaks=time_breaks,
                               log_scale=log_scale),
              vp=viewport(layout.pos.row=2,layout.pos.col=2)) 
        
        dev.off()
    }
}

#' topic_keyword_plot
#'
#' Plot a single topic's most probable words.
#'
#' wkf: weighted keys frame; topic: a topic number from 1.
#'
#' color_scale: specify if you are making many plots and want coloration
#' by alpha.

topic_keyword_plot <- function(wkf,topic,
                               color_scale=scale_color_gradient()) {
    if(length(topic) > 1) {
        stop("Can only plot a single topic's keywords at once")
    }

    keys <- wkf[wkf$topic==topic,]
    ordered_words <- keys$word[order(keys$weight,decreasing=T)]
    plot_title=sprintf("Top words in topic %03d %s\na=%.3f",
                        topic,paste(ordered_words[1:3],collapse=" "),
                        keys$alpha[1])
    
    keys$sort_order <- with(keys,order(topic,-weight))
    p <- ggplot(keys) 
    p <- p + 
        geom_segment(aes(x=0,xend=weight,
                         y=word,
                         yend=word,
                         color=alpha),
                     size=2) +
        color_scale

    #if(length(topic) > 1) {
    #    p <- p + facet_wrap(~ topic_label,scale="free")
    #} else


    p <- p + scale_y_discrete(limits=rev(ordered_words))
    p <- p + theme(axis.title.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   axis.text.y=element_text(color="black",size=10),
                   legend.position="none") +
        xlab("weight in topic") +
        ggtitle(plot_title)
            
    p
}

#' corpus_dist_plot
#'
#' a quick way to visualize a useful diagnostic calculated by mallet:
#' the KL divergence between a topic and the corpus itself. These is
#' calculated by mallet's diagnostics output. Use read_diagnostics() to
#' get dataframes from the XML. Or you *could* calculate it yourself if
#' you insist.
#'
#' topic_diagnostics: a dataframe
#'
#' wkf: the weighted keys frame (for naming topics)
#'
#' Pass in subsets of these by topics if you wish to plot only some
#' topics. > 100 topics makes the labels hard to fit in a vertical stack.

corpus_dist_plot <- function(topic_diagnostics,wkf) {
    topic_order <- order(topic_diagnostics$corpus_dist,decreasing=T)
    to_plot <- data.frame(topic=topic_names(wkf),
                          distance=as.numeric(topic_diagnostics$corpus_dist))

    p <- ggplot(to_plot)
    p <- p + 
        geom_segment(aes(x=0,xend=distance,
                         y=topic,
                         yend=topic),
                     size=2)

    p <- p + scale_y_discrete(limits=to_plot$topic[topic_order])
    p <- p + theme(axis.title.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   axis.text.y=element_text(color="black",size=10),
                   legend.position="none") +
        xlab("KL divergence from corpus") +
        ggtitle("Distances of topics from corpus")

    p
}



#' tm_yearly_line_plot
#'
#' plot yearly averages. Supply a long or wide form data frame with
#' document- topic scores. Alternatively, you can precalculate the yearly
#' totals (using tm_yearly_totals() and supply them as .yearly_totals).
#'
#' topics: which topics to consider, as a vector of numbers from 1
#'
#' raw_counts: are topic scores word counts or estimated proportions?
#' Does not affect the actual plot, but if the topic scores have been
#' normalized then we are looking at frequency of the topic in documents
#' rather than freq. of the topic in words, so the title of the plot is
#' changed accordingly
#'
#' facet: faceted plot or multiple lines on one plot? If yes, you can use
#' the .faceting parameter to tweak the facet by passing a facet_wrap()
#' call
#'
#' .yearly_overall: if the denominator for a yearly average is not simply
#' the sum of all the entries (because you are supplying a subset of
#' the full topic matrix) you can supply this parameter


tm_yearly_line_plot <- function(tm_long=NULL,tm_wide=NULL,
                                topics=NULL,raw_counts=T,facet=F,
                                smoothing_line=F,
                                .yearly_totals=NULL,
                                .yearly_overall=NULL,
                                .faceting=facet_wrap(~ topic),
                                tnames=NULL) {
    if(!is.null(.yearly_totals)) {
        series <- .yearly_totals
    } else if(!is.null(tm_long)) {
        series <- tm_yearly_totals(tm_long=tm_long)
    } else if(!is.null(tm_wide)) {
        series <- tm_yearly_totals(tm_wide=tm_wide)
    } else {
        stop("Supply long, wide, or pre-aggregated document-topic matrix")
    }

    plot_title <- ifelse(raw_counts,
                         "Proportion of words in topic",
                         "Proportion of documents in topic")

    dates <- colnames(series)

    if(is.null(.yearly_overall)) { 
        yearly_overall <- colSums(series)
    }
    else {
        yearly_overall <- .yearly_overall
    }
    series <- series %*% diag(1 / yearly_overall) 
    colnames(series) <- dates

    # keep just the specified topics; if none specified, do all
    if(is.null(topics)) {
        topics <- 1:nrow(series)
    }

    tlabels <- paste("topic",topics,sep="")
    to.plot <- melt(series[tlabels,])

    if(length(topics) == 1) {
        to.plot$pubdate <- as.Date(rownames(to.plot))
        result <- ggplot(to.plot,aes(pubdate,value,group=1))
        result <- result + geom_line()

        plot_title <- paste(plot_title,topics)

        if(facet) {
            warning("Ignoring facet=TRUE for single topic")
        }

        if(smoothing_line) {
            result <- result + geom_smooth(method="loess")
        }
    }
    else { 
        to.plot <- rename(to.plot,c("Var1"="topic"))
        tnums <- as.character(to.plot$topic)
        tnums <- as.integer(substr(tnums,6,nchar(tnums)))

        if(!is.null(tnames)) {
            to.plot$topic <- tnames[tnums]
        } else { 
            to.plot$topic <- sprintf("%03d",tnums)
        }
        result <- ggplot(to.plot,aes(as.Date(Var2),value,group=topic))

        plot_title <- paste(plot_title,"s",sep="")

        if(facet) { 
            result <- result + geom_line() + .faceting
            if(smoothing_line) {
                result <- result + geom_smooth(method="loess")
            }
        }
        else  {
            result <- result + geom_line(aes(color=topic))
            if(smoothing_line) {
                result <- result + geom_smooth(aes(color=topic),
                                               method="loess")
            }
        }
    }

    result <- result + xlab("publication year") +
        ylab("proportion of year's words") +
        ggtitle(plot_title)

    result
}

tm_time_averages_plot <- function(topics,yearly_matrix,
                                  denominator=NULL,
                                  years=5,
                                  facet=F) {
    series <- topic_proportions_series_frame(yearly=yearly_matrix,
                                             topics=topics,
                                             denominator=denominator,
                                             rolling_window=years)

    series$topic <- sprintf("%03d",series$topic)
    
    result <- ggplot(series,aes(year,weight))

    if(length(topics) > 1) {
        if(facet) {
            result <- result + geom_line(aes(group=1)) + facet_wrap(~ topic)
        }
        else {
            result <- result + geom_line(aes(group=topic,color=topic))
        }
    }
    else {
        result <- result + geom_line(aes(group=1)) +
            theme(legend.position="none")
    }

    result +
        ylab("overall topic proportion") +
        ggtitle(paste("Topic proportion (moving intervals of",
                      years,"years)"))
}

#' tm_time_boxplots
#'
#' tm_long: a doc-topics frame with merged-in pubdate metadata
#'
#' time_breaks: intervals in which to plot doc-topic distributions
#'
#' log_scale: set to F if there are zeroes in the doc-topic proportions.

tm_time_boxplots <- function(tm_long,time_breaks="5 years",log_scale=T) {
    tm_long$date_cut <- cut(pubdate_Date(tm_long$pubdate),time_breaks)

    result <- ggplot(tm_long,aes(x=as.Date(date_cut),y=value,group=date_cut))
    result <- result +
        geom_boxplot() +
        geom_smooth(aes(x=pubdate_Date(pubdate),y=value,group=1),
                    method="auto")

    plot_title <- "Doc-Topic distributions"
    if(log_scale) {
        result <- result +
            scale_y_continuous(trans=log_trans())

        plot_title <- paste(plot_title,"(log scale)")
    }

    if(length(unique(tm_long$variable)) > 1) {
        result <- result + facet_wrap(~ variable)
    }

    result +
        xlab(paste("date (intervals of ",time_breaks,")",sep="")) +
        ylab("document topic proportions") +
        ggtitle(plot_title)
}

#' tm_yearly_journals_plot
#'
#' Plot a topic's yearly average in individual journals in an area plot
#'
#' doctops,metadata,yearly_overall: as in tm_yearly_totals_meta
#'
#' or pass .yrly_j, a dataframe of yearly totals by journal and topic

tm_yearly_journals_plot <- function(topic,
                                    doctops=NULL,
                                    metadata=NULL,
                                    yearly_overall=NULL,
                                    .yrly_j=NULL) { 
    if(is.null(.yrly_j)) {
        yrly_j <- tm_yearly_totals_meta(doctops,metadata,yearly_overall,
                                        vars="journaltitle")
    } else {
        yrly_j <- .yrly_j
    }

    topic_name <- paste("topic",topic,sep="")
    ggplot(yrly_j,aes_string(x="as.Date(pubdate)",
                             y=topic_name,
                             group="journaltitle",
                             fill="journaltitle")) +
        geom_area() +
        ggtitle(paste("Proportion of words in topic",topic))
}



#' ----------------
#' Individual words
#' ----------------

#' mallet_word_plot
#'
#' The MALLET 1-gram viewer! Also useful in conjunction with topic
#' frequencies over time. Compare tm_yearly_line_plot(topic,...) with
#' the results of mallet_word_plot(topic_top_words(topic,n=50,...)) to
#' discover whether corpus frequencies and topic frequencies diverge
#' (which may or may not be significant!)
#'
#' words: a vector of words
#'
#' term_year: the term_year_matrix
#'
#' year_seq: the year sequence corresponding to columns in the
#' term_year_matrix. Expected to be a factor or vector of ISO dates.
#'
#' the vocabulary corresponding to rows of the term_year_matrix
#'
#' plot_freq: plot raw counts or yearly ratios?
#'
#' smoothing: add a smoothing line to the plot?
#'
#' gg_only: if T, don't add geoms to plot object (so the caller can do it
#' instead)

mallet_word_plot <- function(words,term_year,year_seq,vocab,
                             plot_freq=T,
                             plot_total=F,
                             smoothing=F,
                             gg_only=F) {
    words <- words[words %in% vocab]

    series <- term_year_series_frame(words,term_year,year_seq,vocab,
                                     raw_counts=!plot_freq,
                                     total=plot_total)

    if(length(words) > 1 & !plot_total) { 
        result <- ggplot(series,aes(year,weight,color=word,group=word))
        plot_title <- "Words over time (filtered corpus)"
    }
    else {
        # Otherwise, we are just plotting the one time series
        # and the label has been stuck in the single "word" entry
        result <- ggplot(series,aes(year,weight,group=1))
        plot_title <- paste(series$word,' over time (filtered corpus)',sep="")
    }

    if(!gg_only) {
        result <- result + geom_line()
        if(smoothing) {
            result <- result + geom_smooth()
        }
    }

    result +
        ylab(ifelse(plot_freq,"yearly word frequency",
                    "yearly word count")) +
        ggtitle(plot_title)
}

#' words_topic_yearly_plot
#'
#' Given the results of term_year_topic_matrix, make a line plot showing
#' the occurrence of individual words IN A TOPIC over time. Whether you
#' plot counts or ratios, this is not the same as the corpus frequency of
#' the individual words; this is the time track of words assigned to the
#' given topic.
#'
#' words: a vector of words
#'
#' topic_desc: a short label for the topic, to go in the plot title
#'
#' tytm: the sparse matrix returned in the results of
#' term_year_topic_matrix
#'
#' yseq: the years corresponding to the columns of the tytm (also in
#' results of term_year_topic_matrix)
#'
#' vocab: the mallet vocabulary, corresponding to the rows of the tytm

words_topic_yearly_plot <- function(words,topic_desc,
                                    tytm,yseq,vocab,...) {
    result <- mallet_word_plot(words=words,
                               term_year=tytm,
                               year_seq=yseq,
                               vocab=vocab,
                               ...)
    plot_title <- ifelse(length(words)==1,words,"Words")
    plot_title <- paste(plot_title,"in\n",topic_desc)
    result + ggtitle(plot_title)
}

#' words_topic_yearly_plot_overall
#'
#' if you want the occurrence of the top words for a topic IN THE CORPUS,
#' you can use this convenience function to pass topic_top_words to
#' mallet_word_plot
#'
#' n: number of top words
#'
#' n = 0 to instead accept the default threshold for topic top words

words_topic_yearly_plot_overall <- function(topic,wkf,n,...) {
    words <- topic_top_words(topic,wkf,n)
    mallet_word_plot(words,...)
}

#' topic_dist_plot
#'
#' Gives a sense of the "closeness" of topics to one another
#'
#' More precisely, the strategy is to take the Jensen-Shannon divergence among 
#' the topics considered as distributions over words, and then use 
#' multidimensional scaling (i.e. PCA) to reduce these distances in word-
#' distribution space to distances in R^2.
#'
#' twm: matrix with topics in rows and word counts in columns
#' b: beta (used to smooth the counts)
#' wkf: weighted keys frame (for labeling)
#'
#' actually, nothing stops you setting twm = topic-document matrix and b = 
#' vector of alphas. That gives the distances among topics as distributions over 
#' documents.

topic_dist_plot <- function(twm,b,wkf) {
    divs <- topic_divergences(twm,b)
    dists <- cmdscale(divs,k=2)
    to_plot <- data.frame(label=topic_names(wkf),x=dists[,1],y=dists[,2])
    ggplot(to_plot) + geom_text(aes(x=x,y=y,label=label,hjust=1)) +
        scale_x_continuous(expand=c(.1,.1)) + # a little extra horizontal air 
        theme(axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
}

    
#' ---------------
#' About documents
#' ---------------

#' Return a frame with ids and weights with the "top" documents for a topic
#'
#' method: the notion of a "top" document is not well-specified.
#'       "raw":  maximum scores in the topic-column of the dtm.
#'       "max_frac": maximum after normalizing the topic-column of the dtm. A 
#'       topic may reach its maximum proportion in a document and yet that 
#'       document may yet have a larger proportion of another topic.

top_documents <- function(topic,id_map,dtm,n=5,method="raw") {
    if(method=="raw") {
        doc_scores <- dtm[,topic]
    } else if(method=="max_frac") {
        doc_scores <- dtm[,topic] / rowSums(dtm)
    } else {
        stop("Unknown method.")
    }

    indices <- order(doc_scores,decreasing=T)[1:n]
    
    ids <- id_map[indices]
    wts <- doc_scores[indices]

    data.frame(id=ids,weight=wts)
}

#' This one means different things, depending on whether dtm is
#' normalized per topic. If dtm is raw counts, one gets the topics that
#' have been assigned the largest number of words in a document. But
#' if dtm is column-normalized, then one gets the topics for which the
#' document is comparatively most prominent within that topic.

top_topics <- function(id,id_map,dtm,n=5) {
    i <- match(id,id_map)
    indices <- order(dtm[i,],decreasing=T)[1:n]

    data.frame(topic=indices,weight=dtm[i,indices])
}

#' ------------
#' About topics
#' ------------

topic_name <- function(topic,wkf,n=0,threshold=0.5,
                       name_format="%03d %s") {
    words <- topic_top_words(topic,wkf,n,threshold)

    words_str <- paste(words, collapse=" ")

    sprintf(name_format,topic,words_str)
}

#' or the above applied to many topics at once
topic_names <- function(wkf,n=2,topics=NULL,
                        name_format="%03d %s") {
    if(length(topics) == 0) {
        topics <- 1:length(unique(wkf$topic))
    }
    ws <- lapply(topics,topic_name,wkf=wkf,n=n,
                 name_format=name_format)
    sapply(ws,paste,collapse=" ")
}

topic_top_words <- function(topic,wkf,n=0,threshold=0.5) {
    wkf <- wkf[wkf$topic==topic,]
    if(n <= 0) {
        threshold <- max(wkf$weight) * 0.5
        wkf <- wkf[wkf$weight >= threshold,]
        words <- wkf$word[order(wkf$weight,decreasing=T)]
    } else {
        words <- wkf$word[order(wkf$weight,decreasing=T)[1:n]]
    }

    words
}

#' How many of each item type appear in each temporal interval?
#'
#' not a very fancy plot

plot_items_by_year <- function(metadata,time_interval="year") {
    to.plot <- transform(metadata,
                         Date=cut(pubdate_Date(pubdate),
                                  breaks=time_interval))

    # TODO proper date-interval breaks with cut.Date()
    qplot(as.Date(Date),
          data=to.plot,geom="histogram",
          facets = ~ type) +
        xlab("publication date") +
        ggtitle("Number of each item type, by year")
}

