# Selected visualizations of the topic model and the corpus, with an emphasis on seeing 
# how the counts are distributed over document dates.

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
#' @param doc_topics document-topic frame from \code{\link{doc_topics_frame}}
#' @param wkf topic key words frame from \code{\link{weighted_keys_frame}}
#' @param metadata metadata frame from \code{\link{read_metadata}}
#'
#' @param topic a vector of topics to generate plots for (by default, all)
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
topic_report <- function(doc_topics,wkf,metadata,
                         topics=1:(ncol(doc_topics) - 1),
                         time_breaks="5 years",
                         log_scale=F,
                         output_dir="topic_report",
                         w=1200,h=800) {

    dt_wide <- doc_topics_wide(doc_topics,metadata,meta_keep="pubdate")
    dt_long <- doc_topics_long(doc_topics,metadata,meta_keep="pubdate")

    # scale so that keyword weight bars are appropriately colored
    color_scale <- scale_color_gradient(limits=range(wkf$alpha))

    # only calculate yearly totals once for all topics

    yearly <- topic_year_matrix(dt_wide=dt_wide)

    series <- topic_proportions_series_frame(yearly,topics=topics)

    for(topic in topics) {
        filename <- file.path(output_dir,
                              sprintf("%03d.png",topic))
        png(file=filename,width=w,height=h)
        message("Saving ",filename)
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(2,2)))

        print(topic_keyword_plot(wkf,topic,color_scale),
              vp=viewport(layout.pos.row=c(1,2),layout.pos.col=1))

        print(topic_yearly_lineplot(series[series$topic==topic,],
                                    raw_counts=F),
              vp=viewport(layout.pos.row=1,layout.pos.col=2))

        print(topic_time_boxplots(subset(dt_long,
                                      variable==paste("topic",topic,
                                                      sep="")),
                               time_breaks=time_breaks,
                               log_scale=log_scale),
              vp=viewport(layout.pos.row=2,layout.pos.col=2)) 
        
        dev.off()
    }
}

#' Plot a single topic's most probable words
#'
#' Creates a simple plot visualizing word weights in a topic as horizontal bars. The 
#' weightiest words appear at the top of the plot.
#'
#' The default \code{\link{weighted_keys_frame}} will simply rank words by frequency in 
#' the topic. However, nothing prevents you from using this function with the results of 
#' another scoring scheme, for example by taking 
#' \code{\link{tw_wkf}(\link{topic_word_scores})}.
#' 
#' @param wkf weighted keys frame
#' @param topic topic number (matched against \code{wkf$topic})
#' @param color_scale scale to use for mapping alpha value to color. Not particularly 
#' useful for a single plot; if you are making multiple plots, however, as 
#' \code{\link{topic_report}} does, then specifying this is handier.
#' @return a \link[ggplot2]{ggplot} object
#'
#' @seealso
#' \code{\link{topic_report}},
#' \code{\link{weighted_keys_frame}}
#'
#' @export
#'
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

#' Visualize topic-corpus distances
#'
#' This provides a quick way to visualize a useful diagnostic calculated by MALLET:
#' the KL divergence between a topic and the corpus itself.
#'
#' If a topic is too "close" to the corpus in this sense, it provides little additional information about patterns of word use. Thus topics that score low on this diagnostic may be "bad" for most purposes. Use \code{\link{read_diagnostics}}
#' to get a dataframe from MALLET's XML diagnostic output to pass to this function.
#'
#' Pass in subsets of these by topics if you wish to plot only some
#' topics. With more than 100 topics, the labels get hard to fit in a vertical stack.
#'
#' @param corpus_dist a vector of corpus distances in topic order. Normally, obtain this 
#' from \code{\link{read_diagnostics("diagnostics.xml")$topics$corpus_dist)}}.
#'
#' @param wkf the weighted keys frame (for naming topics on the plot)
#'
#' @return a \link[ggplot2]{ggplot} object
#'
corpus_dist_plot <- function(corpus_dist,wkf) {
    topic_order <- order(corpus_dist,decreasing=T)
    to_plot <- data.frame(topic=topic_names(wkf),
                          distance=corpus_dist)

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



#' Plot time series of yearly topic proportions
#'
#' Plot a time series of topics as lines on a single plot or on faceted plots.
#'
#' This is a convenience function for quickly visualizing output from 
#' \code{\link{topic_proportions_series_frame}}. It does not offer fine-grained control 
#' over the plot: for that, I recommend making plots yourself from the data frame. 
#'
#' @param series three-column data frame of dates, topics, and weights, from 
#' \code{\link{topic_proportions_series_frame}}
#'
#' @param topic_label function mapping topic numbers to labels. Recommended: pass the result of \code{\link{topic_labeller}(wkf,...)}. By default just the topic number is used.
#'
#' @param raw_counts Are topic scores word counts or estimated proportions? This
#' does not affect the plotted data, but if the topic scores have been
#' normalized then we are looking at the frequency of the topic in documents
#' rather than the frequency of the topic in words, so the title of the plot is
#' changed accordingly.
#'
#' @param facet If TRUE, make a faceted plot with one small plot for each topic in \code{series}. If FALSE, put multiple lines on a single plot.
#' @param smoothing_line If TRUE, add a loess smoother.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{topic_proportions_series_frame}} to generate the data needed for the plot,
#' \code{\link{topic_labeller}} for generating facet titles,
#' \code{\link{topic_yearly_barplot}} for bars instead of lines showing the same data.
#'
#' @export
#'
topic_yearly_lineplot <- function(series,
                                  topic_label=function (n) { sprintf("%03d",n) },
                                  raw_counts=T,facet=F,
                                  smoothing_line=F) {

    plot_title <- ifelse(raw_counts,
                         "Proportion of words in topic",
                         "Proportion of documents in topic")

    if(length(unique(series$topic)) == 1) {
        result <- ggplot(series,aes(year,weight,group=1))
        result <- result + geom_line()
        plot_title <- paste(plot_title,topic_label(unique(series$topic)))

        if(facet) {
            warning("Ignoring facet=TRUE for single topic")
        }

        if(smoothing_line) {
            result <- result + geom_smooth(method="loess")
        }
        result <- result + theme(legend.position="none")
    }
    else { 
        series$topic <- topic_label(series$topic)
        result <- ggplot(series,aes(year,weight,group=topic))
        plot_title <- paste(plot_title,"s",sep="")

        if(facet) { 
            result <- result + geom_line() + facet_wrap(~ topic)
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

#' Plot time series of yearly topic proportions as bars
#'
#' Plot time series of topics as bars (faceted for multiple topics).
#'
#' This is a convenience function for quickly visualizing output from 
#' \code{\link{topic_proportions_series_frame}}. It does not offer fine-grained control 
#' over the plot: for that, I recommend making plots yourself from the data frame. 
#'
#' @param series three-column data frame of dates, topics, and weights, from 
#' \code{\link{topic_proportions_series_frame}}
#'
#' @param topic_label function mapping topic numbers to labels. Recommended: pass the result of \code{\link{topic_labeller}(wkf,...)}. By default just the topic number is used.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{topic_proportions_series_frame}} to generate the data needed for the plot,
#' \code{\link{topic_labeller}} for generating facet titles,
#' \code{\link{topic_yearly_lineplot}} for bars instead of lines showing the same data.
#'
#' @export
#'
topic_yearly_barplot <- function(series,
                                 topic_label=function (n) { sprintf("%03d",n) }) {

    series$topic <- topic_label(series$topic)

    p <- ggplot(series,aes(year,weight)) +
        geom_bar(stat="identity",fill="grey80",width=90) +
        geom_smooth(method="loess",span=0.5,fill="grey60",color="black",se=F)

    if(length(unique(series$topic)) > 1) {
        p <- p + facet_wrap(~ topic)
    }

    p <- p + scale_y_continuous(labels=percent_format()) +
        xlab("article publication year") +
        ylab("proportion of words in corpus") +
        ggtitle("Topics over time")
    
    p
}

#' Plot time-sliced distributions of topic weights in documents
#'
#' In order to visualize how a topic is "spread out" in documents over time, this plot 
#' shows you boxplots of the document-weight distribution for a topic, sliced by a 
#' specified time interval, with a smoothing line superimposed.
#'
#' @param doctops_long long-form document-topic data frame with added pubdate metadata 
#' (from \code{\link{doc_topics_long}(doctops,metadata,meta_keep="pubdate")})
#'
#' @param time_breaks intervals in which to plot doc-topic distributions. For the specification, see \code{\link[base]{cut.Date}}. The default is five-year intervals.
#' @param log_scale if TRUE (the default), log the y axis. Set to FALSE if there are 
#' zeroes in the doc-topic proportions (i.e. no smoothing).
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{topic_yearly_barplot}},
#' \code{\link{topic_yearly_lineplot}},
#' \code{\link{topic_report}}
#'
#' @export
#'
topic_time_boxplots <- function(doctops_long,time_breaks="5 years",log_scale=T) {
    doctops_long$date_cut <- cut(pubdate_Date(doctops_long$pubdate),time_breaks)

    result <- ggplot(doctops_long,aes(x=as.Date(date_cut),y=value,group=date_cut))
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

    if(length(unique(doctops_long$variable)) > 1) {
        result <- result + facet_wrap(~ variable)
    }

    result +
        xlab(paste("date (intervals of ",time_breaks,")",sep="")) +
        ylab("document topic proportions") +
        ggtitle(plot_title)
}

#' Plot a topic's time series in individual journals
#'
#' Convenience function for plotting results from \code{\link{topic_year_meta}}.
#' Produces an area plot of the yearly proportions of topic, conditional on journal.
#'
#' @param topic the one-indexed topic number.
#' @param topic_year_meta_frame the result of
#' \code{topic_year_meta(doctops,metadata,yearly_overall,vars="journaltitle")}
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{topic_year_meta}}
#'
#' @export
#' 
topic_yearly_journals_plot <- function(topic,topic_year_meta_frame) {
    topic_year_meta_frame$pubdate <- as.Date(pubdate)

    topic_name <- paste("topic",topic,sep="")
    ggplot(topic_year_meta_frame,aes_string(x="pubdate",
                                            y=topic_name,
                                            group="journaltitle",
                                            fill="journaltitle")) +
        geom_area() +
        ggtitle(paste("Proportion of words in topic",topic))
}



#' Plot individual word frequencies over time
#'
#' The MALLET 1-gram viewer! Use date metadata to show 
#' you the changing frequency of terms over time in the processed corpus.
#'
#' This is useful when exploring topic
#' frequencies over time. For example, compare \code{\link{topic_yearly_lineplot}(topic,...
#' )} with \code{\link{term_yearly_lineplot}(\link{topic_top_words}(topic,n=50,...))} to
#' discover whether the top words of a topic really move up and down in the same way that 
#' a topic does (which may or may not be significant).
#'
#' @param words a vector of words
#'
#' @param term_year term-year matrix, from \code{\link{term_year_matrix}}
#'
#' @param year_seq map from column indices of \code{term_year} to dates. Also returned in 
#' the 
#' list from \code{\link{term_year_matrix}}
#'
#' @param vocab map from matrix rows to terms, matched against \code{words}
#'
#' @param plot_freq if TRUE, plot yearly fractions of the total; if FALSE, plot raw counts 
#' @param plot_total if TRUE, plot sum of weights of all \code{words}
#'
#' @param smoothing if TRUE, add a smoothing line to the plot
#'
#' @seealso
#' \code{\link{term_year_series_frame}}
#' \code{\link{term_year_matrix}}
#'
#' @export
#'
term_yearly_lineplot <- function(words,term_year,year_seq,vocab,
                                 plot_freq=T,
                                 plot_total=F,
                                 smoothing=F,
                                 denominator=NULL) {
    words <- words[words %in% vocab]

    series <- term_year_series_frame(words,term_year,year_seq,vocab,
                                     raw_counts=!plot_freq,
                                     total=plot_total,
                                     denominator=denominator)

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

    result <- result + geom_line()
    if(smoothing) {
        result <- result + geom_smooth()
    }

    result +
        ylab(ifelse(plot_freq,"yearly word frequency",
                    "yearly word count")) +
        ggtitle(plot_title)
}

#' Plot words in a topic over time
#'
#' Making use of the final Gibbs sampling state, this function makes a line plot showing
#' the occurrence of individual words \emph{in a topic} over time. Whether you
#' plot counts or ratios, this is not the same as the corpus frequency of
#' the individual words; this is the time track of words assigned to the
#' given topic.
#'
#' In fact this function is simply a convenience wrapper for 
#' \code{\link{term_yearly_lineplot}}, just passing that function the topic-conditioned 
#' term-document matrix instead of the overall term-document matrix.
#'
#' @param words a vector of words
#'
#' @param topic_label: a short label for the topic, to go in the plot title
#'
#' @param tytm the sparse matrix returned in the results of
#' \code{\link{term_year_topic_matrix}}
#'
#' @param yseq map from columns of the tytm to dates (also in
#' results of \code{\link{term_year_topic_matrix}})
#'
#' @param vocab map from rows of \code{tytm} to words (the MALLET vocabulary)
#'
#' @param ... plotting parameters handed on to
#' \code{\link{term_yearly_lineplot}}. Be careful with the
#' \code{plot_freq} parameter: by default, this will give the
#' results normalized by column sums of \code{tytm}, which will
#' give the proportion of words \emph{words assigned to the topic}
#' occupied by \code{words}. If you want the denominator to be
#' the proportion of words \emph{in the processed corpus}, pass
#' \code{denominator=colSums(term_yearly_matrix(...))}.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{term_year_topic_matrix}},
#' \code{\link{term_yearly_lineplot}}
#'
#' @export
#'
term_yearly_topic_lineplot <- function(words,topic_label,
                                       tytm,yseq,vocab,...) {
    result <- term_yearly_lineplot(words=words,
                                   term_year=tytm,
                                   year_seq=yseq,
                                   vocab=vocab,
                                   ...)
    plot_title <- ifelse(length(words)==1,words,"Words")
    plot_title <- paste(plot_title,"in\n",topic_label)
    result <- result + ggtitle(plot_title)
    result
}

#' Plot topics on the plane
#'
#' This plot gives a heuristic sense of how "close" topics are to one another.
#'
#' The strategy is to take the Jensen-Shannon divergence among 
#' the topics considered as distributions over words, and then use 
#' multidimensional scaling (i.e. PCA) to reduce these distances in word-
#' distribution space to distances in \eqn{R^2}.
#'
#' Actually, nothing stops you setting \code{tw} to be the topic-document matrix and 
#' \code{b} to be the vector of \eqn{\alpha_k}. That would plot the distances among topics 
#' as distributions over 
#' documents.
#'
#' @param tw matrix with topics in rows and raw word counts in columns (use \code{\link{read_topic_words}}
#' @param b estimated \eqn{beta} value (used to smooth the counts)
#' @param topic_label function mapping topic numbers to labels. Recommended: pass the 
#' result of \code{\link{topic_labeller}(wkf,...)}. By default just the topic number is 
#' used.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso
#' \code{\link{topic_divergences}},
#' \code{\link{read_topic_words}},
#' \code{\link[stats]{cmdscale}},
#' \code{\link{topic_labeller}}
#'
#' @export
#'
topic_dist_plot <- function(tw,b,
                            topic_label=function (n) { sprintf("%03d",n) }) {
    divs <- topic_divergences(tw,b)
    dists <- cmdscale(divs,k=2)
    to_plot <- data.frame(label=topic_label(seq(nrow(tw))),
                          x=dists[,1],y=dists[,2])
    ggplot(to_plot) + geom_text(aes(x=x,y=y,label=label,hjust=1)) +
        scale_x_continuous(expand=c(.1,.1)) + # a little extra horizontal air 
        theme(axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
}

    

#' Histogram of items by year
#'
#' Use the metadata to see the time distribution of the corpus. Not a very fancy plot.
#'
#' Facets on item types (often useful, but note that JSTOR's metadata is imperfect).
#'
#' @param metadata the metadata frame
#' @param time_interval passed on to \code{\link[base]{cut.Date}} to bin dates
#' 
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @export
#'
items_time_histogram <- function(metadata,time_interval="year") {
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

