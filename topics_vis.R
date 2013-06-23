# libraries
library(grid)
library(ggplot2)
library(scales)
library(plyr)

# topic_report
#
# Visualize some information about a number of topics. Generates one PNG
# for each topic, with three plots: a plot showing the weights of the
# most probable words in each topic, and a plot showing distributions of
# doc-topic counts/proportions over time (with a smoothing line), and a
# plot showing the yearly proportion of words/docs in the topic---which
# alternative in these latter two depends on whether the doc-topic
# scores are normalized or not.
#
# dt_long: long-form doc-topics data frame. Assumed to have been made by
# doc_topics_long(doc_topics,metadata,meta_keep="pubdate"), i.e.,
# four columns, id, variable (in the form "topicN"), pubdate, value
# (topic weight).
#
# dt_wide: wide-form doc-topics frame, assumed to have been made by
# reading in doc_topics, merging by id with metadata, and dropping
# all but id and pubdate.
#
# topic: a vector of topics (if NULL, visualizes every topic)
#
# wkf: weighted keys data frame
#
# time_breaks: time intervals by which to show doc-topic distributions
#
# log-scale: whether to show topic proportions on a log scale (for
# unsmoothed models, set to F so as not to take log 0 )
#
# raw_counts: are scores normalized? TODO currently unused
#
# filename_base: the name of a directory to save PNG files in.
#
# w,h: PNG dimensions in pixels.

topic_report <- function(dt_long,dt_wide,wkf,
                         topics=NULL,
                         time_breaks="5 years",
                         log_scale=F,
                         raw_counts=F,
                         filename_base="topic_report",
                         w=1200,h=800) {

    if(is.null(topics)) {
        topics <- 1:length(unique(wkf$topic))
    }

    # scale so that keyword weight bars are appropriately colored
    color_scale <- scale_color_gradient(limits=range(wkf$alpha))

    # only calculate yearly totals once for all topics

    yearly <- tm_yearly_totals(tm_wide=dt_wide)

    for(topic in topics) {
        filename <- file.path(filename_base,
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

# topic_keyword_plot
#
# Plot a single topic's most probable words.
#
# wkf: weighted keys frame; topic: a topic number from 1.
#
# color_scale: specify if you are making many plots and want coloration
# by alpha.

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


# tm_yearly_line_plot
#
# plot yearly averages. Supply a long or wide form data frame with
# document- topic scores. Alternatively, you can precalculate the yearly
# totals (using tm_yearly_totals() and supply them as .yearly_totals).
#
# topics: which topics to consider, as a vector of numbers from 1
#
# raw_counts: are topic scores word counts or estimated proportions?
#
# TODO title should reflect raw_counts value
#
# facet: faceted plot or multiple lines on one plot?

tm_yearly_line_plot <- function(tm_long=NULL,tm_wide=NULL,
                                topics=NULL,raw_counts=T,facet=F,
                                smoothing_line=F,
                                .yearly_totals=NULL) {
    if(!is.null(.yearly_totals)) {
        series <- .yearly_totals
    } else if(!is.null(tm_long)) {
        series <- tm_yearly_totals(tm_long=tm_long)
    } else if(!is.null(tm_wide)) {
        series <- tm_yearly_totals(tm_wide=tm_wide)
    } else {
        stop("Supply long, wide, or pre-aggregated document-topic matrix")
    }

    dates <- colnames(series)
    yearly_totals <- colSums(series)
    series <- series %*% diag(1 / yearly_totals) 
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
        result <- result + geom_line() +
            ggtitle(paste("Proportion of words in topic",topics))

        if(facet) {
            warning("Ignoring facet=TRUE for single topic")
        }

        if(smoothing_line) {
            result <- result + geom_smooth(method="loess")
        }
    }
    else {
        to.plot <- rename(to.plot,c("Var1"="topic"))
        result <- ggplot(to.plot,aes(as.Date(Var2),value,group=topic))
        result <- result +
            ggtitle("Proportion of words in topics")

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
        ylab("proportion of year's words")

    result
}

# TODO moving averages
tm_time_averages_plot <- function(tm_long,time_breaks="5 years",
                                  grouping="journaltitle") {

    stop("Moving average plots: not currently implemented")

    to.plot <- tm_time_averages(tm_long,time_breaks,grouping)

    # only works if pubdate has been made into a _factor_ by cut.Date()

    # "variable" is topic1,topic2,etc
    # "proportion" is the mean proportion
    result <- ggplot(to.plot,aes(as.Date(pubdate),proportion))
    if(!is.null(grouping)) {
        geom <- geom_line(aes_string(fill=grouping))

    }
    else {
        geom <- geom_line()
    }

    result <- result + geom + facet_wrap(~ variable)

    # add a median line to give some clue to distorted averages
    result <- result + geom_line(aes(as.Date(pubdate),median),
                                 color="blue",alpha=I(0.5))
    result +
        xlab(paste("date (intervals of ",time_breaks,")",sep="")) +
        ylab("overall topic proportion")
        
}

# tm_time_boxplots
#
# tm_long: a doc-topics frame with merged-in pubdate metadata
#
# time_breaks: intervals in which to plot doc-topic distributions
#
# log_scale: set to F if there are zeroes in the doc-topic proportions.

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

# ----------------
# Individual words
# ----------------

# mallet_word_plot
#
# The MALLET 1-gram viewer!
#
# words: a vector of words
#
# term_year: the term_year_matrix
#
# year_seq: the year sequence corresponding to columns in the term_year_matrix. 
# Expected to be a factor or vector of ISO dates.
#
# the vocabulary corresponding to rows of the term_year_matrix
#
# plot_freq: plot raw counts or yearly ratios?
#
# smoothing: add a smoothing line to the plot?
#
# gg_only: if T, don't add geoms to plot object (so the caller can do it 
# instead)

mallet_word_plot <- function(words,term_year,year_seq,vocab,
                             plot_freq=T,
                             smoothing=F,
                             gg_only=F) {
    w <- match(words,vocab)
    if(any(is.na(w))) {
        message("Dropping words missing from vocabulary: ",
                 paste(words[is.na(w)],collapse=" "))
        words <- words[!is.na(w)]
        w <- w[!is.na(w)]
     }

    wts <- term_year[w,,drop=F]
    if(plot_freq) {
        wts <- wts %*% diag(1 / colSums(term_year)) 
        label <- "yearly word frequency"
    }
    else {
        label <- "yearly word count"
    }

    rownames(wts) <- words
    colnames(wts) <- year_seq
    series <- melt(wts)
    names(series) <- c("word","year","weight")
    series$year <- as.Date(series$year)

    if(length(w) > 1) { 
        result <- ggplot(series,aes(year,weight,color=word,group=word))
        plot_title <- "Words over time (filtered corpus)"
    }
    else {
        result <- ggplot(series,aes(year,weight,group=1))
        plot_title <- paste('"',words,'" over time (filtered corpus)',sep="")
    }

    if(!gg_only) {
        result <- result + geom_line()
        if(smoothing) {
            result <- result + geom_smooth()
        }
    }

    result +
        ylab(label) +
        ggtitle(plot_title)
}

# TODO TEST
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
