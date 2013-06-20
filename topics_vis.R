# libraries
library(grid)
library(ggplot2)
library(scales)
library(plyr)

# topic_keyword_multi
#
# currently deprecated; use topic_report instead.
#
# put multiple topic keyword plots on a single page

topic_keyword_multi <- function(wkf,topics,breaks,
                                filename_base="",
                                format="png",
                                w=6,h=4) {

    # TODO given limitations of topic_keyword_plot, use grid to put multiple
    # plots on each page instead
    cuts <- cut(topics,breaks=breaks,ordered_result=T)
    for(c in levels(cuts)) {
        ts <- topics[cuts==c]
        p <- topic_keyword_plot(wkf,ts)
        plotname <- paste(filename_base,min(ts),"_",max(ts),".",format,sep="")
        message("Saving ",plotname)
        ggsave(plot=p,filename=plotname,width=w,height=h)
    } 
}

# topic_report
#
# Visualize some information about a number of topics. Generates one
# PNG for each topic, with two plots: a plot showing the weights of the
# most probable words in each topic, and a plot showing distributions of
# doc-topic proportions over time (with a smoothing line).
#
# dt_long: long-form doc-topics data frame
#
# topic: a vector of topics (if NULL, visualizes every topic in dt_long)
#
# wkf: weighted keys data frame
#
# time_breaks: time intervals by which to show doc-topic distributions
#
# log-scale: whether to show topic proportions on a log scale (for
# unsmoothed models, set to F so as not to take log 0 )

topic_report <- function(dt_long,wkf,topics=NULL,
                         time_breaks="5 years",
                         log_scale=T, 
                         filename_base="topic_report",
                         w=1200,h=800) {

    if(is.null(topics)) {
        topics <- 1:length(unique(wkf$topic))
    }
    color_scale <- scale_color_gradient(limits=range(wkf$alpha))
    for(topic in topics) {
        filename <- file.path(filename_base,
                              sprintf("%03d.png",topic))
        png(file=filename,width=w,height=h)
        message("Saving ",filename)
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(1,2)))

        print(topic_keyword_plot(wkf,topic,color_scale),
              vp=viewport(layout.pos.row=1,layout.pos.col=1))
        print(tm_time_boxplots(subset(dt_long,
                                      variable==paste("topic",topic,
                                                      sep="")),
                               time_breaks=time_breaks,
                               log_scale=log_scale),
              vp=viewport(layout.pos.row=1,layout.pos.col=2)) 
        
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

# tm_yearly_totals
#
# Tot up the total of each topic for each year. Either a long-form or a
# wide-form data frame may be passed in; the wide form can be handled much
# (orders of magnitude) faster. 
#
# result: a matrix with: rows containing topic totals, columns
# containing years present in the data, colnames with strings
# representing dates.

tm_yearly_totals <- function(tm_long=NULL,tm_wide=NULL) {

    if(!is.null(tm_long)) {
        # copy on modify
        tm_long$pubdate <- cut(pubdate_Date(tm_long$pubdate),breaks="years")
        tm_long$id <- NULL
        totals <- ddply(tm_long,c("variable","pubdate"),summarize,
                        total=sum(value))
        acast(totals,variable ~ pubdate,value.var="total")
    }
    else if(!is.null(tm_wide)) {
        tm_wide$pubdate <- cut(pubdate_Date(tm_wide$pubdate),breaks="years")
        tm_wide$id <- NULL

        # Here, assume that the wide matrix has topic scores in all but
        # the last column, which holds the pubdate. daply will stick the
        # pubdate back on the front when it splits the frame by years.
        # The result will have topics in columns, so transpose.

        topic_sum <- function (d) {
            colSums(d[,1:(ncol(d) - 1)])
        }
        print(names(tm_wide))
        t(daply(tm_wide,"pubdate",topic_sum))
    }
    else {
        stop("Supply either long or wide-form document-topic matrix")
    }
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
# facet: faceted plot or multiple lines on one plot?

tm_yearly_line_plot <- function(tm_long=NULL,tm_wide=NULL,
                                topics=NULL,raw_counts=T,facet=F,
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
    }
    else {
        to.plot <- rename(to.plot,c("Var1"="topic"))
        result <- ggplot(to.plot,aes(as.Date(Var2),value,group=topic))
        result <- result +
            ggtitle("Proportion of words in topics")

        if(facet) { 
            result <- result + geom_line() + facet_wrap(~ topic)
        }
        else  {
            result <- result + geom_line(aes(color=topic))
        }
    }

    result <- result + xlab("publication year") +
        ylab("proportion of year's words")

    result
}

# TODO moving averages
tm_time_averages_plot <- function(tm_long,time_breaks="5 years",
                                  grouping="journaltitle") {
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

