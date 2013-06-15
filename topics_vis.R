# libraries
library(grid)
library(ggplot2)
library(scales)
library(plyr)

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

topic_report <- function(dt_long,wkf,topics=NULL,
                         time_breaks="5 years",
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
                               time_breaks=time_breaks),
              vp=viewport(layout.pos.row=1,layout.pos.col=2)) 
        
        dev.off()
    }
}

topic_keyword_plot <- function(wkf,topic,
                               color_scale=scale_color_gradient()) {
    if(length(topic) > 1) {
        stop("Can only plot a single topic's keywords at once")
    }

    keys <- wkf[wkf$topic==topic,]
    ordered_words <- keys$word[order(keys$weight,decreasing=T)]
    plot_title=sprintf("Top words in topic %03d %s,\na=%.3f",
                        topic,paste(ordered_words[1:3],collapse=" "),
                        wkf$alpha[1])
    
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


tm_time_averages <- function(tm_long,time_breaks="5 years",
                             grouping="journaltitle") {
    # copy on modify
    tm_long$pubdate <- cut(pubdate_Date(tm_long$pubdate),time_breaks)
    
    grouping <- c("pubdate",grouping,"variable")

    ddply(tm_long,grouping,summarize,
          proportion=mean(value),
          median=median(value))
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

tm_time_boxplots <- function(tm_long,time_breaks="5 years") {
    tm_long$date_cut <- cut(pubdate_Date(tm_long$pubdate),time_breaks)

    result <- ggplot(tm_long,aes(x=as.Date(date_cut),y=value,group=date_cut))
    result <- result +
        geom_boxplot() +
        geom_smooth(aes(x=pubdate_Date(pubdate),y=value,group=1),
                    method="auto") +
        scale_y_continuous(trans=log_trans())

    if(length(unique(tm_long$variable)) > 1) {
        result <- result + facet_wrap(~ variable)
    }

    result +
        xlab(paste("date (intervals of ",time_breaks,")",sep="")) +
        ylab("document topic proportions") +
        ggtitle("Doc-Topic distributions (log scale)")
}

topic_time_series <- function(tm,wkf,topic,
                              date_window="2 years") {
    times <- cut.Date(tm$date,date_window)
    docs <- tm[,paste("topic",topic,sep="")]
    docs <- cbind(tm,times=times,stringAsFactors=F)
    means <- ddply(docs,times,colMeans)
   
    # TODO finish
}

plot.topics.yearly <- function(topics,df,keys.frame,w=2) {
    n <- length(topics) * length(df$id)
    
    # TODO better split-apply-combine strategy
    # TODO allow choice of superimposed lines or facets

    to.plot.list <- lapply(as.list(topics), function (i) { 
        to.add <- as.data.frame(topic.proportions.by.year(i,df,w))
        names(to.add) <- c("year","proportion")
        # The facets will be sorted in alphabetical order
        # so, until I learn how to order them,
        # let's just do this kludge, works for n.topics < 1000
        tnum <- sprintf("%03d",i)
        to.add$topic <- paste(tnum,topic.shortnames(i,keys.frame))
        to.add$alpha <- keys.frame$alpha[i]
        to.add
    }
    )
    to.plot <- do.call(rbind,to.plot.list)
    qplot(year,proportion,data=to.plot,facets= ~ topic,color=alpha,geom="line")
}

