
topic_keyword_multi <- function(wkf,topics,breaks,
                                filename_base="",
                                format="png",
                                w=6,h=4) {
  cuts <- cut(topics,breaks=breaks,ordered_result=T)
  for(c in levels(cuts)) {
    ts <- topics[cuts==c]
    p <- topic_keyword_plot(wkf,ts)
    plotname <- paste(filename_base,min(ts),"_",max(ts),".",format,sep="")
    message("Saving ",plotname)
    ggsave(plot=p,filename=plotname,width=w,height=h)
  } 
}

topic_keyword_plot <- function(wkf,topic) {
    keys <- wkf[wkf$topic %in% topic,]
    keys <- transform(keys,
                      topic_label=sprintf("topic %03d, a=%.3f",topic,alpha))
    
    # TODO fix axis ordering problem.
    keys$word_order <- with(keys,
                            reorder(word,order(topic,-weight)))
    p <- ggplot(keys) 
    p <- p + 
        geom_segment(aes(x=0,xend=weight,
                         y=word_order,
                         yend=word_order,
                         color=alpha),
                     size=2) +
        facet_wrap(~ topic_label,scale="free")
    p <- p + theme(axis.title.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   axis.text.y=element_text(color="black",size=10),
                   legend.position="none")
            
    p
}

# TODO test
doctops_long <- function(doctops,metadata,
                    meta_keep=c("pubdate","journaltitle")) {
    meta <- unique(c("id",meta_keep))
    wide <- merge(doctops,metadata[,meta],by="id")
    melt(wide,id.vars=c(meta))
}

# TODO test
tm_time_averages <- function(tm_long,time_breaks="5 years",
                             grouping="journaltitle") {
    # copy on modify
    tm_long$pubdate <- cut(pubdate_Date(tm_long$pubdate),time_breaks)
    
    # TODO use grouping

    ddply(tm_long,.(pubdate,journaltitle,variable),summarize,
          proportion=mean(value))
}

# TODO test and fix up
tm_time_averages_plot <- function(tm_long,time_breaks="5 years",
                                  grouping="journaltitle") {
    to.plot <- tm_time_averages(tm_long,time_breaks,grouping)

    # only works if pubdate has been made into a _factor_ by cut.Date()

    # "variable" is topic1,topic2,etc
    # "proportion" is the mean proportion
    qplot(as.Date(pubdate),proportion,data=to.plot,
          fill=journaltitle,geom="bar",stat="identity",position="dodge",
          facets=~variable)
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

