
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
