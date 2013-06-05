# need these functions

source("metadata.R")

# TODO fix irresponsible function names

##############
# File loading
##############

# t is the data frame read in from the doc-topics file
# with rows in mallet's format:
# doc name topic_i proportion_i topic_j proportion_j ...
# we want to sort the topics in index order to compare documents
#
# note that mallet numbers topics from 0 to n - 1, but
# we are going to put topic 0 in column 1 of the result matrix 
#
# this is pretty slow. a faster way is outboard sorting in perl or other
# for-loop-friendly language; see the read.sorted.doc.topics() function

sort.topics <- function(df) {
    # width of data frame: 2n + 2
    w <- dim(df)[2]
    n.topics <- (w - 2) / 2
    # number of docs
    n.docs <- dim(df)[1]

    # construct matrix of indices by extracting topic numbers from each row
    topic.nums <- df[,seq(from=3,to=w-1,by=2)]
    # thus topic.nums[i,j] is the jth most frequent topic in doc i
    # and df[i,2(j + 2)] is the proportion of topic topic.nums[i,j] in doc i
    # with topics numbered from 0 
    # since this is just permuting the even-numbered rows of
    # t.m <- as.matrix(df[,3:w])
    # it must be expressible as some outer product or something
    # but screw it, time for a for loop

    result <- matrix(0,nrow=n.docs,ncol=n.topics)
    for(i in 1:n.docs) {
        for(j in 1:n.topics) {
            result[i,topic.nums[i,j]+1] <- df[i,2 * j + 2] 
        }
    }
    result
}

# create a dataframe with row n, column m giving proportion of topic m in doc m
# add rows named by doc id
# Pass in a function for converting filenames stored
# in mallet's output to id's. The default is the as.id function from metadata.
# R, but you can pass a different one

read.doc.topics <- function(filename=NA,docname.to.id=as.id) {
    topics.filename <- filename
    if(is.na(filename)) { 
        cat(
  "Select file created by mallet train-topics --output-doc-topics...\n")
        ignore <- readline("(press return to open file dialog) ")
        topics.filename <- file.choose()
        print(topics.filename)
    }
    df <- read.table(topics.filename,header=FALSE,skip=1,stringsAsFactors=FALSE)
    ids <- docname.to.id(df$V2)

    topics <- as.data.frame(sort.topics(df))
    names(topics) <- paste("topic",sep="",1:length(topics))
    cbind(topics,id=ids,stringsAsFactors=FALSE)
}

# alternative to read.doc.topics, if you have run the sort_doc_topics
# script on mallet's --output-doc-topics output. Should be faster,
# otherwise the same result

read.sorted.doc.topics <- function(filename=NA,docname.to.id=as.id) {
    topics.filename <- filename
    if(is.na(filename)) { 
        cat("Select doc-topics file sorted by sort_doc_topics...\n")
        ignore <- readline("(press return to open file dialog) ")
        topics.filename <- file.choose()
        print(topics.filename)
    }
    df <- read.table(topics.filename,header=FALSE,stringsAsFactors=FALSE)
    # in this format, V1 is the doc number, V2 is the filename
    ids <- docname.to.id(df$V2)

    # drop those first two columns
    topics <- subset(df,select=-c(V1,V2))
    names(topics) <- paste("topic",sep="",1:length(topics))

    # add the ids again, but on the right 
    cbind(topics,id=ids,stringsAsFactors=FALSE)
}

# read.keys
# input the information in a topic-keys file from mallet-train-topics
# result is a data frame with topics renumbered from 1
# and columns giving the Dirichlet alpha parameter

read.keys <- function(filename=NA) {
    keys.filename <- filename
    if(is.na(filename)) { 
        cat(
  "Select file created by mallet train-topics --output-topic-keys...\n")
        ignore <- readline("(press return to open file dialog) ")
        keys.filename <- file.choose()
        print(keys.filename)
    }

    df <- read.csv(keys.filename,sep="\t",header=FALSE,as.is=TRUE,
             col.names=c("topic","alpha","keywords"))
    df$topic <- df$topic + 1
    df
}

# top-level input function: make a combined 
# dataframe of topic proportions and document metadata
# replace pubdate string with numeric year
# NB id formats must match in the two frames, since the merge is by id
topic.model.df <- function(topic.frame,meta.frame) {
    mf <- meta.frame
    mf$pubdate <- pubdate.to.years(mf$pubdate)
    
    # merge and
    # clumsily reorder to ensure that subsetting result to nth column
    # will give topic n
    merged <- merge(topic.frame,mf,by="id")
    ids <- merged$id
    cbind(subset(merged,select=-id),id=ids)  
}

# for the frame returned by topic.model.df(), this is the number of topics
n.topics <- function(tm) {
    length(tm) - 13
}

topic.model.matrix <- function(tm) {
    tm[,1:n.topics(tm)]
}

#################
# Data processing
#################


# return some descriptive information about the topic
# n: topic number, from 1
# df: frame returned by topic.model.df
# keys.frame: frame returned by read.keys
# threshold: proportion of topic required for a document to be considered
#   exemplary of that topic
# result: a list
#   $top.words: key words in descending order
#   $alpha: alpha_n for the topic
#   $top.articles: exemplary articles, in descending order
topic.info <- function(n,df,keys.frame,threshold=0.3) {
    result <- list()
    result$top.words <- keys.frame$keywords[n]
    result$alpha <- keys.frame$alpha[n]
    topic.selector <- paste("topic",n,sep="")
    docs <- df[df[topic.selector] > threshold,]
    docs <- docs[c("id","title","pubdate",topic.selector)]
    result$top.articles <- docs[order(docs[topic.selector],decreasing=TRUE),]
    result
}

doc.info <- function(doc,tm,kf,num.topics=3) {
    top.topics <- order(tm[doc,1:100],decreasing=TRUE)[seq(num.topics)]
    data.frame(topic=top.topics,proportion=as.numeric(tm[doc,top.topics]),top.words=kf$keywords[top.topics])
}

# Return the top num.words keywords for topic i
# not vectorized in topic

topic.keywords <- function(topic,keys.frame,num.words=5) {
    words <- unlist(strsplit(keys.frame$keywords[topic],split=" "))
    words[1:num.words]
}

# Return a list of keyword-based labels for topics
# vectorized in topic
topic.shortnames <- function(topic,keys.frame,num.words=2) {
    words.list <- strsplit(keys.frame$keywords[topic],split=" ")
    sapply(words.list, function (words) {
           paste(words[1:num.words],collapse=" ")
             }
    )
}

# what is the time interval covered by the data?
# N.B. this is a two-element vector c(min, max)
# if a sequence is desired, generate one with seq from this result
year.range <- function(df) {
    range(df$pubdate)
}

# topics are numbered from 1
# time course of topic number n is thus one column of the topics.frame
# together with the list of years
topic.time.series <- function (n,df) { 
    data.frame(pubdate=df$pubdate,topic.proportion=df[,n])
}

# a frequency table of publication years
# expects df to have a pubdate column of integer years for factoring
articles.per.year <- function(df) {
    table(df$pubdate)
}

# Nice R: vectorized in topics and years, gives overall proportion
# Unnice R: yrs.table, being a table, is indexable by labels, not numbers
# even though it is a table of numbers
topic.years.proportion <- function(topic,yrs,df,
                                   yrs.table = articles.per.year(df)
                                   ) {
    sum(df[df$pubdate %in% yrs,topic]) /
        sum(yrs.table[as.character(yrs)])
}

# The moving window for averages is 2w + 1 years
# (not exactly smoothing, since it doesn't just average averages but
# calculates the average over the whole time-interval)
# returns a two-column matrix, with the years covered in the first column
# and the averaged values in the second column
topic.proportions.by.year <- function(topic,df,smoothing.window=0) {
    yi <- year.range(df)
    w <- smoothing.window
    
    years <- seq.int(yi[1]+w,yi[2]-w) 
    result <- matrix(nrow=length(years),ncol=2)
    result[,1] <- years
    result[,2] <- sapply(years, function (y)
           (topic.years.proportion(topic,(y-w):(y+w),df)))
    result
}

##########
# Plotting
##########

# Plot the yearly proportions of a topic in a basic R plot

plot.topic.proportion <- function(topic,df,keys.frame) {
    topic.words <- paste(topic.keywords(topic,keys.frame),collapse=" ")
    topic.label <- paste("Presence over time of topic", as.character(topic),topic.words)
    plot(topic.proportions.by.year(topic,df), 
        main=topic.label,
        xlab="year",
        ylab="Overall proportion in year's articles"
        )
}

# Plot yearly proportions of a topic as well as proportions in a moving window
# of 2w + 1 years
plot.topic.lines <- function(topic,df,keys.frame,w=2) {
    topic.words <- paste(topic.keywords(topic,keys.frame),collapse=" ")
    topic.label <- paste("Presence over time of topic", as.character(topic),topic.words)
    plot(topic.proportions.by.year(topic,df,smoothing.window=0),
        type="l",col="orange",
        main=topic.label,
        xlab="Year",
        ylab="Overall proportion of topic"
        )
    lines(topic.proportions.by.year(topic,df,smoothing.window=w),
          type="l",col="blue")
    legend(x="topright",
           c(paste(2*w+1,"year moving window"),"1 year window"),
           text.col=c("blue","orange")) 
}

# Same as plot.topic.lines, but uses qplot
plot.topic.yearly <- function(topic,df,keys.frame,w=2) { 
    topic.words <- paste(topic.keywords(topic,keys.frame),collapse=" ")
    topic.label <- paste("Presence over time of topic", as.character(topic),topic.words)

    # construct frame of all observations to plot
    unsmoothed <- as.data.frame(
        topic.proportions.by.year(topic,df,smoothing.window=0))
    smoothed <- as.data.frame(
        topic.proportions.by.year(topic,df,smoothing.window=w))
    unsmoothed$window <- "1 year window"
    smoothed$window <- paste(2*w+1,"year moving window")
    to.plot <- rbind(unsmoothed,smoothed)
    names(to.plot) <- c("year","proportion","window")

    qplot(year,proportion,color=window,data=to.plot,geom="line",
        main=topic.label,
        xlab="Year",
        ylab="Overall proportion of topic"
    )
}

# Builds a faceted plot of yearly proportions (or proportions for a
# given time window 2w + 1) for a vector of topics;
# each facet is a topic.
# graphs are colored according to the "alpha" value, so with a
# hyperparameter-optimized topic model you can see which topics are "bigger"
#

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


#
# visualize a topic over time by plotting its proportion in each document
# against the year of the document
#
# geom: deprecated parameter
# decided the jitter plot option was more chart-junky than anything
#
# date.bin: interval of years to bin documents by

plot.topic.boxplot <- function(topic,df,keys.frame,date.bin=10,geom="boxplot") {
    library(ggplot2)
    library(plyr)
    library(scales)

    topic.words <- paste(topic.keywords(topic,keys.frame,8),collapse=" ")
    topic.label <- paste("Distributions of proportions for topic ", topic,"\n",
                         topic.words, "\n(alpha=",
                         round(keys.frame$alpha[topic],digits=2),") ",
                         "by ",date.bin," year intervals", 
                         sep="")

    series <- topic.time.series(topic,df)
    # binning the years produces a clearer plot
    # I'm sure I'm not supposed to do this manually, but whatevs
    df.plot <- transform(series,pubdate.binned=pubdate - pubdate %% date.bin)
    df.means <- ddply(series,"pubdate",summarize,topic.mean=mean(topic.proportion))

    box.plot <- ggplot(df.plot,aes(pubdate)) +
      geom_boxplot(aes(y=topic.proportion,group=pubdate.binned),
                   color=alpha("black",0.5))
    
    # results: box plots by date.bin intervals with overlay average line
    # TODO smoothing window on average?
    box.plot +
      geom_line(data=df.means,aes(pubdate,topic.mean,
                                  color="yearly average")) +
      labs(title=topic.label) +
      xlab("Date") +
      ylab("Topic proportion") +
      scale_color_hue("") # no title on legend
}

# plain plot.topic is alias for boxplot (backwards compatibility)
plot.topic <- plot.topic.boxplot

plot.topic.scatter <- function(topic,df,keys.frame,a=1/10) {

    topic.words <- paste(topic.keywords(topic,keys.frame,8),collapse=" ")
    topic.label <- paste("Yearly distributions of proportions for topic ",
                         topic,"\n",
                         topic.words, "\n",
                         "(alpha=",
                         round(keys.frame$alpha[topic],digits=2),") ",
                         sep="")
    qplot(pubdate,topic.proportion,
          data=topic.time.series(topic,df),
          alpha=I(a),
          geom=c("point","smooth"),
          xlab="publication date",
          ylab="topic proportion in article",
          main=topic.label)
}


# for each topic in topics, save a plot created by plot.topic to
# a file topicNN.pdf in dirname
write.plots <- function(df,keys.frame,dirname="Rplots",topics=1:n.topics(df),geom="boxplot") {
    dir.create(dirname)
    for(i in topics) {
        filename <- paste(dirname,"/topic",as.character(i),".pdf",sep="")
        plot.topic(i,df,keys.frame,geom=geom)
        ggsave(filename)
    }
}

######################
# Topic interrelations
######################

topics.cor <- function(df) {
    cor(topic.model.matrix(df))
}

topic.time.series.cor <- function(tm) {
  yts <- sapply(1:100,
    function (t) {
      topic.proportions.by.year(t,tm,smoothing.window=0)[,2]
    }
  )
  cor(yts)
}

time.correlated.topics <- function(tm,
                                   corrs=topic.time.series.cor(tm),
                                   threshold=0.7,
                                   anti.correlation=FALSE) {
  # Don't care about the lower triangle or the diagonal 
  m <- corrs
  m[lower.tri(m,diag=TRUE)] <- NA
  result <-
    if(anti.correlation) {
      which(m < -abs(threshold),arr.ind=TRUE) 
    }
    else {
      which(m > threshold,arr.ind=TRUE)
    }
  data.frame(result,Corr=m[result])
}

# dendrogram: experimental
# returns cluster object, which can be plotted with plot()
topics.cluster <- function(df,keys.frame) {
    m <- topic.model.matrix(df)
    m <- t(m)
    row.names(m) <- topic.shortnames(1:n.topics(df),keys.frame)
    hclust(dist(m))
}

# Save a matrix of distances among documents: experimental
# What is a reasonable metric? Manhattan?
# nb the rows will carry an id column indexed from 0

write.doc.distances <- function(tm,outfile="doc_dists.csv") {
    # dist operates on rows
    distances <- as.matrix(dist(topic.model.matrix(tm),method="euclidean"))
    row.names(distances) <- seq_along(tm$id) - 1
    write.csv(distances,outfile,quote=FALSE)
} 

# Save a list of nodes
# these will be listed with an id indexed from 0 to match the output of the 
# above

write.doc.nodes <- function(tm,outfile="doc_nodes.csv") {
    write.csv(data.frame(Id=seq_along(tm$id) - 1,Label=tm$id,Year=tm$pubdate),outfile,row.names=FALSE,quote=FALSE)
}

# Write a list of nodes corresponding to topics, suitable for importing
# into gephi.
# Indexed from 1, labeled by topic.shortnames
write.topic.nodes <- function(tm,kf,outfile="topic_nodes.csv") {
    topic.sequence <- seq(n.topics(tm))
    write.csv(
      data.frame(Id=topic.sequence,
                 Label=topic.shortnames(topic.sequence,kf),
                 Alpha=kf$alpha
                 ),
      outfile,row.names=FALSE,quote=FALSE)
}

# Write a list of edges connecting correlated topics, suitable
# for importing in gephi.
# Topics are indexed from 1 as in the write.doc.nodes output
# Edges are weighted by r
# Only topic pairs with r > corr.threshold are included
# Because computing the yearly average document proportions for all
# topics is expensive, you can pass in the matrix of correlations
# returned by topic.time.series.cor to bypass recalculating it.
write.topic.time.corrs <- function(tm,
                                   outfile="topic_corr_edges.csv",
                                   corrs=NA,
                                   corr.threshold=0.7) {
    to.write <- NA
    anti.cor <- corr.threshold < 0
    if(any(is.na(corrs))) {
      to.write <- time.correlated.topics(tm,
                                          threshold=corr.threshold,
                                          anti.correlation=anti.cor)
    }
    else {
      to.write <- time.correlated.topics(tm,
                                         corrs,
                                         threshold=corr.threshold,
                                         anti.correlation=anti.cor)
    }
    names(to.write) <- c("Source","Target","Weight")
    to.write$Type <- "Undirected"
    write.csv(
      subset(to.write,select=c(Source,Target,Type,Weight)),
      file=outfile,
      row.names=FALSE,
      quote=FALSE)  
} 

