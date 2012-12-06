# need these functions

source("metadata.R")

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

    to.plot.list <- lapply(as.list(topics), function (i) { 
        to.add <- as.data.frame(topic.proportions.by.year(i,df,w))
        names(to.add) <- c("year","proportion")
        to.add$topic <- paste(i,topic.shortnames(i,keys.frame))
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
# geom: boxplot is clearest for seeing time trends and outliers
# but "jitter" is also illustrative of where the topic is distributed
#
# date.bin: interval of years to bin documents by

plot.topic <- function(topic,df,keys.frame,date.bin=10,geom="boxplot") {
    require(ggplot2)

    topic.words <- paste(topic.keywords(topic,keys.frame,8),collapse=" ")
    topic.label <- paste("Presence over time of topic ", topic,"\n",
                         topic.words," (",keys.frame$alpha[topic],")\n",
                         "by ",date.bin," year intervals", 
                         sep="")

    series <- topic.time.series(topic,df)
    # binning the years produces a clearer plot
    # I'm sure I'm not supposed to do this manually, but whatevs
    attach(series)
    binned.pubdate <- pubdate - pubdate %% date.bin
    detach(series)
    series$pubdate <- factor(binned.pubdate)

    qplot(pubdate,topic.proportion,data=series,geom=geom, 
        main=topic.label,
        xlab="Date",
        ylab="Overall proportion of topic"
    )
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

# dendrogram: experimental
# returns cluster object, which can be plotted with plot()
topics.cluster <- function(df,keys.frame) {
    m <- topic.model.matrix(df)
    m <- t(m)
    row.names(m) <- topic.shortnames(1:n.topics(df),keys.frame)
    hclust(dist(m))
}

# Save a matrix of distances among documents
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
