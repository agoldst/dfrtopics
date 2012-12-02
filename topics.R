# TODO GENERALIZE

# need these functions

source("metadata.R")

##############
# File loading
##############


# t is the data frame read in from the doc-topics file
# with raws in mallet's format:
# doc name topic_i proportion_i topic_j proportion_j ...
# we want to sort the topics in index order to compare documents
#
# note that mallet numbers topics from 0 to n - 1, but
# we are going to put topic 0 in column 1 of the result matrix 

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
# To get these names, need a way of converting from the filenames storted
# in mallet's output to id's. Default is the as.id function, but you can pass
# a different one

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

# read.keys
# input the information in a topic-keys file from mallet-train-topics
# result is a data frame with topics renumbered from 1
# and columns giving the Dirichlet alpha (TODO CHECK) parameter

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
    merge(topic.frame,mf,by="id")
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
    docs <- docs[c("id,","title","pubdate",topic.selector)]
    result$top.articles <- docs[order(docs[topic.selector],decreasing=TRUE),]
    result
}

# TODO TEST ALL THESE
year.range <- function(df) {
    range(df$pubdate)
}

# topics are numbered from 1
# time course of topic number n is thus one column of the topics.frame
# together with the list of years
topic.year.series <- function (n,df) { 
    matrix(c(df[,n],df$pubdate),ncol=2) 
}

# expects df to have a pubdate column of integer years for factoring
articles.per.year <- function(df) {
    table(df$pubdate)
}

# Nice R: this accepts a year or a range of years
topic.years.proportion <- function(topic,yrs,df,
                                   yrs.table <- articles.per.year(df)
                                   ) {
    sum(df[df$pubdate %in% yrs,topic]) /
        sum(yrs.table[yrs])
}

# The window for smoothing by moving averages is 2w + 1
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


blob.IGNORE <- function () {
##########
# Plotting
##########

plot.topic.proportion <- function(topic) {
    topic.words <- paste(topic.keywords(topic)[1:5],collapse=" ")
    topic.label <- paste("Presence over time of topic", as.character(topic),topic.words)
    plot(year.range,topic.proportions.by.year(topic), 
        main=topic.label,
        xlab="year",
        ylab="Overall proportion in year's articles"
        )
}

# parameters to the function would be better than function names,
# but I'm too lazy
plot.topic.smoothed.and.unsmoothed <- function(topic) {
    w <- 2 
    topic.words <- paste(topic.keywords(topic)[1:5],collapse=" ")
    topic.label <- paste("Presence over time of topic", as.character(topic),topic.words)
    y.label <- paste("Overall proportion in year's articles")
    plot(year.range,topic.proportions.by.year(topic),
        type="l",col="orange",
        main=topic.label,
        xlab="year",
        ylab=y.label
        )
    lines(topic.proportions.by.year.smoothed(topic,w),type="l",col="blue")
    legend(x="topright",
           c(paste(2*w+1,"year smoothing"),"raw"),
           text.col=c("blue","orange")) 
}

make.all.plots <- function() {
    dir.create("Rplots")
    for(i in 0:(n.topics - 1)) {
        filename <- paste("Rplots/topic",as.character(i),".pdf",sep="")
        pdf(filename)
        plot.topic.smoothed.and.unsmoothed(i)
        dev.off()
    }
}

#######################
# Topic browsing
#######################

# metadata.from.db
# gets metadata from database, returns it in a frame
# opens and closes database connection

metadata.from.db <- function(database.filename=file.choose(),table.name="document") { 
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname=database.filename)
    result <- dbReadTable(db.con,table.name)

    # Database cleanup

    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)

    result
}


write.all.exemplary.docs <- function(
        topic.model.frame,
        keys.frame,
        filename="exemplary-documents.txt",
        t=0.2
        ) {
    write(paste("# Threshold: >",t),filename) 

    # TODO FIX UP TO USE NEW ROUTINES
    for(n in 1:(n.topics - 1)) {
        write(paste("Topic",n),filename,append=TRUE) 
        write(paste(topic.keywords(n),collapse=" "),filename,append=TRUE) 
        write.table(
                documents.by.topic(n,docs.metadata.frame,threshold=t),
                filename, row.names=FALSE,col.names=FALSE,quote=FALSE,
                append=TRUE)
    }
    cat("Data written to ",filename,"\n")
}
}

#########
# Testing
#########

# TODO MAKE GENERIC
test.routines <- function () {

cat("Sanity tests on the data:\n")
test.year <- 1940
test.topic <- 25
test.row <- 3023
cat("Test year", test.year)
cat("; test topic", test.topic, ":\n",paste(topic.keywords(test.topic)))
cat("\nTest doc: row #", test.row)
cat("\nFilename on that row:", as.character(topics.frame[test.row,2]))
cat("\nDoc id #", name.to.id(topics.frame[test.row,2]))

cat("\nSum of all topics in test doc (should be 1)","\n")
print(sum(topics.frame[test.row,3:(n.topics + 2)]))

cat("Sum of proportions over all docs and topics")
cat("\nShould be equal to the number of documents,",n.docs,"\n")

print(sum(topics.frame[,3:(n.topics + 2)]))

cat("Testing that year range is as expected:")
cat("\nrange(year.range)==range(topics.frame$year)\n")
print(all(range(year.range)==range(topics.frame$year)))

cat("Yearly aggregates:")
cat("\nSum of proportions of topics in the test year (should be 1)\n")
print(
      sum(
          sapply(0:(n.topics-1),
                 function(n) (topic.years.proportion(n,test.year)))
          )
      )

cat("Sum over all topics of proportions across all years")
cat("\nShould be equal to the number of years in the range",
    max(year.range)-min(year.range)+1,"\n")
print(
      sum(
          sapply(0:(n.topics-1), topic.proportions.by.year)
          )
      )

cat("Smoothing:")
cat("\nZero smoothing is same as proportions by year without smoothing\n")
print(
      all(
          topic.proportions.by.year.smoothed(test.topic,0)[,2]
          ==
          topic.proportions.by.year(test.topic)
          )
      )

cat("Showing a plot superimposing smoothed (blue) and unsmoothed (orange) data")
plot.topic.smoothed.and.unsmoothed(test.topic)

cat("\nShowing exemplary documents for test topic\n")
metadata.frame <- metadata.from.db()
print(documents.by.topic(test.topic,metadata.frame))
cat("\nEnd of tests.")
}
