# TODO GENERALIZE

##############
# File loading
##############


# t is the data frame read in from the doc-topics file
# with raws in mallet's format:
# doc name topic_i proportion_i topic_j proportion_j ...
# we want to sort the topics in index order to compare documents

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

blob.IGNORE <- function () {

cat("Select a file containing SORTED topic data for all documents...\n")
ignore <- readline("(press return to open file dialog) ")
topics.filename <- file.choose()
print(topics.filename)
topics.frame <- read.csv(topics.filename,sep="\t",header=FALSE)


# Number of docs is number of rows
n.docs <- dim(topics.frame)[1]

# Number of topics is the number of columns in the frame,
# less two. Later on we're going to add another column to the frame
# with article years in it, so the below is not an equation that holds
# true at the end of this script

n.topics <- dim(topics.frame)[2] - 2

cat("Loaded data for", n.docs, "documents and", n.topics,"topics\n")

cat("Select a file containing keywords for all topics...\n") 
ignore <- readline("(press return to open file dialog) ")
keys.filename <- file.choose() 
print(keys.filename)
keys.frame <- read.csv(keys.filename,sep="\t",header=FALSE,col.names=c("n","dirichlet","keywords"))

# This one doesn't change; could also use the sqlite database
# directly but never mind that for now.
# N.B. CONTAINS MORE DOCUMENTS THAN WERE PROCESSED FOR TOPIC MODEL
# because it's from the metadata for *all* pmla docs downloaded from
# jstor, whereas topic model was applied only to articles 1890-1999
cat("Select a file containing dates for all filenames...\n") 
ignore <- readline("(press return to open file dialog) ")
#docdates.filename <- "/Users/agoldst/Documents/book/20c/modernism-mining/jstor/pmla/docdates.csv"
docdates.filename <- file.choose()
cat("Reading document date data from:\n",docdates.filename,"\n")
date.frame <- read.csv(docdates.filename,header=FALSE,col.names=c("id","year"))




#################
# Data processing
# Do some elementary computations on the data and store results
# in global variables
# also create some utility functions
#################


# Let's hard code the year range
year.range <- 1890:1999

# Access the keywords in keys.frame
topic.keywords <- function (n) {
    strsplit(as.character(keys.frame$keywords[n+1])," ",fixed=TRUE)[[1]]
}

# Rewrite date.frame as a vector of years labeled by doc ids
date.map = date.frame$year
names(date.map) <- date.frame$id


# Function for turning a doc id into a filename
id.to.name <- function (id) {
    year <- date.map[id] 
    dec <- substr(as.character(year),1,3)
    label <- paste("file:/Users/agoldst/Documents/book/20c/modernism-mining/jstor/pmla/fla-bags/",dec,sep="")
    id.str <- sub("/","_",id)
    label <- paste(label,"/wordcounts_",id.str,".txt",sep="")
    label
}

# And the reverse
name.to.id <- function(filename) {
    match <- regexpr("10\\.2307_\\d+\\.txt",filename,perl=TRUE)
    id.tail <- substr(filename,match + 8,match + attr(match,"match.length") - 5)
    paste("10.2307/",id.tail,sep="") 
}


# Now add a column of article years to the main data frame
# Use date.map to carry column 2, the filename in topics.frame,
# into the corresponding year
topics.frame$year <- sapply(topics.frame[,2],function (f) (date.map[name.to.id(f)]))

# How many articles in a given year?
# Make a table from the date column we have just added to the topics.frame
# The table is labeled by years AS STRINGS

articles.per.year <- table(topics.frame$year)

# Utility function returning the series of data on topic n
# n runs from 0 to n.topics - 1
# Nice R: this is implicitly vectorized for a vector n of topic numbers
topic.proportions <- function (n) {
    topics.frame[,n+3]
}

# topics are numbered from 0
# time course of topic number n is thus one column of the topics.frame
# together with the list of years
topic.year.series <- function (n) { 
    matrix(c(topic.proportions(n),topics.frame$year),ncol=2) 
}

# Nice R: this accepts a year or a range of years
# (unfortunately not in topics, since I can't figure out how write 
# the year subscript correctly for that)
topic.years.proportion <- function(topic,yrs) {
    sum(topic.proportions(topic)[topics.frame$year %in% yrs]) /
        sum(articles.per.year[as.character(yrs)])
}

# The run over the whole range of years of the topic fractions
# for a given topic
topic.proportions.by.year <- function(topic) { 
    sapply(year.range, function (y) (topic.years.proportion(topic,y)))
}

# The window for smoothing by moving averages is 2w + 1
# returns a two-column matrix, with the years covered in the first column
# and the averaged values in the second column
topic.proportions.by.year.smoothed <- function(topic,w) {
    years <- year.range[(1+w):(length(year.range)-w)] 
    result <- matrix(nrow=length(years),ncol=2)
    result[,1] <- years
    result[,2] <- sapply(years, function (y)
           (topic.years.proportion(topic,(y-w):(y+w))))
    result
}


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
# Uses sqlite database
#######################



# metadata.from.db
# gets metadata from database, returns it in a frame
# opens and closes database connection

metadata.from.db <- function(database.filename="/Users/agoldst/Documents/book/20c/modernism-mining/jstor/pmla/metadata.db") { 
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname=database.filename)
    result <- dbReadTable(db.con,"document")

    # Database cleanup

    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)

    result
}

# documents.by.topic
# show metadata about documents for which the specified topic n
# is present in proportion greater than threshold
# pass in the frame with metadata returned by metadata.from.db

documents.by.topic <- function(n,docs.metadata.frame,threshold=0.3) {
    # Database access

    cat("Exemplary documents for topic",n)
    cat("\nKeywords: ", paste(topic.keywords(test.topic)),"\n")

    docs <- topics.frame[topic.proportions(n) >= threshold,2]
    ids <- name.to.id(docs)

    result <- docs.metadata.frame[
                                  docs.metadata.frame$id %in% ids,
                                  c("author","title","pubdate")] 
    result$topic <- topic.proportions(n)[topics.frame[,2] %in% docs]

    result 
}

write.all.exemplary.docs <- function(
        docs.metadata.frame,
        filename="exemplary-documents.txt",
        t=0.2
        ) {
    write(paste("# Threshold: >",t),filename) 
    for(n in 0:(n.topics - 1)) {
        write(paste("Topic",n),filename,append=TRUE) 
        write(paste(topic.keywords(n),collapse=" "),filename,append=TRUE) 
        write.table(
                documents.by.topic(n,docs.metadata.frame,threshold=t),
                filename, row.names=FALSE,col.names=FALSE,quote=FALSE,
                append=TRUE)
    }
    cat("Data written to ",filename,"\n")
}

#########
# Testing
#########

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
