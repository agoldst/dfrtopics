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

# TODO this attempt to clone ParallelTopicModel.printState
# doesn't work
get_mallet_state <- function(trainer) {
    dat <- trainer$model$data
    maxsize <- trainer$model$totalTokens
    result <- data.frame(doc=numeric(maxsize),
                         source=character(maxsize),
                         pos=numeric(maxsize),
                         typeindex=numeric(maxsize),
                         type=character(maxsize),
                         topic=numeric(maxsize),
                         stringsAsFactors=F) 

    alph <- trainer$model$alphabet
    p0 <- 1
    for(d in (seq_len(dat$size()) - 1)) {
        doc <- dat$get(as.integer(d))
        tops <- doc$topicSequence
        toks <- doc$instance$getData()
        src <- doc$instance$getSource()
        src <- ifelse(is.null(src),NA,src$toString())

        doclen <- tops$getLength()
        result$doc[p0:(p0 + doclen - 1)] <- d
        result$source[p0:(p0 + doclen - 1)] <- src

        for(p in seq_len(doclen)) {
            pj <- as.integer(p - 1) # java 0-index
            result$pos[p0 + pj] <- 
                            pj,
                            toks$getIndexAtPosition(pj),
                            alph$lookupObject(pj),
                            tops$getIndexAtPosition(pj))
        }
        p0 <- p0 + doclen
    }
    result <- result[1:p0,]     # truncate to length of actual data
    result
}
