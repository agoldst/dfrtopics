# Let's assume we're typically going to need more Java heap space
# this sets the maximum allocation

options(java.parameters = "-Xmx2g")
library(mallet)
library(plyr)
source("topics.R")

# Using the R mallet API
#
# when you make a trainer object with MalletLDA(), the result actually
# holds the full topic model in the form of the ParallelTopicTrainer
# java object accessible at trainer$model. N.B. java is strict about
# typing, which makes for some funtimes. To index into array-like
# objects, apply as.integer() to parameters

# Basic usage is wrapped up in this convenience function.
#
# Returns a list with
# tm: data frame with document topic proportions and metadata together
# kf: data frame with topic "key words" and alpha parameters
# trainer: the RTopicModel object, after running LDA
#
# To tweak modeling parameters, keep the instances, or work on the documents,
# run these steps individually

model_documents <- function(citations.file,dirs,stoplist.file,num.topics) { 
    mf <- read_metadata(citations.file)
    texts <- read_dfr_wordcounts(dirs=dirs)
    instances <- make_instances(texts,stoplist.file)
    model <- train_model(instances,num.topics=num.topics)
    doc_topics <- topic.model.df(topic_frame(model),mf)
    kf <- keys_frame(model)

    list(tm=doc_topics,kf=kf,trainer=model)
}

# read_dfr_wordcounts
#
# reads in wordcounts*.CSV files and produces a dataframe with one line
# for each document containing the id and the "text" of the document as
# an inflated bag of words
#
# dirs, files: vectors
#
# uses all wordcounts*.CSV files in each of the dirs and all the files
# in files

read_dfr_wordcounts <- function(dirs=NULL,files=NULL) {
    counts <- read_dfr(dirs=dirs,files=files)
    docs_frame(counts)
}

# read_dfr
#
# dirs, files: as above
#
# reads in a bunch of wordcounts*.CSV files and stacks them up in a
# single "long format" dataframe with rows:
# id        WORDCOUNTS  WEIGHT
# <docid>   <feature>   <count>
#
# Note that empty documents are skipped; DfR supplies wordcounts files
# for documents that have no wordcount data. These will be in DfR's
# metadata but not in the output dataframe here.
#
# TODO could this be faster? This is slow. A perl script would be
# faster, but this keeps us in R and does everything in memory.
#
# memory usage: for N typical journal articles, the resulting dataframe
# needs about 20N K of memory. So R will hit its limits somewhere around
# 100K articles.

read_dfr <- function(dirs=NULL,files=NULL,report_interval=100) {
    # aggregate all filenames in files
    # and all wordcounts*.CSV files in each dir in dirs
    # into a single vector
    globs <- file.path(dirs,"wordcounts*.CSV")
    fv <- c(files,Sys.glob(globs))
    
    if(any(!grepl("\\.CSV$",fv))) {
        warning("Not all files specified for reading are named *.CSV")
    }

    counts <- vector("list",length(fv))
    n_types <- integer(length(fv))
    
    for(i in seq_along(fv)) { 
        counts[[i]] <- read.csv(fv[i],strip.white=T,header=T,as.is=T,
                                colClasses=c("character","integer"))

        
        n_types[i] <- nrow(counts[[i]])

        if(i %% report_interval == 0) {
            message("Read ",i," files")
        }
    }

    message("Preparing aggregate data frame...")

    # infuriatingly, concatenating columns separately is a major
    # performance improvement

    wordtype <- do.call(c,lapply(counts,"[[","WORDCOUNTS"))
    wordweight <- do.call(c,lapply(counts,"[[","WEIGHT"))

    # add id column

    data.frame(id=rep(as.id(fv),times=n_types),
               WORDCOUNTS=wordtype,
               WEIGHT=wordweight,
               stringsAsFactors=F)

}

# remove_rare
#
# throw out words whose frequency in the *corpus* is below a threshold
#
# counts: long-form dataframe as returned by read_dfr
#
# freq_threshold: between 0 and 1.

remove_rare <- function(counts,freq_threshold) { 
    # the dumb way is surprisingly fast (lazy evaluation?)
    # whereas ddply(counts,.(WORDCOUNTS),summarize,count=sum(WEIGHT))
    # is very slow
    overall <- with(counts,table(rep(WORDCOUNTS,times=WEIGHT)))
    total <- sum(counts$WEIGHT)
    subset(counts,
           subset=(overall[WORDCOUNTS] / total >= freq_threshold))
} 
                        
# docs_frame
#
# counts: long-format data frame like that returned by read_dfr
#
# inflates wordcounts into bags of words
# 
# the result is a dataframe

docs_frame <- function(counts) {
    ddply(counts,.(id),summarize,
          text=paste(rep(WORDCOUNTS,times=WEIGHT),collapse=" "))
}

# make_instances
# 
# given a frame like that returned by docs_frame above,
# create a mallet InstanceList object
#
# stoplist.file is passed on to mallet
 
make_instances <- function(docs,stoplist.file,...) {
    # token regex: letters only, by default
    # another possibility would be to include punctuation \p{P}
    mallet.import(docs$id,docs$text,
                  stoplist.file=stoplist.file,
                  ...)
}

# write_instances
#
# write a mallet InstanceList object to a file
 
write_instances <- function(instances,output.file) {
  instances$save(new(J("java.io.File"),output.file))
}

# train_model
#
# train the topic model
#
# instances: can either be a mallet instance object or the name of a
# mallet instance file
#
# returns the trainer object, which holds a reference to the RTopicModel
# object (which in turns points to the actual modeling object of class
# ParallelTopicModel

train_model <- function(instances,num.topics,
                        alpha.sum=5,beta=0.01,
                        n.iters=200,n.max.iters=10,
                        n.hyper.iters=20,n.burn.in=50) {
    trainer <- MalletLDA(num.topics,alpha.sum,beta)
    trainer$loadDocuments(instances)
    trainer$setAlphaOptimization(n.hyper.iters,n.burn.in)

    trainer$train(n.iters)
    # following from dmimno's mallet-example.R:
    # iterate picking "best" (?) topic for each token instead of sampling
    # from posterior distribution (?)
    trainer$maximize(n.max.iters)
    trainer
}

# topic_frame
#
# create a data frame with topic proportions for each document rows
# are in the order they were passed in to mallet renumbers topics
# from 1. The result is suitable for joining with metadata using
# topic.model.df() (see "topics.R")

topic_frame <- function(trainer) { 
    # matrix of topic proportions (cols) in docs (rows)
    # smoothing means nothing has 0 prob.
    # normalized instead of raw counts 
    doc.topics <- mallet.doc.topics(trainer, smoothed=T, normalized=T)

    doc.frame <- as.data.frame(doc.topics) 
    names(doc.frame) <- paste("topic",sep="",seq(trainer$model$numTopics))
    cbind(doc.frame,id=trainer$getDocumentNames(),stringsAsFactors=F)
}

# keys_frame
#
# for compatibility with my old read.keys function, this throws out the
# weighting information returned by mallet.topic.words the provided
# mallet.top.words function only works on one topic at a time and rather
# expensively copies out the whole vocabulary each time you call it so
# let's at least frontload that step
#
# efficiency: where it counts least!

# renumbers topics from 1

keys_frame <- function(trainer,num.top.words=20) {
    # matrix of weight assigned to vocabulary item j (cols) in topic i (rows)
    # word j is trainer$getVocabulary()[j]
    word_weights <- mallet.topic.words(trainer,smoothed=T,normalized=T)
    vocab <- trainer$getVocabulary()
    data.frame(topic=seq(trainer$model$numTopics),
               alpha=trainer$getAlpha(),
               keywords=
                    apply(word_weights,1,function(wts) {
                          paste(vocab[order(wts,decreasing=T)[1:num.top.words]],
                                collapse=" ")
                                        }
                    ),
               stringsAsFactors=F
    )
} 

# weighted_keys_frame
#
# A more informative topic key-words data frame, in "long" format
# each row is (alpha,topic,word,weight)
# with num.top.words rows for each of the topics
# the words are in order by weight for each topic.
# you can also use reshape or similar to cast this to "wide" format
# 
# renumbers topics from 1

weighted_keys_frame <- function(trainer,num.top.words=20) {
    word_weights <- mallet.topic.words(trainer,smoothed=T,normalized=T)
    vocab <- trainer$getVocabulary()
    n <- trainer$model$numTopics
    reps <- rep(num.top.words,n)

    result <- data.frame(
        topic=rep(seq(n),times=reps),
        alpha=rep(trainer$getAlpha(),times=reps),
        word=character(n * num.top.words),
        weight=numeric(n * num.top.words),
        stringsAsFactors=F)

    for(i in seq(n)) {
        rows <- 1 + (((i - 1) * num.top.words) :  ((i * num.top.words) - 1))
        js <- order(word_weights[i,],decreasing=T)[1:num.top.words]
        result$weight[rows] <- word_weights[i,js]
        result$word[rows] <- vocab[js]
    }
    result
}

# write_mallet_state
# 
# save the Gibbs sampling state of <trainer> to a (gzipped) file

write_mallet_state <- function(trainer,outfile="state.gz") {
    fileobj <- new(J("java.io.File"),outfile)
    trainer$model$printState(fileobj)
}


# read_mallet_state
#
# Read in a Gibbs sampling state from disk
#
# expects the name of a gzipped file in <infile>

read_mallet_state <- function(infile) {
    con <- gzfile(infile)
    result <- read.table(con,header=F,comment.char="#",
               col.names=c("doc","source","pos","typeindex","type","topic"),
               sep=" ",as.is=T) 
    result
}

# sampling_state
#
# wrapper for the previous two: get a dataframe with the sampling state
#
# obviously you could do this all in memory, but: rJava = annoying

sampling_state <- function(trainer,tmpfile="state.gz",rm.tmpfile=F) {
    write_mallet_state(trainer,tmpfile)
    result <- read_mallet_state(tmpfile)
    if(rm.tmpfile) {
        unlink(tmpfile)
    }
    result
}

# sampling_state_nodisk: DO NOT USE
#
# TODO this attempt to clone ParallelTopicModel.printState doesn't work
#
# TODO for speed, must rewrite rJava $ operator with low-level .jcall()
# and probably also build up each column separately to get away from the
# nightmares associated with indexing into dataframes

sampling_state_nodisk <- function(trainer) {
    warning("Function not implemented. Use sampling_state() instead.")
    return(NULL)

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
            result[p0 + pj,3:6] <- c(pj,
                                      toks$getIndexAtPosition(pj),
                                      alph$lookupObject(pj),
                                      tops$getIndexAtPosition(pj))
        }
        p0 <- p0 + doclen
    }
    result <- result[1:p0,]     # truncate to length of actual data
    result
}
