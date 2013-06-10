library(mallet)
library(plyr)
source("topics.R")

#
# Using the R mallet API
#
# when you make a trainer object with MalletLDA(), the result actually
# holds the full topic model in the form of the ParallelTopicTrainer
# java object accessible at trainer$model. N.B. java is strict about
# typing, which makes for some funtimes. To index into array-like
# objects, apply as.integer() to parameters

# TODO TEST

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

read_dfr_wordcounts <- function(dirs=NULL,files=NULL) {
    counts <- read_dfr(dirs=dirs,files=files)
    docs_frame(counts)
}

read_dfr <- function(dirs=NULL,files=NULL) {
    # aggregate all filenames in files
    # and all wordcounts*.CSV files in each dir in dirs
    # into a single vector
    fv <- c(files,unlist(lapply(dirs,function(d) {
                                    Sys.glob(file.path(d,"wordcounts*.CSV"))
                                })))
    
    if(any(!grepl("\\.CSV$",fv))) {
        warning("Not all files specified for reading are named *.CSV")
    }

    counts <- vector("list",length(fv))
    n_types <- integer(length(fv))
    
    for(i in seq_along(fv)) { 
        counts[[i]] <- read.csv(fv[i],strip.white=T,header=T,as.is=T,
                                colClasses=c("character","integer"))

        
        n_types[i] <- nrow(counts[[i]])
    }

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

docs_frame <- function(counts) {
    ddply(counts,.(id),summarize,
          text=paste(rep(WORDCOUNTS,times=WEIGHT),collapse=" "))
}


# create mallet instance and train model
make_instances <- function(docs_frame,stoplist.file,...) {
    # token regex: letters only, by default
    # another possibility would be to include punctuation \p{P}
    mallet.import(docs_frame$id,docs_frame$text,
                  stoplist.file=stoplist.file,
                  ...)
}

write_instances <- function(instances,output.file) {
  instances$save(new(J("java.io.File"),output.file))
}

# instances can either be a mallet instance object
# or the name of a mallet instance file
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

# create a data frame with topic proportions for each document
# rows are in the order they were passed in to mallet
# renumbers topics from 1

topic_frame <- function(trainer) { 
    # matrix of topic proportions (cols) in docs (rows)
    # smoothing means nothing has 0 prob.
    # normalized instead of raw counts 
    doc.topics <- mallet.doc.topics(trainer, smoothed=T, normalized=T)

    doc.frame <- as.data.frame(doc.topics) 
    names(doc.frame) <- paste("topic",sep="",seq(trainer$model$numTopics))
    cbind(doc.frame,id=trainer$getDocumentNames(),stringsAsFactors=F)
}

# for compatibility with my old read.keys function, this
# throws out the weighting information returned by mallet.topic.words
# the provided mallet.top.words function only works on one topic at a time
# and rather expensively copies out the whole vocabulary each time you call it
# so let's at least frontload that step
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


# save the state
write_mallet_state <- function(trainer,outfile="state.gz") {
    fileobj <- new(J("java.io.File"),outfile)
    trainer$model$printState(fileobj)
}


# Read in a Gibbs sampling state from disk. supply a .gz filename
read_mallet_state <- function(infile) {
    con <- gzfile(infile)
    result <- read.table(con,header=F,comment.char="#",
               col.names=c("doc","source","pos","typeindex","type","topic"),
               sep=" ",as.is=T) 
    result
}

# wrapper for the previous two
# obviously you could do this all in memory, but: rJava = annoying
sampling_state <- function(trainer,tmpfile="state.gz",rm.tmpfile=F) {
    write_mallet_state(trainer,tmpfile)
    result <- read_mallet_state(tmpfile)
    if(rm.tmpfile) {
        unlink(tmpfile)
    }
    result
}

# TODO this attempt to clone ParallelTopicModel.printState doesn't work
# TODO for speed, must rewrite rJava $ operator with low-level .jcall()
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
