# topics_rmallet.R
#
# Functions for creating topic models and manipulating their output.
# Uses the R API for mallet. The underlying modeling is done by the
# cc.mallet.topics.ParallelTopicModel java class.
#
# Source this, then call topics_rmallet_setup(). model_documents() is
# a wrapper for the most basic usage, but it is more useful as an
# explanation of the order in which the other functions here are to be
# used.
#
# A note on using the R mallet API
# --------------------------------
#
# When you make a trainer object with MalletLDA(), the result actually
# holds the full topic model in the form of the ParallelTopicTrainer
# java object accessible at trainer$model. N.B. java is strict about
# typing, which makes for some funtimes. To index into array-like
# objects, apply as.integer() to parameters.

# topics_rmallet_setup
#
# Call this before running any other functions here.
#
# Needed because we need to set the heap allocation option before rJava
# gets loaded by Rmallet.

topics_rmallet_setup <- function(java_heap="2g") {
    # Let's assume we're typically going to need more Java heap space;
    # this sets the maximum allocation
    heap_param <- paste("-Xmx",java_heap,sep="") 
    options(java.parameters=heap_param)
    library(mallet)
    library(plyr)
    library(Matrix)

    # This file's only real dependency is on metadata.R, which is
    # sourced by topics.R. However, I do use one topics.R function in
    # model_documents(), which shows how to make the output from R
    # mallet into a dataframe like that expected by my old visualization
    # etc. functions in topics.R: use topic.model.df() in cojunction
    # with keys_frame().
    #

    source("metadata.R")
}

# model_documents()
#
# Basic usage is wrapped up in this convenience function.
#
# Returns a list of:
#
# metadata: dataframe of metadata
#
# doc_topics: document-topic weights, with an id column on the right
#
# wkf: data frame with the weightiest words in each topic, plus topic alphas
#
# topic_words: data frame with list of weights of all words in all topics (big)
#
# trainer: the RTopicModel object, after running LDA, holding the sampling 
# state and references to the instance-list.
#
# To tweak modeling parameters, keep the instances, or work on the documents,
# run these steps individually.

model_documents <- function(citations.file,dirs,stoplist.file,num.topics) { 
    mf <- read_metadata(citations.file)
    texts <- read_dfr_wordcounts(dirs=dirs)
    instances <- make_instances(texts,stoplist.file)
    model <- train_model(instances,num.topics=num.topics)
    doc_topics <- doc_topics_frame(model,smoothed=F,normalized=F)
    keys <- weighted_keys_frame(model,smoothed=F,normalized=F)
    topics <- topic_words(model,smoothed=F,normalized=F)

    list(metadata=mf,
         doc_topics=doc_topics,
         wkf=keys,
         topic_words=topics,
         trainer=model)
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

# overall_counts
#
# given a counts frame like that returned by read_dfr, calculate total
# corpus-wide counts for each word type
#
# returns a 1D table, i.e. a vector of counts with word types as the
# element names

overall_counts <- function(counts) {
    # the dumb way is surprisingly fast (lazy evaluation?)
    # whereas ddply(counts,.(WORDCOUNTS),summarize,count=sum(WEIGHT))
    # is very slow
    with(counts,table(rep(WORDCOUNTS,times=WEIGHT)))
}

# remove_rare
#
# throw out words whose frequency in the *corpus* is below a threshold
#
# counts: long-form dataframe as returned by read_dfr
#
# freq_threshold: frequency threshold (between 0 and 1)
#
# OR rank_threshold: rank threshold (natural number)

remove_rare <- function(counts,freq_threshold=NULL,rank_threshold=NULL,
                        .overall=NULL) { 
    # (buried parameter: .overall: precalculated overall counts) 
    if(is.null(.overall)) {
        overall <- overall_counts(counts)
    }
    else {
        overall <- .overall
    }

    if(!is.null(freq_threshold)) {
        message("applying freq_threshold ",freq_threshold)
        total <- sum(overall)
        result <- subset(counts,
                         subset=(overall[WORDCOUNTS] / total >= freq_threshold))
    }
    else {
        if(is.null(rank_threshold)) {
            warning("No threshold provided.")
            result <- counts
        }
        else { 
            count_threshold <- sort(overall,decreasing=T)[rank_threshold]
            result <- subset(counts,
                             subset=(overall[WORDCOUNTS] >= count_threshold))
        }
    }

    result
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

# read_instances
#
# read a mallet InstanceList object from a file

read_instances <- function(filename) {
    J("cc.mallet.types.InstanceList","load",new(J("java.io.File"),
                                                path.expand(filename)))
}

# train_model
#
# train the topic model
#
# instances: can either be a mallet instances object or the name of a
# mallet instances file
#
# returns the trainer object, which holds a reference to the RTopicModel
# object (which in turns points to the actual modeling object of class
# ParallelTopicModel
#
# optimize_hyperparameters=F overrides the other switches for hyperparameter 
# optimization and turns estimation of alpha_k off.

train_model <- function(instances,num.topics,
                        alpha.sum=5,beta=0.01,      # starting values
                        n.iters=200,
                        n.max.iters=10,     # at end: iterated conditional modes
                        optimize_hyperparameters=T,
                        n.hyper.iters=20,   # how often to do hyperparam. opt.
                        n.burn.in=50,       # num. iters before starting hyp. o.
                        symmetric_alpha=F,  # all alpha_k equal?
                        threads=4L) {
    trainer <- MalletLDA(num.topics,alpha.sum,beta)
    trainer$model$setNumThreads(threads)

    trainer$loadDocuments(instances)

    if(optimize_hyperparameters) {
        trainer$model$setSymmetricAlpha(symmetric_alpha)
        trainer$setAlphaOptimization(n.hyper.iters,n.burn.in)
    }
    else {
        trainer$setAlphaOptimization(0,0)
    }

    trainer$train(n.iters)
    # following from dmimno's mallet-example.R:
    # iterate picking "best" (?) topic for each token instead of sampling
    # from posterior distribution (?)
    trainer$maximize(n.max.iters)
    trainer
}

# doc_topics_frame
#
# Create a data frame with topic proportions for each document in the
# first n columns and document ids in the last column. The rows are in
# the order of documents passed into mallet. This function renumbers
# topics from 1. The result is suitable for joining with metadata. 

doc_topics_frame <- function(trainer,smoothed=T,normalized=T) { 
    # matrix of topic proportions (cols) in docs (rows)
    # smoothing means nothing has 0 prob.
    # normalized instead of raw counts 
    doc.topics <- mallet.doc.topics(trainer,smoothed=smoothed,
                                    normalized=normalized)

    doc.frame <- as.data.frame(doc.topics) 
    names(doc.frame) <- paste("topic",sep="",seq(trainer$model$numTopics))
    cbind(doc.frame,id=trainer$getDocumentNames(),stringsAsFactors=F)
}

# doc_topics_matrix
#
# Extract just the numerical part of a doc_topics frame. If you are using the 
# output of the above, you can assume topics in numerical order and documents
# in the order of the instances passed to mallet.

doc_topics_matrix <- function(doctopics) {
    as.matrix(doctopics[,-ncol(doctopics)])
}

# normalize_doc_topics
#
# by convention, I'll call the parameter dtm when I use just the doc-topic 
# matrix, with no metadata in it.

normalize_doc_topics <- function(dtm) {
    dtm <- dtm %*% diag(1 / colSums(dtm))
}

# doc_topics_long
#
# synthesize the above doc_topics frame with metadata into a "long" format
#
# meta_keep: vector of names of columns of metadata to keep
#
# the result will have rows called:
#
# "id"      meta_keep[1]  meta_keep[2] ...  "variable"  "value"
# <docid>   <metadata vals ...>             topicN      <topic proportion>

doc_topics_long <- function(doctops,metadata,
                            meta_keep=c("pubdate","journaltitle")) {
    library(reshape2)
    meta <- unique(c("id",meta_keep))
    wide <- merge(doctops,metadata[,meta],by="id")
    melt(wide,id.vars=meta)
}

# doc_topics_wide
#
# synthesize the above doc_topics frame with metadata into a "wide" format
#
# meta_keep: vector of names of columns of metadata to keep
#
# the result will look like:
#
# "id"      topic1  topic2  topic3  ... meta_keep[1]    meta_keep[2] ...
# <docid>   <topic proportions...>      <metadata vals...>

doc_topics_wide <- function(doctops,metadata,
                            meta_keep="pubdate") {
    meta_keep <- unique(c("id",meta_keep))
    merge(doctops,metadata[,meta_keep],by="id")
}

# tm_yearly_totals
#
# Tot up the total of each topic for each year. Either a long-form or a
# wide-form data frame may be passed in; the wide form can be handled much
# (orders of magnitude) faster. 
#
# result: a matrix with: rows containing topic totals, columns
# containing years present in the data, colnames with strings
# representing dates.

tm_yearly_totals <- function(tm_long=NULL,tm_wide=NULL) {

    if(!is.null(tm_long)) {
        # copy on modify
        tm_long$pubdate <- cut(pubdate_Date(tm_long$pubdate),breaks="years")
        tm_long$pubdate <- droplevels(tm_long$pubdate)
        tm_long$id <- NULL
        totals <- ddply(tm_long,c("variable","pubdate"),summarize,
                        total=sum(value))
        acast(totals,variable ~ pubdate,value.var="total")
    }
    else if(!is.null(tm_wide)) {
        tm_wide$pubdate <- cut(pubdate_Date(tm_wide$pubdate),breaks="years")
        tm_wide$pubdate <- droplevels(tm_wide$pubdate)
        tm_wide$id <- NULL

        # Here, assume that the wide matrix has topic scores in all but
        # the last column, which holds the pubdate. daply will stick the
        # pubdate back on the front when it splits the frame by years.
        # The result will have topics in columns, so transpose.

        topic_sum <- function (d) {
            colSums(subset(d,select=-pubdate))
        }
        t(daply(tm_wide,"pubdate",topic_sum))
    }
    else {
        stop("Supply either long or wide-form document-topic matrix")
    }
} 

# tm_yearly_totals_meta
#
# tally up document topic proportions, keeping some metadata categories
#
# A convenience version of tm_yearly_totals that allows you 
# to split out the yearly totals by, e.g., journaltitle
#
# doctops: the document-topic matrix, assumed to be in a form like that 
# returned by doc_topics_wide
#
# metadata: the metadata frame, or a subset of its columns
#
# yearly_totals: the result of tm_yearly_totals, used for normalizing within 
# each year.
#
# vars: metadata columns to split by; by default, use all metadata columns
#
# result: a data frame suitable for plotting, where each row gives yearly 
# totals for each topic within for a given metadata combination. The topic 
# proportion columns are called "topic1", "topic2", etc.

tm_yearly_totals_meta <- function(doctops,metadata,yearly_totals,vars=NULL) { 
    if(is.null(vars)) {
        vars <- names(metadata)
    }
    vars <- unique(c("id","pubdate",vars))
    doctops <- merge(doctops,metadata[,vars],by="id")
    doctops$pubdate <- cut(pubdate_Date(doctops$pubdate),breaks="years")
    doctops$pubdate <- droplevels(doctops$pubdate)
    doctops$id <- NULL
    vars <- vars[vars != "id"]

    ddply(dt_j,vars,function (d) {
              yr_col <- match(d$pubdate[1],colnames(m$yrly))
              colSums(d[,1:m$n]) / sum(m$yrly[,yr_col])
         })

}


# keys_frame
#
# For compatibility with my old read.keys function, this throws out the
# weighting information returned by mallet.topic.words. The provided
# mallet.top.words function only works on one topic at a time and rather
# expensively copies out the whole vocabulary each time you call it so
# let's at least frontload that step.
#
# Efficiency: where it counts least!
#
# Renumbers topics from 1

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
# A more informative topic key-words data frame, in "long" format.
# each row is (alpha,topic,word,weight),
# with num.top.words rows for each of the topics.
# The words are in order by weight for each topic.
#
# smoothed, normalized: parameters passed on to RTopicModel, which transforms 
# the weights accordingly. For raw counts, set both to F (the default)
#
# Renumbers topics from 1

weighted_keys_frame <- function(trainer,num.top.words=20,
                                smoothed=F,normalized=F) {
    word_weights <- mallet.topic.words(trainer,
                                       smoothed=smoothed,
                                       normalized=normalized)
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

# wkf_kf
#
# turn a weighted keys frame into something like what keys_frame returns
# (for use with plot.topics.yearly etc. from topics.R)

wkf_kf <- function(wkf) {
    ddply(wkf,"topic",summarize,
          alpha=alpha[1],
          keywords=paste(word[order(weight,decreasing=T)],collapse=" "))
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
# Sad wrapper for the previous two: get a dataframe with the sampling
# state. Uses a temporary file and wastes time gzipping and gunzipping.
#
# not particularly easy to do this without going to a file--cf.
# ParallelTopicModel.printState()

sampling_state <- function(trainer,tmpfile="state.gz",rm.tmpfile=F) {
    write_mallet_state(trainer,tmpfile)
    result <- read_mallet_state(tmpfile)
    if(rm.tmpfile) {
        unlink(tmpfile)
    }
    result
}

# read_simplified_state
#
# Read in a Gibbs sampling state and return a dataframe of <document,
# word, topic, count> rows. The former three are represented as
# integers, corrected to be indexed from 1, against the list of
# documents and word types in the mallet instances. Thus you can recover
# the table of words and documents using the instance-reading functions
# below.
#
# generate_file: The Gibbs sampling state is written in a highly redundant
# form which R is not very happy to read in as is. simplify_state.py is
# a python script to toss out the redundant columns (basically, it's
# just gunzip -c | cut -f) which you can use to generate a simplified
# statefile. This function will call that script for you if you pass
# simplify=T and set statefile to the original gzipped mallet state.
#
# data_type: the C++ type to store the data in. If all values have magnitude 
# less than 2^15, you can get away with "short", but guess what? Linguistic
# data hates you, and a typical vocabulary includes more word types than that.

read_simplified_state <- function(infile,generate_file=F,state_file=NULL,
                                  simplifier="python/simplify_state.py",
                                  big=T,
                                  data_type="integer",
                                  big_workdir=tempdir()) {
    if(generate_file) {
        cmd <- paste("python",simplifier,state_file,">",infile)
        message("Executing ",cmd)
        system(cmd)
    }
    if(big) {
        library(bigmemory)
        message("Loading ",infile," to a big.matrix...")
        state <- read.big.matrix(infile,type=data_type,header=T,sep=",",
                                 backingpath=big_workdir,
                                 backingfile="state.bin",
                                 descriptorfile="state.desc")
        message("Done.")
    }
    else {
        state <- read.table(infile,header=T,sep=",",
                             colClasses=rep(integer(),3))
    }

    # change mallet's 0-based indices to 1-based
    state[,1] <- state[,1] + 1L     # docs
    state[,2] <- state[,2] + 1L     # types
    state[,3] <- state[,3] + 1L     # topics

    state
}

# term_year_topic_matrix
#
# Find the term counts per year for a given topic.
#
# The result is, like that of term_year_matrix() below, a list:
#     tym, a sparseMatrix with terms in vocab order in rows and years in cols
#     yseq, the ordering of years in the columns
#     topic, the topic that this slice represents
#
# ss: the "simplified state" returned by read_simplified_state. Operated on 
# using the mwhich function from the bigmemory package.
#
# id_map: the list of doc id's as mallet knows them.
#
# vocab: the list of word types as mallet knows them.
#
# metadata: returned from read_metadata

term_year_topic_matrix <- function(topic,ss,id_map,metadata,vocab) {
    library(Matrix)

    indices <- mwhich(ss,"topic",topic,"eq")

    tdm_topic <- sparseMatrix(i=ss[indices,"type"],
                              j=ss[indices,"doc"],
                              x=ss[indices,"count"],
                              dims=c(length(vocab),
                                     length(id_map)))

    result <- term_year_matrix(metadata=metadata,
                               tdm=tdm_topic,
                               id_map=id_map,
                               vocabulary=vocab,
                               big=T)

    result$topic <- topic
    result
}

# topic_yearly_top_words
#
# Which are the top words in a given topic per year?
#
# returns a vector of pasted-together words, with the dates as names
# 
# tytm, yseq: returned from the term_year_topic_matrix
#
# n_words: how many words?

topic_yearly_top_words <- function(tytm,yseq,vocab,n_words=5) {
    result <- character(length(yseq))
    for (y in seq_along(yseq)) {
        words <- vocab[order(tytm[,y],decreasing=T)[1:n_words]]
        result[y] <- paste(words,collapse=" ")
    }
    names(result) <- yseq
    result
}

# topic_term_time_series
#
# yearly totals of a given word (or words) within a topic: just a
# convenience function to pull out the right row(s) of the tytm.
#
# tytm: the matrix in the results from term_year_topic_matrix

topic_term_time_series <- function(word,tytm,vocab) {
    w <- match(word,vocab)
    tytm[w,,drop=F]
}

term_year_series_frame <- function(words,term_year,year_seq,vocab,
                                   raw_counts=F,
                                   total=F) {
    w <- match(words,vocab)
    if(any(is.na(w))) {
        message("Dropping words missing from vocabulary: ",
                 paste(words[is.na(w)],collapse=" "))
        words <- words[!is.na(w)]
        w <- w[!is.na(w)]
     }

    wts <- term_year[w,,drop=F]
    if(!raw_counts) {
        wts <- wts %*% diag(1 / colSums(term_year)) 
    }

    if (total) {
        wts <- matrix(colSums(wts),nrow=1)
        if(length(words) > 5) {
            words <- paste(words[1:5],collapse=" ")
            words <- paste('Total of "',words,'," etc.',sep="")
        } else {
            words <- paste(words,collapse=" ")
            words <- paste("Total of ",words,sep="")
        }
    }

    rownames(wts) <- words
    colnames(wts) <- year_seq
    series <- melt(as.matrix(wts))
    names(series) <- c("word","year","weight")
    series$year <- as.Date(series$year)

    series
}    



# Save the "topic word weights," i.e. the estimated weights of each word
# for each topic
#
# topic_wordfile: name of a file to write the topic x word matrix as a
# csv file (topics are rows). No row or column headers.
#
# vocab_file: name of a file to write the vocabulary corresponding to
# columns of topic_wordfile. one word per line.
#
# smoothed, normalized: parameters passed on to RTopicModel. Set both to
#F to get raw counts.
#
# This is the same information, differently organized, output in a
# single file by mallet's --topic-word-weights-file <outfile.tsv>
# option. To access that, use:
#
# trainer$model$printTopicWordWeights(new(J("java.io.File"),
#                                     "outfile.tsv"))
#

write_topic_words <- function(trainer,
                              topic_words_file="topics_words.csv",
                              vocab_file="vocab.txt",
                              smoothed=T,
                              normalized=T) {
    tw <- mallet.topic.words(trainer,smoothed=smoothed,normalized=normalized)
    write.table(tw,topic_words_file,sep=",",row.names=F,col.names=F) 
    message("Saved topic-word matrix to ",topic_words_file)
    vocab <- trainer$getVocabulary()
    writeLines(vocab,vocab_file)
    message("Saved vocabulary to ",vocab_file)
}

# read_topic_words
#
# get a "long" format dataframe of words in topics by reading in the
# two files output by the above
#
# result: data frame with three columns, topic, word, and weight.
# 
# NB. check the data: it may be weighted or unweighted, normalized or
# unnormalized

read_topic_words <- function(topic_words_file,vocab_file) {
    vocab <- readLines(vocab_file)
    tw <- read.csv(topic_words_file,header=F,as.is=T)
    nwords <- length(vocab)
    ntopics <- nrow(tw)

    # matrices are unrolled column by column, so we need the transpose of the
    # topic x word matrix

    data.frame(topic=rep(1:ntopics,each=nwords),
               word=rep(vocab,times=ntopics),
               weight=as.vector(t(tw)),
               stringsAsFactors=F)      # if you want to factorize it...
}
    
# topic_words
#
# in-memory version of the above: much, much faster
#
# TODO refactor

topic_words <- function(trainer,smoothed=T,normalized=T) {
    tw <- mallet.topic.words(trainer,smoothed=smoothed,normalized=normalized)
    vocab <- trainer$getVocabulary()
    nwords <- length(vocab)
    ntopics <- nrow(tw)

    # matrices are unrolled column by column, so we need the transpose of the
    # topic x word matrix

    data.frame(topic=rep(1:ntopics,each=nwords),
               word=rep(vocab,times=ntopics),
               weight=as.vector(t(tw)),
               stringsAsFactors=F)      # if you want to factorize it...
}

# topic_words_wide
#
# Convert a long-format topic_words frame to wide (rows are topics,
# variables are words). Or at least try. No promises on speed. The
# result is like the topic-word matrix but has a first column labeling
# the topic (and its variable names contain the actual words).

topic_words_wide <- function(tw,assume_ordered=T) {
    library(reshape2)
    tw <- rename(tw,c("weight"="value"))
    dcast(tw,topic ~ word)
}

# smooth_words

smooth_words <- function(tw,beta) {
    transform(tw,weight=weight + beta)
}

# normalize_words

normalize_words <- function(tw,smoothed,beta=NULL) {
    ddply(tw,"topic",transform,weight=weight/sum(weight))
}

# Basic access to mallet topic diagnostics

get_diagnostics <- function(trainer,num_top_words=20L) {
    .jnew("cc/mallet/topics/TopicModelDiagnostics",
          trainer$model,num_top_words)
}

write_diagnostics <- function(trainer,output_file="diagnostics.xml",
                              diagnostics=get_diagnostics(trainer,20L)) {
    xml <- diagnostics$toXML()
    cat(xml,file=output_file)
}

# read_diagnostics
#
# Parses mallet diagnostic xml output in xml_file. Requires XML library
# and libxml
#
# returns a list of two dataframes:
#
# topics: topic-level diagnostics
#
# words: word-level diagnostics of the most probable words in each
# topics

read_diagnostics <- function(xml_file) {
    library(XML)
    d <- xmlParse(file=xml_file)
    topic_attrs <- xmlSApply(getNodeSet(d,"/model/topic"),xmlAttrs)
    # topic_attrs is a string matrix with topics in *columns*

    topics <- as.data.frame(t(topic_attrs),stringsAsFactors=F)
    # standardize to 1-indexed
    topics <- cbind(topic=as.numeric(topics$id) + 1,topics)
    topics$id <- NULL

    word_info <- function(node) {
        w <- xmlValue(node)
        topic <- as.numeric(xmlGetAttr(xmlParent(node),"id")) + 1
        attrs <- xmlAttrs(node)
        c(topic=topic,word=w,attrs)
    }

    # result of this is a string matrix
    wm <- xmlSApply(getNodeSet(d,"/model/topic/word"),word_info)
    
    words <- as.data.frame(t(wm))

    list(topics=topics,words=words)
}

diagnostics_list <- function(trainer) {
    # TODO implement online access
    stop("online access to diagnostics not implemented.")
    # from TopicModelDiagnostics constructor
#		diagnostics.add(getTokensPerTopic(model.tokensPerTopic));
#		diagnostics.add(getDocumentEntropy(model.tokensPerTopic));
#		diagnostics.add(getWordLengthScores());
#		diagnostics.add(getCoherence());
#		diagnostics.add(getDistanceFromUniform());
#		diagnostics.add(getDistanceFromCorpus());
#		diagnostics.add(getEffectiveNumberOfWords());
#		diagnostics.add(getTokenDocumentDiscrepancies());
#		diagnostics.add(getRank1Percent());
#		diagnostics.add(getDocumentPercentRatio(FIFTY_PERCENT_INDEX, TWO_PERCENT_INDEX));
#		diagnostics.add(getDocumentPercent(5));
}
    

# model_params
#
# Collect together some overall parameters of a model into a one-row
# data frame.
#
# Recall that the topic model is also estimating a beta parameter (just
# one). Also: reminders of the final log-likelihood of the model and the
# total number of tokens in the instances mallet operated on.

model_params <- function(trainer) {
    data.frame(beta=trainer$model$beta,
               n_tokens=trainer$model$totalTokens,
               LL=trainer$model$modelLogLikelihood())
}


# -------------------------------
# Comparing topics (experimental)
# -------------------------------

# JS_divergence
#
# computes the Jensen-Shannon divergence between two vectors, understood as
# distributions over the index.
#
# See Mimno, D. 2012. Computational historiography: Data mining in
# a century of classics journals. ACM J. Comput. Cult. Herit. 5, 1,
# Article 3 (April 2012), 19 pages.
#
# http://doi.acm.org/10.1145/2160165.2160168

JS_divergence <- function(P,Q) {
    PQ_mean = (P + Q) / 2
    sum((1/2) * (P * log(P / PQ_mean) + Q * log(Q / PQ_mean)))

}

# JS_flexmix
#
# For testing JS_divergence against an "official" implementation of
# KLdiv. Not faster on a single pair of rows, and can't use KLdiv's
# vectorization to do lots of JS's at once, unfortunately.

JS_flexmix <- function(P,Q) {
    library(flexmix)
    PQ_mean = (P + Q) / 2
    eps = min(P,Q) / 2    # otherwise KLdiv replaces values less than 10^-4
    result <- (KLdiv(cbind(P,PQ_mean),eps=eps) +
               KLdiv(cbind(Q,PQ_mean),eps=eps)) / 2
    result[1,2]
}

# row_dists
#
# Compute a matrix of distances between the rows of a matrix M.
#
# specify method="pearson" or "spearman" for
# correlations, or "JS" for the Jensen-Shannon divergence given above.
#
# The generic name is a reminder that there are multiple possible
# applications to a hierarchical model; see functions below for examples.

row_dists <- function(M,method="JS") {
    if(method=="pearson" || method=="spearman") {
        return(cor(t(M),method=method))
    } else if(method=="JS") {
        # FIXME failure to vectorize. Ugh.

        n <- nrow(M)
        result <- matrix(0,nrow=n,ncol=n)

        for(i in seq(n)) {
            for(j in i:n) {
                result[i,j] <- JS_divergence(M[i,],M[j,])
            }
        }
        # at least take advantage of the symmetry
        result[lower.tri(result)] <- t(result)[lower.tri(result)]
        return(result)
    } else {
        stop("Unknown method.")
    }
}

# doc_topic_cor
#
# Correlations between TOPICS according to their log proportions in documents
#
# Pass the transpose of doc_topics_frame, which has documents in rows.

doc_topic_cor <- function(doctops) {
    # copy on modify
    doctops$id <- NULL
    row_dists(log(t(doctops)),method="pearson")
}

# topic_divergences
#
# this will give you the J-S divergences between topics considered as
# distributions of words.
#
# tw_wide: the wide format data frame from topic_words_wide.
# TODO why so slow???
#
# OR trainer: the live trainer object. Better.
#
# If you have the topic-word matrix, just call row_dists(M).

topic_divergences <- function(tw_wide=NULL,trainer=NULL) {
    if(!is.null(tw_wide)) {
        # drop the first column (with the topic id); copy on modify
        tw_wide$topic <- NULL
        return(row_dists(tw_wide,method="JS"))
    }
    else if(!is.null(trainer)) {
        mtw <- mallet.topic.words(trainer,smoothed=F,normalized=F)
        # smoothing
        mtw <- mtw + trainer$model$beta
        # normalization
        mtw <- diag(1 / rowSums(mtw)) %*% mtw
        return(row_dists(mtw,method="JS"))
    }
    else {
        stop("Specify either tw (topic_words dataframe) or trainer object.")
    }
}

# -----------------------------------------
# Probing a corpus via the mallet instances
# -----------------------------------------

# instances_tdm
#
# Given an instance list, return a term-document matrix.
#
# Documents are in columns, terms are in rows. The ordering of the terms
# is as in the vocabulary, and the ordering of documents is as in the
# instance list; retrieve the ids in this order with instances_ids().
#
# big: if true, try to build up a sparseMatrix instead of a regular matrix.
# On your head be it. For the idea of going sparse, h/t Ben Marwick.
#
# parallel: if true (when big is true), try to run in parallel using    
# doMC. Not actually workable at scale the way I've written it, so    
# this option is deprecated, and the function will error out instead. 
#
# N.B. Instances typically hold processed text (no stopwords,
# lowercased, etc.)
#

instances_tdm <- function(instances,big=T,parallel=F,verbose=F) {
    if(verbose) {
        log <- message
    }
    else {
        log <- function(...) {NULL}
    }

    nwords <- instances$getAlphabet()$size()
    if (class(instances)=="character") {
        log("Loading instances from ",instances)
        instances <- read_instances(instances)
    }

    instances <- .jevalArray(instances$toArray(),simplify=T) 

    log("Retrieved instances from mallet.")
    log("Compiling tdm...")


    if(big) {

        library(Matrix)

        if(parallel) {
            stop("Serial execution will be better.")

            library(foreach)
            library(doMC)
            registerDoMC()
            log("Executing in parallel with ",getDoParWorkers(),
                " workers")

            result <- foreach(i=instances,
                              .combine=cBind) %dopar% {
                Matrix(tabulate(instance_vector(i),nbins=nwords),
                       ncol=1,sparse=T)
            }
        }
        else {
            # ugh. Solution from:
            # http://stackoverflow.com/questions/8843700/creating-sparse-matrix-from-a-list-of-sparse-vectors
            log("Tabulating instances into sparseVector list")

            instance_tf <- function(inst) {
                counts <- tabulate(instance_vector(inst))
                sparseVector(counts,seq_along(counts),length=nwords)
            }

            vs <- lapply(instances,instance_tf) 
            n_x <- sapply(vs,function(v) length(v@i))

            # If we wanted to be totally vectorial, we could compute a
            # running total of n_x, which tells us how to index into our
            # row and column vectors for each document:
            # 
            #    acc <- matrix(0L,nrow=length(n_x),ncol=length(n_x))
            #    acc[upper.tri[rsum,diag=T]] <- 1L 
            #    indices <- n_x %*% acc
            #
            # and then calculate the a:a+l-1 sequences in advance,
            # but whatevs!

            N <- sum(n_x)
            rs <- integer(N)
            cs <- integer(N)
            xs <- integer(N)

            a <- 1
            for(k in seq_along(vs)) {
                stopifnot(is(vs[[k]],"sparseVector"))
            }

            log("Building sparseMatrix parameters")
            for (k in seq_along(vs)) {
                l <- n_x[k]
                if(l == 0) {
                    next
                }

                elems_k <- a:(a + l - 1)

                cs[elems_k] <- k
                rs[elems_k] <- vs[[k]]@i
                xs[elems_k] <- vs[[k]]@x
                a <- a + l
            }

            log("Constructing sparseMatrix")

            result <- sparseMatrix(i=rs,j=cs,x=xs)
        }
    }
    else { 
        instance_tf <- function(inst) {
            tabulate(instance_vector(inst),nbins=nwords)
        }
        result <- vapply(instances,instance_tf,integer(nwords))
    }
    result
}

# instances_ids
#
# return a vector of id's ("names") of instances,
# in the order mallet keeps them in

instances_ids <- function(instances) {
    iter <- instances$iterator()

    instance_name <- function() {
        inst <- .jcall(iter,"Ljava/lang/Object;","next")
        .jstrVal(.jcall(inst,"Ljava/lang/Object;","getName"))
    }

    replicate(instances$size(),instance_name())
}

# get_instance
#
# retrieve an instance from the instance list by id

get_instance <- function(instances,id,id_map=instances_ids(instances)) {
    j <- match(id,id_map) - 1
    .jcall(instances,"Ljava/lang/Object;","get",as.integer(j))
}

# instance_vector
#
# An instance holds a vector giving the _sequence_ of features,
# zero-indexed. To get a vector we can read off against
# trainer$getVocabulary() in R, we add 1.

instance_vector <- function(instance) {
    fs <- .jcall(instance,"Ljava/lang/Object;","getData")
    .jcall(fs,"[I","getFeatures") + 1
}

# instance_text
#
# The instance is a sequence, so this is how you read it
#
# Repeated calls: this will be much faster if you retrieve the
# vocabulary separately and pass that in. An InstanceList guarantees
# that all Instances have the same vocabulary.

instance_text <- function(instance,
                          vocab=instances_vocabulary(instance),
                          collapse=" ") {
    paste(vocab[instance_vector(instance)],collapse=collapse)
}


# The vocabulary, from the raw instance. If you have the topic model
# trainer object, the vocabulary is retrievable more quickly with an
# RTopicModel method: trainer$getVocabulary().

instances_vocabulary <- function(instances) {
    sapply(.jevalArray(instances$getAlphabet()$toArray()),.jstrVal)
}

# term_year_matrix
#
# Aggregate word counts by years. Pass in a term-document matrix created
# by instances_tdm, as well as the instances object or precalculated
# versions of the remaining parameters
#
# returns a two-element list:
#
# tym: the term-year-matrix (possibly sparse)
#
# yseq: the year sequence represented by the columns. Should be
# sequential (ordered factor) but may not be evenly spaced if any year
# is missing from the data.
#
# Further parameters:
#
# big: if tdm is a sparseMatrix, set to T and the resulting
# term-year-matrix will be sparse too.
#
# id_map: ids in InstanceList order
#
# vocabulary: the vocabulary, as known to the instances

term_year_matrix <- function(metadata,
                             tdm,
                             instances=NULL,
                             id_map=instances_ids(instances),
                             vocabulary=instances_vocabulary(instances),
                             big=T) {
    metadata <- metadata[metadata$id %in% id_map,]
    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    # indicator-matrix version of years
    if(big) {
        library(Matrix)
        Y <- Matrix(0,nrow=length(years),ncol=nlevels(years))
    }
    else {
        Y <- matrix(0,nrow=length(years),ncol=nlevels(years))
    }

    Y[cbind(seq_along(years),years)] <- 1

    result <- tdm %*% Y

    list(tym=result,yseq=levels(years))
}

# term_year_matrix_journal
#
# like term_year_matrix, but sum words only in one journal
#
# journal: a string, to match against metadata$journaltitle (so be careful 
# about those trailing "\t"s)

term_year_matrix_journal <- function(journal,
                                     metadata,
                                     tdm,
                                     id_map,
                                     vocabulary) {

    metadata <- metadata[metadata$id %in% id_map,]
    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    jm <- Diagonal(n=ncol(tdm),x=(journals==journal))

    term_year_matrix(metadata,
                     tdm=tdm %*% jm,
                     id_map=id_map,
                     vocabulary=vocabulary,
                     big=T)
}

# journal_year_matrix
#
# total wordcounts per journal per year 

journal_year_matrix <- function(tdm,metadata,id_map) {
    metadata <- metadata[metadata$id %in% id_map,]

    dates <- pubdate_Date(metadata$pubdate)
    names(dates) <- metadata$id
    dates <- dates[id_map]

    years <- cut(dates,breaks="years",ordered=T)
    years <- droplevels(years)

    journals <- metadata$journaltitle
    names(journals) <- metadata$id
    journals <- journals[id_map]
    journals <- factor(journals,ordered=T)

    library(Matrix)
    Y <- Matrix(0,nrow=length(years),ncol=nlevels(years)) 
    Y[cbind(seq_along(years),years)] <- 1

    result <- matrix(nrow=nlevels(journals),ncol=nlevels(years))

    # The fully algebraic way would be to construct some block matrices
    # to do these sums, but since the number of journals is small, it's
    # not worth it

    Csum <- Matrix(rep(1,nrow(tdm)),nrow=1)

    for(i in seq_along(levels(journals))) {
        jrnl <- levels(journals)[i]

        jm <- Diagonal(n=ncol(tdm),x=(journals==jrnl))
        m <- tdm %*% jm
        tym <- m %*% Y

        result[i,] <- drop(Csum %*% tym)
    }

    rownames(result) <- levels(journals)
    colnames(result) <- levels(years)
    result
}

# tdm_tm
#
# Convert a term-document sparseMatrix to the tm package's format.
#
# If tf_idf is TRUE, also replaces entries with tf-idf scores.

tdm_tm <- function(tdm,tf_idf=T) {
    library(tm)
    if(tf_idf) {
        result <- as.TermDocumentMatrix(tdm,weighting=weightTfIdf)
    } else  {
        # weightTf is just the identity function
        result <- as.TermDocumentMatrix(tdm,weighting=weightTf)
    }
    result
}

# Converting the tm package object back down to a regular sparseMatrix
# Because they don't provide an "as" method themselves

TermDocument_sparse <- function(tdm) {
    sparseMatrix(i=tdm$i,j=tdm$j,x=tdm$v,dims=c(tdm$nrow,tdm$ncol))
}


