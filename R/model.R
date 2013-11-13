# internal use only
# reload mallet and rJava with a new heap parameter for java
# no-op if java_heap is NULL
#
.reload_mallet <- function(java_heap=NULL) {
    if(!is.null(java_heap)) {
        message("Reloading mallet with rJava heap setting -Xmx ",java_heap)
        options(java.parameters=paste("-Xmx",java_heap,sep=""))
        detach("package:mallet",unload=T,character.only=T)
        detach("package:rJava",unload=T,character.only=T)
        library("mallet")
    }
}

#' Make a topic model of DfR documents
#'
#' The basic usage of this package is wrapped up in this convenience
#' function.
#'
#' Given wordcount and metadata files, this function sets up MALLET
#' inputs and then runs MALLET to produce a topic model. For
#' finer-grained control over the mallet inputs or to tweak more
#' modeling parameters, follow the order of calls in this function.
#' Invoke \code{\link{make_instances}} separately and pass the results
#' to \code{\link{train_model}}.  
#'
#' If java gives out-of-memory errors, try passing a large value in the \code{java_heap} 
#' parameter (\code{java_heap="4g"}, say).
#'
#' @return A list with the following members:
#' \describe{
#' \item{\code{metadata}}{dataframe of metadata}
#' \item{\code{doc_topics}}{dataframe with documents in rows, topics in columns, and an 
#' extra column with document id's}
#' \item{\code{wkf}}{data frame of the weightiest words in each topic (as returned by 
#' \code{\link{weighted_keys_frame}}); also stores topic alphas}
#' \item{\code{trainer}}{reference to the \code{RTopicModel} object, which, with the LDA 
#' run complete, holds the sampling state and references to the instance-list}
#' \item{\code{seed}}{the random-number seed, or \code{NULL} to use default seeding}
#' }
#'
#' @param citations_files character vector with names of DfR \code{citations.CSV} metadata 
#' files 
#' @param dirs character vector with names of directories holding \code{wordcounts*.CSV} 
#' files
#' @param stoplist_file name of stoplist (containing one stopword per line)
#' @param num_topics number of topics to model
#' @param seed integer random number seed for mallet
#' @param num_top_words integer number of "key words" per topic
#' @param java_heap if non-null, java is restarted with this heap parameter
#' @seealso
#' \code{\link{read_metadata}},
#' \code{\link{read_dfr_wordcounts}},
#' \code{\link{make_instances}},
#' \code{\link{train_model}},
#' \code{\link{doc_topics_frame}},
#' \code{\link{weighted_keys_frame}},
#' \code{\link{output_model}}
#' @examples
#' \dontrun{model_documents("citations.CSV","wordcounts","stoplist.txt",50)}
#'
#' @export
#'
model_documents <- function(citations_files,dirs,stoplist_file,num_topics,
                            seed=NULL,num_top_words=50L,java_heap=NULL) { 
    .reload_mallet(java_heap=java_heap)
    mf <- read_metadata(citations_files)
    texts <- read_dfr_wordcounts(dirs=dirs)
    instances <- make_instances(texts,stoplist_file)
    model <- train_model(instances,num_topics=num_topics,seed=seed)
    doc_topics <- doc_topics_frame(model,smoothed=F,normalized=F)
    keys <- weighted_keys_frame(model,num_top_words=as_integer(num_top_words),
                                smoothed=F,normalized=F)

    list(metadata=mf,
         doc_topics=doc_topics,
         wkf=keys,
         trainer=model,
         seed=seed)
}

#' A convenience function for saving all the model outputs at once.
#'
#' Save a series of files with the results of an LDA run.
#'
#' The following files are written to \code{output_dir}:
#' \describe{
#' \item{\code{topic_words.csv}}{unnormalized topic-word matrix, CSV format}
#' \item{\code{vocab.txt}}{list of words (same order as columns of topic-word matrix), one 
#' per line}
#' \item{\code{params.csv}}{CSV with one row of data holding miscellaneous model 
#' parameters}
#' \item{\code{keys.csv}}{topic key words CSV; see \code{\link{weighted_keys_frame}} for 
#' the format}
#' \item{\code{doc_topics.csv}}{document-topic matrix CSV; see
#' \code{\link{doc_topics_frame}} for the format}
#' \item{\code{mallet_state.gz}}{MALLET sampling state (a big file)}
#' \item{\code{diagnostics.xml}}{MALLET model diagnostics}
#' \item{\code{instances.mallet}}{save the source text "instances" file (not done by
#' default)}
#' \item{\code{topic_scaled.csv}}{CSV with scaled 2D coordinates for the topics. Obtained 
#' by applying \code{\link{cmdscale}} to a matrix of topic divergences calculated by 
#' \code{\link{topic_divergences}}}
#' }
#'
#' @param model_result the result from \code{\link{model_documents}}, or, equivalently, a 
#' list with 
#' elements called \code{trainer}, \code{doc_topics}, \code{wkf}, and \code{seed}.
#'
#' @param output_dir where to save all the output files.
#'
#' @param save_instances if TRUE, extract the instance list from the trainer object and save it to \code{instances.mallet}; if FALSE (the default), don't
#'
#' @param save_scaled if TRUE (the default), write a file of 2D coordinate for the topics
#'
#' @export
#'
output_model <- function(model_result,output_dir=".",
                         save_instances=F,save_scaled=T) {
    tw_f <- file.path(output_dir,"topic_words.csv")
    vocab_f <- file.path(output_dir,"vocab.txt")
    write_topic_words(model_result$trainer,
                      topic_words_file=tw_f,
                      vocab_file=vocab_f,
                      smoothed=F, normalized=F)
    message("Wrote ",tw_f)
    message("Wrote ",vocab_f)

    params <- model_params(model_result$trainer)
    params$seed <- model_result$seed
    params_f <- file.path(output_dir,"params.csv")
    write.csv(params,params_f)
    message("Wrote ",params_f)

    keys_f <- file.path(output_dir,"keys.csv")
    write.table(model_result$wkf,keys_f,
                quote=F,sep=",",row.names=F,col.names=T)
    message("Wrote ",keys_f)

    dt_f <- file.path(output_dir,"doc_topics.csv")
    write.table(model_result$doc_topics,
                dt_f,
                quote=F,sep=",",row.names=F,col.names=T)
    message("Wrote ",dt_f)

    state_f <- file.path(output_dir,"mallet_state.gz")
    write_mallet_state(model_result$trainer,state_f)
    message("Wrote ",state_f)

    diag_f <- file.path(output_dir,"diagnostics.xml") 
    write_diagnostics(model_result$trainer,
                      diag_f, 
                      num_top_words=as.integer(sum(model_result$keys$topic==1)))
    message("Wrote ",diag_f)

    if (save_instances) {
        inst_f <- file.path(output_dir,"instances.mallet")
        write_instances(model_result$trainer$instances,inst_f)
        message("Wrote ",inst_f)
    }

    if (save_scaled) {
        scaled_f <- file.path(output_dir,"topic_scaled.csv")
        scaled <- cmdscale(topic_divergences(
            mallet.topic.words(model_result$trainer,
                               smoothed=F,normalized=F),
            params$beta),
                           k=2)
        write.table(scaled,scaled_f,
                    quote=F,sep=",",row.names=F,col.names=F)
        message("Wrote ",scaled_f)
    }
}

#' Convert DfR wordcount files to a single dataframe
#'
#' This function reads in \code{wordcounts*.CSV} files and produces a
#' dataframe with one line for each document. A big waste of memory, but a simple 
#' way to get these files into MALLET.
#'
#' @param dirs character vector of directories containing \code{wordcounts*.CSV} files
#' @param files individual filenames to read.
#' @return a dataframe with one line for each document with two fields, the document id 
#' (from the filename) and the "text" of the document as an inflated bag of words.
#'
#' @export
#' 
read_dfr_wordcounts <- function(dirs=NULL,files=NULL) {
    counts <- read_dfr(dirs=dirs,files=files)
    docs_frame(counts)
}

#' Convert DfR wordcount files to a long-format data frame
#'
#' Reads in a bunch of \code{wordcounts*.CSV} files and stacks them up in a
#' single "long format" dataframe. Invoked by
#' \code{\link{read_dfr_wordcounts}}.
#'
#' Empty documents are skipped; DfR supplies wordcounts files
#' for documents that have no wordcount data. These will be in DfR's
#' metadata but not in the output dataframe here.
#'
#' This is slow. An outboard script in python or Perl is
#' faster, but this keeps us in R and does everything in memory.
#' Memory usage: for N typical journal articles, the resulting dataframe
#' needs about 20N K of memory. So R will hit its limits somewhere around
#' 100K articles of typical length.
#'
#' @param dirs character vector of directories containing \code{wordcounts*.CSV} files
#' @param files individual filenames to read.
#' @return A dataframe with three columns: \code{id}, the document ID; 
#' \code{WORDCOUNTS}, a feature counted by JSTOR (i.e. a word type); \code{WEIGHT}, the 
#' count.
#' @seealso
#' \code{\link{read_dfr_wordcounts}}
#'
#' @export
#' 
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

    data.frame(id=rep(filename_id(fv),times=n_types),
               WORDCOUNTS=wordtype,
               WEIGHT=wordweight,
               stringsAsFactors=F)

}

#' Calculate total corpus-wide feature counts
#'
#' Given a wordcounts long-format dataframe returned by \code{\link{read_dfr}},
#' calculate total
#' corpus-wide counts for each word type
#'
#' @param counts The dataframe from \code{\link{read_dfr}}
#' @return a 1D table, i.e. a vector of counts with word types as the
#' element names
#' @seealso \code{\link{read_dfr}}
#'
#' @export
#' 
overall_counts <- function(counts) {
    # the dumb way is surprisingly fast (lazy evaluation?)
    # whereas ddply(counts,.(WORDCOUNTS),summarize,count=sum(WEIGHT))
    # is very slow
    with(counts,table(rep(WORDCOUNTS,times=WEIGHT)))
}

#' Throw out words whose frequency in the corpus is below a threshold
#'
#' For filtering wordcounts before building MALLET instances.
#'
#' @param counts long-form dataframe as returned by \code{\link{read_dfr}}
#' @param freq_threshold frequency threshold (NULL, or a number between 0 and 1)
#' @param rank_threshold rank threshold (natural number): used if \code{freq_threshold} 
#' is NULL
#' @param .overall precalculated overall counts (if NULL, \code{\link{overall_counts}} is 
#' invoked)
#'
#' @return A filtered feature-counts dataframe
#' 
#' @export
#' 
remove_rare <- function(counts,freq_threshold=NULL,rank_threshold=NULL,
                        .overall=NULL) { 
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
                        
#' Convert long-format feature-counts into documents
#'
#' Naively "inflates" feature counts into a bag of words, for sending to MALLET.
#'
#' @param counts long-format data frame like that returned by \code{\link{read_dfr}}
#'
#' @return a dataframe with two columns: \code{id}, the document id; \code{text}, the full 
#' document text (but with the words in meaningless order)
#'
#' @seealso \code{\link{read_dfr_wordcounts}}
#' 
#' @export
#' 
docs_frame <- function(counts) {
    ddply(counts,.(id),summarize,
          text=paste(rep(WORDCOUNTS,times=WEIGHT),collapse=" "))
}

#' Create MALLET instances from a document frame
#' 
#' Given a frame like that returned by \code{\link{docs_frame}},
#' create a MALLET \code{InstanceList} object
#'
#' @param docs data frame with \code{id} and \code{text} columns
#' @param stoplist_file passed on to MALLET
#' @param java_heap if non-null, java is restarted with this heap parameter
#' @param ... passed on to \code{\link[mallet]{mallet.import}}
#' @return an rJava reference to a MALLET \code{InstanceList}
#' @seealso \code{\link{train_model}}
#' \code{\link{write_instances}}
#'
#' @export
#' 
make_instances <- function(docs,stoplist_file,java_heap=NULL,...) {
    .reload_mallet(java_heap)

    # token regex: letters only, by default
    # another possibility would be to include punctuation \p{P}
    mallet.import(docs$id,docs$text,
                  stoplist.file=stoplist.file,
                  ...)
}

#' Save a mallet InstanceList object to a file
#'
#' Saves mallet instances to disk using MALLET's file writer. The result is then 
#' equivalent to \code{mallet import-dirs} or similar at the command line.
#' @param instances reference to the \code{InstanceList}
#' @param output_file filename
#'
#' @seealso \code{\link{read_instances}},
#' \code{\link{make_instances}}
#'
#' @export
#' 
write_instances <- function(instances,output_file="instances.mallet") {
  instances$save(new(J("java.io.File"),output_file))
}

#' Read a mallet \code{InstanceList} object from a file
#'
#' Reads a mallet \code{InstanceList} object from a file.
#'
#' @param filename the instance file
#' @return a reference to the MALLET \code{InstanceList} object
#' @seealso \code{\link{write_instances}},
#' \code{\link{make_instances}}
#' \code{\link{train_model}}
#'
#' @export
#' 
read_instances <- function(filename) {
    J("cc.mallet.types.InstanceList","load",new(J("java.io.File"),
                                                path.expand(filename)))
}

#' Train a topic model
#'
#' Invokes MALLET's parallel topic modeling algorithm.
#'
#' @param instances either an rJava reference to an \code{InstanceList} object or the 
#' name of a MALLET instances file
#' @param num_topics how many topics to train?
#' @param alpha_sum initial sum of hyperparameters \eqn{alpha_k}: priors of topics over 
#' document
#' @param beta initial value of hyperparameter \eqn{\beta}: prior of topics over words
#' @param n_iters number of Gibbs sampling iterations to run
#' @param n_max_iters number of "iterated conditional modes"
#' @param optimize_hyperparameters if TRUE (the default), optimize \eqn{\alpha_k} and 
#' \eqn{\beta}. If FALSE, the value of \code{symmetric_alpha} is ignored.
#' @param n_hyper_iters how often to do hyperparameter optimization
#' @param n_burn_in number of initial "burn-in" iterations before hyperparameter 
#' optimization
#' @param symmetric_alpha if FALSE (the default), allow the
#' \eqn{\alpha_k} to be different from one another. If TRUE when
#' \code{optimize_hyperparameters} is TRUE, then the sum of the alphas
#' will still be varied by the algorithm, but all the \eqn{\alpha_k}
#' will be the same.
#' @param threads number of threads to run in parallel. 
#' @param seed MALLET's random number seed: set this to ensure a reproducible model.
#' instances: can either be a mallet instances object or the name of a
#' mallet instances file
#' @param java_heap if non-null, java is restarted with this heap parameter
#'
#' @return the trainer object, which holds a reference to the \code{RTopicModel}
#' object constructed by \code{\link{mallet:MalletLDA}}. In order to access the full 
#' MALLET Java API, use this object's reference to the \code{ParallelTopicModel}, which is 
#' simply \code{trainer$model}. You can then call all the \code{ParallelTopicModel} 
#' methods, though Java's strict typing means you'll have some funtimes with
#' \code{\link{rJava:.jcall}}. To index into array-like objects from Java, apply 
#' \code{\link{base:as.integer}} to parameters.
#'
#' @export
#'
#' @seealso \code{\link{make_instances}},
#' \code{\link{make_instances}},
#' \code{\link{model_documents}},
#' \code{\link{output_model}}
#' 
train_model <- function(instances,num_topics,
                        alpha_sum=5,beta=0.01,
                        n_iters=200,
                        n_max_iters=10,
                        optimize_hyperparameters=T,
                        n_hyper_iters=20,
                        n_burn_in=50,
                        symmetric_alpha=F,
                        threads=4L,
                        seed=NULL,
                        java_heap=NULL) {

    .reload_mallet(java_heap)

    trainer <- MalletLDA(num_topics,alpha_sum,beta)
    trainer$model$setNumThreads(as.integer(threads))
    if(!is.null(seed)) {
        trainer$model$setRandomSeed(as.integer(seed))
    }

    trainer$loadDocuments(instances)

    if(optimize_hyperparameters) {
        trainer$model$setSymmetricAlpha(symmetric_alpha)
        trainer$setAlphaOptimization(n_hyper_iters,n_burn_in)
    }
    else {
        trainer$setAlphaOptimization(0,0)
    }

    trainer$train(n_iters)
    # following from dmimno's mallet-example.R:
    # iterate picking "best" (?) topic for each token instead of sampling
    # from posterior distribution (?)
    trainer$maximize(n_max_iters)
    trainer
}

#' Get the document-topic data frame
#'
#' Create a data frame with topic proportions for each document in the
#' first n columns and document ids in the last column.
#'
#' This is the document-topic matrix with an extra column to make merging against metadata 
#' easier. 
#'
#' @param trainer The \code{RTopicModel} object, after a training run
#' @param smoothed if TRUE, smooth document-topic distribution using hyperparameters 
#" \eqn{\alpha_k}
#' @param normalized if TRUE, weights sum to 1
#' @return A dataframe with columns \code{topic1,topic2,...,topicN,id}.
#'
#' @export
#'
doc_topics_frame <- function(trainer,smoothed=F,normalized=F) { 
    # matrix of topic proportions (cols) in docs (rows)
    # smoothing means nothing has 0 prob.
    # normalized instead of raw counts 
    doc.topics <- mallet.doc.topics(trainer,smoothed=smoothed,
                                    normalized=normalized)

    doc.frame <- as.data.frame(doc.topics) 
    names(doc.frame) <- paste("topic",sep="",seq(trainer$model$numTopics))
    cbind(doc.frame,id=trainer$getDocumentNames(),stringsAsFactors=F)
}

#' Extract the numerical part of a doc_topics frame
#'
#' A convenience function for extracting the numerical part of a doc_topics frame, 
#' discarding the document id's.
#'
#' If you have the trainer object, you can simply use 
#' \code{\link{mallet:mallet.doc.topics}}.
#'
#' @param doctopics the data frame from \code{\link{doc_topics_frame}}
#' @seealso \code{\link{mallet:mallet.doc.topics}},
#' \code{\link{doc_topics_frame}}
#'
#' @export
#'
doc_topics_matrix <- function(doctopics) {
    as.matrix(doctopics[,-ncol(doctopics)])
}

#' Normalize columns to sum to one
#'
#' Normalize columns to sum to one
#'
#' A convenience function for expressing the process of normalizing the doc-topic matrix. 
#' Actually valid for any matrix in which no column is entirely zero.
#'
#' @param dtm the doc_topics matrix (or any other matrix with weights in columns)
#'
#' @return a matrix
#'
#' @export
#'
normalize_doc_topics <- function(dtm) {
    dtm <- dtm %*% diag(1 / colSums(dtm))
}

#' Get a melted document-topic-metadata frame
#'
#' Synthesizes a \code{\link{doc_topics_frame}} with metadata into a "long" format
#'
#' @param doctops frame with document-topic weights and an id column
#' @param metadata frame
#' @param meta_keep vector of names of columns of metadata to keep
#'
#' @return a data frame with an id column, one column for each element of 
#' \code{meta_keep}, a \code{variable} column with \code{topic<n>} labeling the topic, and 
#' a \code{value} column with the topic score
#'
#' @seealso \code{\link{plyr:melt}}, \code{\link{doc_topics_frame}}, 
#' \code{\link{doc_topics_wide}},
#' \code{\link{read_metadata}}
#'
#' @export
#'
doc_topics_long <- function(doctops,metadata,
                            meta_keep=c("pubdate","journaltitle")) {
    library(reshape2)
    meta <- unique(c("id",meta_keep))
    wide <- merge(doctops,metadata[,meta],by="id")
    melt(wide,id.vars=meta)
}

#' Get a wide document-topic-metadata frame
#'
#' Synthesizes a \code{\link{doc_topics_frame}} with metadata into a "wide" format
#'
#' @param doctops frame with document-topic weights and an id column
#' @param metadata frame
#' @param meta_keep vector of names of columns of metadata to keep
#'
#' @return a data frame with an id column, one column for each element of 
#' \code{meta_keep}, and \code{topic<n>} columns with topic weights
#'
#' @seealso \code{\link{plyr:melt}}, \code{\link{doc_topics_frame}}, 
#' \code{\link{doc_topics_long}},
#' \code{\link{read_metadata}}
#'
#' @export
#'
doc_topics_wide <- function(doctops,metadata,
                            meta_keep="pubdate") {
    meta_keep <- unique(c("id",meta_keep))
    merge(doctops,metadata[,meta_keep],by="id")
}

#' The yearly totals of topic weights
#'
#' Tots up the total of each topic for each year, given documents and date metadata
#'
#' Either a long-form or a
#' wide-form data frame may be passed in; the wide form can be handled much
#' (orders of magnitude) faster. Formerly called \code{tm_yearly_totals}, but that's silly.
#'
#' @param dt_wide doc_topics frame with attached metadata from 
#' \code{\link{doc_topics_wide}}
#' @param dt_long doc_topics frame with attached metadata from 
#' \code{\link{doc_topics_long}}
#' @return a matrix with: rows containing topic totals, columns
#' containing years present in the data, colnames with strings
#' representing dates.
#'
#' @export
#'
#' @seealso
#' \code{\link{doc_topics_wide}},
#' \code{\link{doc_topics_long}}
#'
topic_year_matrix <- function(dt_wide=NULL,dt_long=NULL) {
    if(!is.null(dt_wide)) {
        dt_wide$pubdate <- cut(pubdate_Date(dt_wide$pubdate),breaks="years")
        dt_wide$pubdate <- droplevels(dt_wide$pubdate)
        dt_wide$id <- NULL

        # Here, assume that the wide matrix has topic scores in all but
        # the last column, which holds the pubdate. daply will stick the
        # pubdate back on the front when it splits the frame by years.
        # The result will have topics in columns, so transpose.

        topic_sum <- function (d) {
            colSums(subset(d,select=-pubdate))
        }
        t(daply(dt_wide,"pubdate",topic_sum))
    }
    else if(!is.null(dt_long)) {
        # copy on modify
        dt_long$pubdate <- cut(pubdate_Date(dt_long$pubdate),breaks="years")
        dt_long$pubdate <- droplevels(dt_long$pubdate)
        dt_long$id <- NULL
        totals <- ddply(dt_long,c("variable","pubdate"),summarize,
                        total=sum(value))
        acast(totals,variable ~ pubdate,value.var="total")
    }
    else {
        stop("Supply either long or wide-form document-topic matrix")
    }
} 

#' Get a dataframe suitable for plotting time series of topics
#'
#' Produces a dataframe suitable for plotting time series of topics on the basis of the 
#' yearly totals of topics.
#'
#' @param yearly the topic-year matrix, with dates as colnames
#' @param topics which topics to keep in the dataframe: by default, all are retained
#' @param denominator the totals to divide the yearly series by. By default, the sums of 
#' the columns of \code{yearly} are used, so that the resulting dataframe gives the 
#' proportion of \emph{words} in the corpus for that year assigned to the topic in 
#' question. 
#' @param rolling_window the number of years to take rolling averages over (default 1)
#' @return a dataframe with columns \code{topic,year,weight}
#' @export
#'
topic_proportions_series_frame <- function(yearly,
                                           topics=1:nrow(yearly),
                                           denominator=colSums(yearly),
                                           rolling_window=1) {
    yseq <- colnames(yearly)

    z <- series_rolling(series=yearly_zoo(yearly[topics,,drop=F]),
                       totals=zoo(denominator,as.Date(yseq)),
                       k=rolling_window)

    yearly_series_frame(yearly=zoo_yearly(z),
                        var_seq=topics,
                        series_names=c("topic","year","weight"))

}

#' Convert a matrix of time series to a dataframe
#'
#' Utility wrapper for \code{\link{plyr:melt}} on matrices with parallel time series in 
#' rows.
#'
#' @param yearly matrix with variables in rows and time measurements in columns
#' @param total if TRUE, sum over rows to produce a single total series
#' @param year_seq character vector giving the dates the columns of \code{yearly} 
#' correspond to (by default, assume these are stored in \code{\link{colnames}(yearly)})
#' @param var_seq character vector giving names of variables in rows of \code{yearly}
#' @param series_names column names for final dataframe
#' @return three-column data frame ("long" form)
#' @seealso TODO vis,
#' \code{\link{topic_proportions_series_frame}}
#'
#' @export
#'
yearly_series_frame <- function(yearly,
                                total=F,
                                year_seq=colnames(yearly),
                                var_seq=rownames(yearly),
                                series_names=c("word","year","weight")) {

    if (total) {
        yearly <- matrix(colSums(yearly),nrow=1)
        if(length(var_seq) != 1) {
            message('Name for total not well-specified; using "total"')
            var_seq <- "total"
        }
    }

    rownames(yearly) <- var_seq
    colnames(yearly) <- year_seq
    series <- melt(as.matrix(yearly)) # Force conversion from any sparseMatrix
    names(series) <- series_names
    series$year <- as.Date(series[,2])

    series
}

#' Convert a yearly series data frame into a zoo object
#'
#' Utility function for swapping between the long form dataframe of 
#' \code{\link{yearly_series_frame}} and a \pkg{zoo} object.
#'
#' @param s three-column data frame
#' @param date_col index of date column (default 2)
#' @param value_col index of value column (default 3)
#' @return a \code{zoo} time series object
#'
#' @seealso
#' \code{\link{yearly_series_frame}}, \pkg{zoo}
#'
#' @export
#'
series_frame_zoo <- function(s,date_col=2,value_col=3) {
    val_var <- names(s)[value_col]
    date_var <- names(s)[date_col]
    category_var <- names(s)[-c(value_col,date_col)]

    s_m <- acast(s,as.formula(paste(category_var,"~",date_var)),
                 value.var=val_var)
    
    zoo(t(s_m),as.Date(colnames(s_m)))
}

#' Convert a matrix of yearly values into a zoo object
#'
#' @param yearly a matrix with variables in rows and dates in string form in its 
#' \code{colnames}
#' @return a \code{\link{zoo:zoo}} object
#'
#' @seealso \code{\link{zoo_yearly}}
yearly_zoo <- function(yearly) {
    zoo(t(yearly),as.Date(colnames(yearly)))
}

#' Convert a zoo object into a matrix with variables in rows
#'
#' @param z a \code{\link{zoo:zoo}} object
#' @return a matrix with variables in rows and dates in string form in its 
#' \code{colnames}
#'
#' @seealso \code{\link{yearly_zoo}}
#'
#' @export
zoo_yearly <- function(z) {
    as.matrix(t(z))
}

#' Convert a zoo object into a "long" data frame
#'
#' @param z a \code{\link{zoo:zoo}} object
#' @param series_names the names for the three columns of the result
#' @return the data frame from \code{\link{yearly_series_frame}}
#' @seealso \code{\link{yearly_series_frame}},
#' \code{\link{zoo_yearly}}
#'
#' @export
zoo_series_frame <- function(z,series_names=c("word","year","weight")) {
    yearly_series_frame(zoo_yearly(z),series_names=series_names)
}

#' Take rolling weighted averages
#'
#' This takes rolling averages where adjacent time-slices are not given equal weights.
#'
#' @param the series (a \code{\link{zoo:zoo}} object, which can contain one or many 
#' variables)
#' @param totals the denominators used in weighting (must be parallel to \code{series})
#' @param k the window to take the rolling average in
#' @return a \code{\link{zoo:zoo}} object with the averaged values
#'
#' @seealso \code{\link{rollapply}}
series_rolling <- function(series,totals,k) {
    rollapply(series,k,sum) / rollapply(totals,k,sum)
}

#' Tally up document-topic proportions, conditional on metadata
#'
#' Like \code{\link{topic_year_matrix}}, this tallies up document-topic proportions over #' yearly time-slices but allows you 
#' to split out the yearly totals by, e.g., \code{journaltitle}
#'
#' @param doctops the document-topic data frame with id column (from 
#' \code{\link{doc_topics_frame}}
#'
#' @param metadata the metadata frame, or a subset of its columns
#'
#' @param topic_year the result of \code{\link{topic_year_matrix}}, used for normalizing 
#' within each year. Pass \code{NULL} if you want raw counts instead.
#'
#' @param vars metadata columns to split by; by default, use all supplied metadata columns
#'
#' @return a data frame suitable for plotting, where each row gives yearly 
#' totals for each topic for a given metadata combination. The topic 
#' proportion columns are called \code{topic1}, \code{topic2}, etc.
#'
#' @export
#' @seealso \code{\link{topic_year_matrix}}
#'
topic_year_meta <- function(doctops,metadata,
                            topic_year=NULL,vars=NULL) { 
    if(is.null(vars)) {
        vars <- names(metadata)
    }
    vars <- unique(c("id","pubdate",vars))
    doctops <- merge(doctops,metadata[,vars],by="id")
    doctops$pubdate <- cut(pubdate_Date(doctops$pubdate),breaks="years")
    doctops$pubdate <- droplevels(doctops$pubdate)
    doctops$id <- NULL
    vars <- vars[vars != "id"]

    drop_cols <- match(vars,names(doctops))

    tally <- function (d) {
        colSums(d[,-drop_cols])
    }

    if(is.null(yearly_totals)) {
        ply_fun <- tally
    }
    else {
        ply_fun <- function(d) {
            yr_col <- match(d$pubdate[1],colnames(topic_year))
            tally(d) / sum(topic_year[,yr_col])
        }
    }

    ddply(doctops,vars,.fun=ply_fun)
}


#' Topic key words with weights
#'
#' A more informative topic key-words data frame, in "long" format.
#'
#' "Key words" are identified simply by rank order of weight within the topic.
#' The result also gives the estimated alpha hyperparameter value for each topic.
#'
#' @param trainer reference to the \code{RTopicModel} object
#' @param n_top number of "top words" for each topic
#' @param smoothed if TRUE, smooth document-topic distribution using hyperparameters 
#" \eqn{\alpha_k}
#' @param normalized if TRUE, weights sum to 1
#' @return a data frame with \code{n} rows for each topic and four columns, \code{alpha,
#' topic,word,weight}. \code{alpha} is repeated \code{n} times in each topic: it gives the 
#' topic hyperparameter \eqn{\alpha_k}. \code{topic} is numbered from 1. The returned 
#' words are in rank order within each topic.
#'
#' @seealso \code{\link{mallet:mallet.topic.words}}
#' \code{\link{tw_wkf}}
#'
#' @export
#'
weighted_keys_frame <- function(trainer,n_top=50,
                                smoothed=F,normalized=F) {
    tw <- mallet.topic.words(trainer,
                             smoothed=smoothed,
                             normalized=normalized)
    tw_wkf(tw,
           vocab=trainer$getVocabulary(),
           alpha=trainer$getAlpha(),
           n_top=n_top)
}

#' Topic key words from the topic-word matrix
#'
#' Given a topic-word matrix, produce the weighted key-word dataframe.
#'
#' This gives the same result as \code{\link{weighted_keys_frame}} but can be used when
#' you no longer have the reference to the \code{RTopicModel} but have saved the topic-word
#' matrix.
#'
#' @param tw a matrix (not dataframe) with topics in rows and word weights in
#' columns
#'
#' @param vocab a character vector with words in the same order as the columns of \code{tw}
#'
#' @param alpha a vector of \eqn{\alpha_k} values for the topics
#'
#' @param n_top number of top key words to store per topic
#'
#' @return a data frame with \code{n} rows for each topic and four columns, \code{alpha,
#' topic,word,weight}. \code{alpha} is repeated \code{n} times in each topic: it gives the 
#' topic hyperparameter \eqn{\alpha_k}. \code{topic} is numbered from 1. The returned words are 
#' in rank order within each topic.
#'
#' @seealso \code{\link{mallet:mallet.topic.words}},
#' \code{\link{weighted_keys_frame}}
#'
#' @export
#'
tw_wkf <- function(tw,vocab,alpha,n_top=50) {
    n <- nrow(tw)
    reps <- rep(n_top,n)

    result <- data.frame(
        topic=rep(seq(n),times=reps),
        alpha=rep(alpha,times=reps),
        word=character(n * n_top),
        weight=numeric(n * n_top),
        stringsAsFactors=F)

    for(i in seq(n)) {
        rows <- 1 + (((i - 1) * n_top) :  ((i * n_top) - 1))
        js <- order(tw[i,],decreasing=T)[1:n_top]
        result$weight[rows] <- tw[i,js]
        result$word[rows] <- vocab[js]
    }
    result
}

topic_word_scores <- function(tw,b,method="blei_lafferty") {
    if(method != "blei_lafferty") {
        stop("I know only the blei_lafferty scoring method.")
    }


    # score(t,v) = p(t,v) log (p(t,v) / Prod_k p(k,v) ^ 1 / K)
    #            = p(t,v) ( log p(t,v) - (1 / K) log( Prod_k p(k,v) ) )
    #            = p(t,v) ( log p(t,v) - (1 / K) Sum_k (log p(k,v) ) )

    n <- nrow(tw)
    V <- ncol(tw)

    # smooth + normalize weights
    topic_totals <- rowSums(tw) + V * b
    tw <- tw + b

    tw <- Diagonal(x=1 / topic_totals) %*% tw

    # ...
    log_tw <- log(tw)

    # calculate down-weighting factor for each word
    word_factor <- tw %*% Diagonal(x=colSums(log_tw) / n)

    tw * (log_tw) - word_factor
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

# term_year_series_frame
#
# make a dataframe, suitable for plotting, of entries from a term-year
# matrix
#
# words: which words to pick out
#
# term_year: the term-year spareMatrix (included in list returned by
# term_year_matrix)
#
# year_seq: the years corresponding to columns of term_year, as strings
# in ISO format (in list from term_year_matrix)
#
# vocab: the vocabulary corresponding to rows of term_year
#
# raw_counts: return counts, or yearly proportions?
#
# total: if true, tally up the total incidences of all words in words
#
# denominator: if raw_counts=F, you can divide through by this
# instead of by the column totals of term_year (useful if you
# are passing in a term_year_topic_matrix but you still want the
# yearly proportion out of all words in the corpus, in which case
# denominator=term_year_matrix()$tym. WARNING: assumes the columns of
# denominator correspond to the same years as those of term_year


term_year_series_frame <- function(words,term_year,year_seq,vocab,
                                   raw_counts=F,
                                   total=F,
                                   denominator=NULL) {
    w <- match(words,vocab)
    if(any(is.na(w))) {
        message("Dropping words missing from vocabulary: ",
                 paste(words[is.na(w)],collapse=" "))
        words <- words[!is.na(w)]
        w <- w[!is.na(w)]
     }

    # TODO use yearly_series_frame to factor this out
    wts <- term_year[w,,drop=F]
    if(!raw_counts) {
        if(is.null(denominator)) {
            denominator <- colSums(term_year)
        }
        wts <- wts %*% diag(1 / denominator) 
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

    # TODO use yearly_series_frame to factor this out too
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

#' Read in the topic-word matrix
#'
#' Get the matrix with topics in rows and word counts in columns.
#' Returns a \code{\link{Matrix:sparseMatrix}}.
#'
#' Formerly known as \code{read_topic_words_matrix}.
#'
#' @return A \code{\link{Matrix:sparseMatrix}} with topics in rows and columns in 
#' order of the vocabulary as known to the mallet instances)
#'
#' @param tw_file CSV filename, for example \code{topic_words.csv}.
#'
#' @param what datatype to read in (passed on to \code{\link{base:scan}}). 
#' \code{integer()} by default; use double() you expect proportions.
#'
#' @seealso
#' \code{\link{write_topic_words}},
#' \code{\link{mallet:mallet.topic.words}} for online access to the same matrix.
#'
#' @export
#'
read_topic_words <- function(tw_file,what=integer()) {
    tw <- scan(tw_file,what=what,sep=",")
    n <- length(scan(tw_file,what=what,sep=",",nlines=1,quiet=T))
    as(matrix(tw,byrow=T,ncol=n),"sparseMatrix")
}

#' Access MALLET's topic model diagnostics
#'
#' Get MALLET's suite of model diagnostics (as a string of XML)
#'
#'
#' @return a reference to the MALLET diagnostics object.
#'
#' @seealso
#' \code{\link{parse_diagnostics}},
#' \code{\link{read_diagnostics}},
#' \code{\link{write_diagnostics}}
#'
#' @export
#'
}

#' Save MALLET's topic model diagnostics as XML
#'
#' Write MALLET's model diagnostics to an XML file.
#'
#' @param trainer the \code{RTopicModel} object.
#' @param output_file the name of a file to save XML to.
#' @param num_top_words the number of top words per topic to calculate topic-word 
#' diagnostics for.
#'
#' @seealso
#' \code{\link{read_diagnostics}}
#'
#' @export
#'
write_diagnostics <- function(trainer,output_file="diagnostics.xml",
                              num_top_words=50L)) {

    d <- .jnew("cc/mallet/topics/TopicModelDiagnostics",
          trainer$model,as.integer(num_top_words))
    xml <- d$toXML()
    cat(xml,file=output_file)
}

#' Read MALLET model-diagnostic results.
#'
#' Uses the \pkg{XML} package and \code{libxml} to parse the MALLET
#' diagnostic output.
#'
#' @param xml_file file holding XML to be parsed.
#'
#' @return a list of two dataframes, \code{topics} and \code{words}.
#' The diagnostics are sparsely documented by the MALLET source
#' code (\url{http://hg-iesl.cs.umass.edu/hg/mallet}: see
#' \code{src/cc/mallet/topics/TopicModelDiagnostics.java}). In
#' \code{topics}, columns include \code{topic}, the 1-indexed topic
#' number; \code{corpus_dist}, the Jensen-Shannon divergence from the
#' corpus; and \code{coherence}, the topic coherence measure defined by
#' Mimno et al., eq. (1) (the sum of log-co-document-document frequency
#' ratios for the top words in the topic [number of top words as set by
#' \code{num_top_words} parameter to \code{\link{write_diagnostics}]).
#'
#' \code{words} gives word-level diagnostics about each of the most probable words in 
#' each topic.
#'
#' @references
#' David Mimno et al. Optimizing Semantic Coherence in Topic Models. \emph{EMNLP} 2011. 
#' \url{http://www.cs.princeton.edu/~mimno/papers/mimno-semantic-emnlp.pdf}
#'
#' @seealso
#' \code{\link{write_diagnostics}}
#'
#' @export
#'
read_diagnostics <- function(xml_file) {
    library(XML)
    d <- xmlParse(file=xml_file)
    # xmlSApply returns a string matrix with topics in *columns*
    topic_attrs <- t(xmlSApply(getNodeSet(d,"/model/topic"),xmlAttrs))
    # de-stringify: 
    topics <- apply(topic_attrs,2,as.numeric)
    # add in a 1-indexed "topic" number
    topics <- data.frame(topic=topics[,"id"] + 1,topics)

    word_info <- function(node) {
        w <- xmlValue(node)
        topic <- as.numeric(xmlGetAttr(xmlParent(node),"id")) + 1
        attrs <- xmlAttrs(node)
        c(topic=topic,word=w,attrs)
    }

    # result of this is a string matrix
    wm <- t(xmlSApply(getNodeSet(d,"/model/topic/word"),word_info))
    w_topics <- as.numeric(wm[,1]) # column "topic," re-de-stringified
    w_words <- wm[,2] # column "word"

    # de-stringify
    w_rest <- apply(wm[,3:ncol(wm)],2,as.numeric)

    words <- data.frame(topic=w_topics,
                        word=w_words,
                        w_rest,
                        stringsAsFactors=F)


    list(topics=topics,words=words)
}

    

#' Get miscellaneous model parameters
#'
#' Collect together some overall parameters of a model into a one-row
#' data frame.
#'
#' The topic model is also estimates a \eqn{\beta} parameter.
#' We can also remember the final log-likelihood of the model and the
#' total number of tokens in the instances mallet operated on.
#'
#' @param trainer the \code{RTopicModel} object
#'
#' @return a data frame with one row and three columns, \code{beta,n_tokens,LL}.
#'
#' @export
#'
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
# twm: the topic-word matrix (can get it from a trainer object with topic.words)
#
# b: the estimated beta value
#
# actually, nothing stops you setting twm = topic-document matrix and b = 
# vector of alphas. That gives the distances among topics as distributions over 
# documents.
topic_divergences <- function(twm,b) {
    # smoothing
    twm <- twm + b
    # normalization
    twm <- diag(1 / rowSums(twm)) %*% twm
    row_dists(twm,method="JS")
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

# tf_idf
#
# direct calculation of tf_idf scores from my plain tdm sparseMatrix
#
# term and doc (both numeric indices) can be vectors.
#
# May not be optimal for speed for calculating scores for the whole tdm.

tf_idf <- function(term,doc,tdm) {
    idf <- log(ncol(tdm) / rowSums(tdm[term,,drop=F] != 0))
    Diagonal(n=length(term),x=idf) %*% tdm[term,doc]
}

# TODO topic key words / all topic words weighted according to
# Blei and Lafferty, "Topic Models":
#
# score(k,v) = beta(k,v) log( beta(k,v) / ( Prod_j beta(j,v) )^(1/K) )
#
# where beta(k,v) is the estimated probability of term v in topic k, and K is 
# the number of topics
