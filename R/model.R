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

#' Convert raw topic-word counts to weighted scores
#'
#' Given an unweighted final sampling state of the assignment of words to topics, produce 
#' a topic-word matrix with weighted scores instead.
#'
#' The only supported method is that given by Blei and Lafferty: the score for word {v} in topic \eqn{t} is
#' \deqn{p(t,v)\textrm{log}(p(t,v) / \prod_k p(k,v)^1/K)}
#' where \eqn{K} is the number of topics. The score gives more weight to words which are 
#' ranked highly in fewer topics.
#'
#' To generate "key words" from the new scoring, apply \code{\link{tw_wkf}} to the results 
#' of this function.
#'
#' @param tw the topic-word matrix (assumed to be raw counts of word-topic assignments)
#' @param b \eqn{beta}, the smoothing over words (zero counts are replaced with this value)
#' @param method The scoring method. Only the default value, \code{blei_lafferty}, is 
#' currently supported.
#' @return a matrix of weighted scores with topics in rows and words in columns.
#'
#' @references
#' D. Blei and J. Lafferty. Topic Models. In A. Srivastava and M. Sahami, editors, \emph{Text Mining: Classification, Clustering, and Applications}. Chapman & Hall/CRC Data Mining and Knowledge Discovery Series, 2009. \url{http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf}.
#' 
#' @seealso \code{\link{tw_wkf}},
#' \code{\link{weighted_keys_frame}}
#'
#' @export
#'
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


#' Write the topic-word matrix to disk
#'
#' Saves the estimated weights of words in topics to a headerless CSV file and the list of 
#' words in the "vocabulary" of the model to a text file.
#'
#' This is the same information, differently organized, output in a
#' single file by mallet's \code{--topic-word-weights-file <outfile.tsv>}
#' command-line option. To create that from R, use:
#' \code{
#' trainer$model$printTopicWordWeights(new(J("java.io.File"),
#'                                     "outfile.tsv"))
#' }
#'
#' @param trainer the reference to the \code{RTopicModel} object.
#'
#' @param topic_words_file name of a file to write the topic-word matrix to as a
#' CSV file (topics are rows). No row or column headers.
#'
#' @param vocab_file name of a file to write the vocabulary corresponding to
#' columns of topic_wordfile. One word per line.
#'
#' @param smoothed passed on to \code{\link{mallet:mallet.topic.words}}.
#' @param normalized passed on to \code{\link{mallet:mallet.topic.words}}. For raw counts, 
#' set \code{smoothed=F,normalized=F}.
#'
#' @seealso
#' \code{\link{read_topic_words}},
#' \code{\link{mallet:mallet.topic.words}},
#' \code{\link{topic_word_scores}},
#' \code{\link{weighted_keys_frame}} for just the "top" or key words in each topic.
#'
#' @export
#'
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


