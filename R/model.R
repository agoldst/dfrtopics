# Core dfrtopics functions: running MALLET and extracting document-topic
# and topic-word information.

#' Make a topic model of DfR documents
#'
#' The basic usage of this package is wrapped up in this convenience
#' function.
#'
#' Given wordcount and metadata files, this function sets up MALLET
#' inputs and then runs MALLET to produce a topic model. Normally you will want
#' finer-grained control over the mallet inputs and 
#' modeling parameters. The steps for that process are described in the package vignette. Once the model has been trained, the results can be saved to disk with \code{\link{write_dfr_lda}}
#'
#' If java gives out-of-memory errors, try increasing the Java heap size to a 
#' large value, like 4GB, by setting \code{options(java.parameters="-Xmx4g")} 
#' \emph{before} loading this package (or rJava).
#'
#'
#' @param citations_files character vector with names of DfR \code{citations.CSV} or \code{citations.tsv} metadata files
#' files 
#' @param wordcounts_dirs character vector with names of directories holding \code{wordcounts*.CSV} 
#' files
#' @param stoplist_file name of stoplist file (containing one stopword per line)
#' @param n_topics number of topics to model
#' @param ... passed on to \code{\link{train_model}}
#'
#' @return a \code{\link{dfr_lda}} object holding the results
#'
#' @seealso
#' \code{\link{read_dfr_metadata}},
#' \code{\link{read_dfr_wordcounts}},
#' \code{\link{make_instances}},
#' \code{\link{train_model}},
#' \code{\link{output_model}}
#' @examples
#' # Make a 50-topic model of documents in the wordcounts folder
#' \dontrun{model_dfr_documents("citations.CSV", "wordcounts", 50)}
#'
#' @export
#'
model_dfr_documents <- function(
        citations_files,
        wordcounts_dirs,
        n_topics,
        stoplist_file=file.path(path.package("dfrtopics"),
                                "stoplist", "stoplist.txt"),
        ...)  {
    result <- read_dfr(list.files(wordcounts_dirs), full.names=T) %>%
        dfr_docs_frame() %>%
        make_instances(stoplist_file) %>%
        train_model(n_topics, ...)
    metadata(result) <- read_dfr_metadata(citations_files)
    result
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
#' \item{\code{id_map.txt}}{instance id's, one per line}
#' \item{\code{instances.mallet}}{save the source text "instances" file (not 
#' done by default)}
#' \item{\code{topic_scaled.csv}}{CSV with scaled 2D
#' coordinates for the topics. Obtained by applying
#' \code{\link[stats]{cmdscale}} to a matrix of topic divergences
#' calculated by \code{\link{topic_divergences}}} } 
#'
#' @param model_result the result from \code{\link{model_documents}},
#' or, equivalently, a list with elements called \code{trainer},
#' \code{doc_topics}, \code{wkf}, and \code{seed}. If \code{doc_topics}
#' or \code{wkf} are omitted, they will be calculated (unsmoothed and
#' unnormalized).
#' 
#' @param output_dir where to save all the output files.
#'
#' @param save_instances if TRUE, extract the instance list from the trainer object and save it to \code{instances.mallet}; if FALSE (the default), don't
#'
#' @param save_scaled if TRUE (the default), write a file of 2D coordinate for the topics
#'
#' @export
#'
write_dfr_lda <- function(m, output_dir=".",
                          n_top_words=50,
                          save_instances=F,
                          save_scaled=T) {
    if(!file.exists(output_dir)) {
        message("Creating output directory ",output_dir)
        dir.create(output_dir)
    }

    tw_f <- file.path(output_dir, "topic_words.csv")
    write_matrix_csv(topic_words(m), tw_f)
    message("Wrote ",tw_f)

    vocab_f <- file.path(output_dir, "vocab.txt")
    writeLines(vocabulary(m), vocab_f)
    message("Wrote ",vocab_f)

    params <- modeling_parameters(m)
    hyper <- hyperparameters(m)

    params_f <- file.path(output_dir, "params.txt")
    dput(list(params, hyper), params_f)
    message("Wrote ",params_f)

    keys_f <- file.path(output_dir, "top_words.csv")
    write.table(top_words(m, n_top_words), 
                quote=F, sep=",", row.names=F, col.names=T)
    message("Wrote ", keys_f)

    dt_f <- file.path(output_dir,"doc_topics.csv")
    write.table(doc_topics(m), dt_f,
                quote=F, sep=",", row.names=F, col.names=T)
    message("Wrote ", dt_f)

    state_f <- file.path(output_dir, "mallet_state.gz")
    write_sampling_state(m, state_f)
    message("Wrote ", state_f)

    diag_f <- file.path(output_dir, "diagnostics.xml") 
    write_diagnostics(m, diag_f, n_top_words=n_top_words)
    message("Wrote ", diag_f)

    id_map_f <- file.path(output_dir, "doc_ids.txt")
    writeLines(doc_ids(m), id_map_f)
    message("Wrote ", id_map_f)

    if (save_instances) {
        inst_f <- file.path(output_dir, "instances.mallet")
        write_instances(instances(m), inst_f)
        message("Wrote ", inst_f)
    }

    if (save_scaled) {
        scaled_f <- file.path(output_dir,"topic_scaled.csv")
        write.table(scaled_topic_coordinates(m), scaled_f,
                    quote=F, sep=",", row.names=F, col.names=F)
        message("Wrote ", scaled_f)
    }
}

#' Train a topic model
#'
#' Invokes MALLET's parallel topic modeling algorithm on a set of documents represented as an InstanceList.
#'
#' Create the instance list object with \code{\link{make_instances}}. This function prints MALLET's progress reporting to the console.
#'
#' If Java gives out-of-memory errors, try increasing the Java heap size to a 
#' large value, like 4GB, by setting \code{options(java.parameters="-Xmx4g")} 
#' \emph{before} loading this package (or rJava).
#'
#' @param instances either an rJava reference to an \code{InstanceList} object or the 
#' name of a file into which such an object has been serialized
#' @param n_topics how many topics to train?
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
#' @param seed MALLET's random number seed: set this to ensure a reproducible run of the Gibbs sampling algorithm.
#'
#' @return a \code{dfr_lda} object#'
#' @export
#'
#' @seealso \code{\link{make_instances}},
#' \code{\link{make_instances}},
#' \code{\link{model_documents}},
#' \code{\link{output_model}}
#' 
train_model <- function(instances, n_topics,
                        alpha_sum=5, beta=0.01,
                        n_iters=200,
                        n_max_iters=10,
                        optimize_hyperparameters=T,
                        n_hyper_iters=20,
                        n_burn_in=50,
                        symmetric_alpha=F,
                        threads=4L,
                        seed=NULL) {

    # Java number types are a problem with R's loose typing. RTopicModel
    # expects doubles everywhere; ParallelTopicModel expects ints
    # for integers. So we have to do some coercions to avoid rJava
    # complaints.

    trainer <- MalletLDA(as.numeric(n_topics),
                         as.numeric(alpha_sum),
                         as.numeric(beta))
    trainer$model$setNumThreads(as.integer(threads))
    if(!is.null(seed)) {
        trainer$model$setRandomSeed(as.integer(seed))
        message("MALLET random number seed set to ", seed)
    }

    trainer$loadDocuments(instances)

    if(optimize_hyperparameters) {
        trainer$model$setSymmetricAlpha(symmetric_alpha)
        trainer$setAlphaOptimization(as.numeric(n_hyper_iters),
                                     as.numeric(n_burn_in))
    }
    else {
        trainer$setAlphaOptimization(0, 0)
    }

    trainer$train(as.numeric(n_iters))
    trainer$maximize(as.numeric(n_max_iters))

    structure(
        list(model=trainer,
             params=list(
                n_iters=n_iters,
                n_max_iters=n_max_iters,
                optimize_hyperparameters=optimize_hyperparameters,
                n_hyper_iters=n_hyper_iters,
                n_burn_in=n_burn_in,
                symmetric_alpha=symmetric_alpha,
                threads=threads,
                seed=seed,
                initial_alpha_sum=alpha_sum,
                initial_beta=beta)
        ),
        class="dfr_lda"
    )
}

n_topics <- function (x) UseMethod("n_topics")
n_topics.dfr_lda <- function (x) {
    if (!is.null(x$model)) {
        x$model$model$numTopics
    } else if (!is.null(x$doc_topics)) {
        ncol(doc_topics(x))
    } else {
        NULL # return null if we haven't loaded enough information yet
    }
}
modeling_parameters  <- function (x) UseMethod("modeling_parameters")
modeling_parameters.dfr_lda  <- function (x) x$params
 
print.dfr_lda <- function (x) {
    s <- str_c(
"A topic model created by MALLET/dfrtopics

Number of topics: ", n_topics(x), "
Number of documents: ", n_docs(x))

    cat(s)
    invisible(x)
}

summary.dfr_lda <- function (x) {
    print(x)
}

#' Access MALLET's glue model object
#'
#' This function returns a reference to the top-level Java object representing an LDA model.
#'
#' For its R interface, MALLET uses a class RTopicModel. This has some convenience methods for accessing and manipulating a topic model from R using rJava. Most of the modeling functionality is carried out by a ParallelTopicModel instance, which is a data member of this class.
#'
#' Java's strict typing means you'll have some funtimes with
#' \code{\link[rJava]{.jcall}}. To index into array-like objects from Java, apply 
#' \code{\link[base]{as.integer}} to parameters.
#'
#' @param x a \code{dfr_lda} object
#' @return a reference to the RTopicModel object
#' @seealso \code{\link{ParallelTopicModel}}
#' @export
#' 
RTopicModel <- function (x) UseMethod("RTopicModel")
RTopicModel.dfr_lda <- function (x) x$model

#' Access MALLET's model object
#'
#' This function returns a reference to the main Java object representing an LDA model in MALLET.
#'
#' For its R interface, MALLET uses a class RTopicModel, but most of the modeling functionality is carried out by a ParallelTopicModel instance. This object is reachable from R. Once you have the reference to it here, you can use rJava to access all its public methods and members.
#'
#' Java's strict typing means you'll have some funtimes with
#' \code{\link[rJava]{.jcall}}. To index into array-like objects from Java, apply 
#' \code{\link[base]{as.integer}} to parameters.
#'
#' @param x a \code{dfr_lda} object
#' @return a reference to the ParallelTopicModel object
#' @seealso \code{\link{RTopicModel}}
#' @export
#' 
ParallelTopicModel <- function (x) UseMethod("ParallelTopicModel")
ParallelTopicModel.dfr_lda <- function (x) x$model$model

#' Access the InstanceList stored by a model
#'
#' MALLET models store a reference to their source documents. This is an accessor function for that reference.
#'
#' @param x a \code{dfr_lda} object
#' @return a reference to the InstanceList object, or NULL if not available
#'
#' @export
#'
instances <- function (x) UseMethod("instances")
instances.dfr_lda <- function (x) if {
    obj <- RTopicModel(x)
    if (is.null(obj)) {
        warning("The model object is not available")
        NULL
    } else {
        obj$instances
    }
}


#' The document-topic matrix
#'
#' Extracts the matrix from a \code{dfr_lda} model with documents in rows and topic weights in columns. The document order is the same as the ordering of the documents supplied to MALLET.
#'
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and unsmoothed weights as the "raw" form of the model; this is not strictly correct, it is easier to reason about and to do post-hoc calculations with. It is up to t user to apply normalization and smoothing where appropriate.
#'
#' @param x a \code{dfr_lda} object
#' @return a numeric matrix
#'
#' @export
#'
doc_topics <- function (x) UseMethod("doc_topics")
`doc_topics<-` <- function (x, value) UseMethod("doc_topics<-")
doc_topics.dfr_lda <- function (x) {
    if (!is.null(x$doc_topics)) {
        x$doc_topics
    } else if (!is.null(x$model)) {
        x$doc_topics <<- mallet.doc.topics(x$model, smoothed=F,
                                           normalized=F)
        x$doc_topics
    } else {
        stop("Neither the model object nor a pre-calculated matrix are available.
             To load the latter, use doc_topics(x) <- read.csv(...)")
    }
}
`doc_topics<-.dfr_lda` <- function (x, value) {
    x$doc_topics <- value
    x
}

#' Retrieve document IDs
#'
#' Extracts a character vector of IDs of the modeled documents from a \code{dfr_lda} model.
#'
#' @param x a \code{dfr_lda} object
#' @return a character vector
#' 
#' @export
#'
doc_ids <- function (x) UseMethod("doc_ids")
`doc_ids<-` <- function (x, value) UseMethod("doc_ids<-")
doc_ids.dfr_lda <- function (x) {
    if (!is.null(x$ids)) {
        x$ids
    } else if (!is.null(x$model)) {
        x$model$getDocumentNames()
    } else {
        stop("Neither the model object nor a pre-loaded list of IDs is available.
             To load the latter, use doc_ids(x) <- readLines(...)")
    }
}
`doc_ids<-.dfr_lda` <- function (x, value) {
    x$doc_ids <- value
    x
}

#' Retrieve model corpus vocabulary
#'
#' Extracts a character vector of the word types included in a \code{dfr_lda} model.
#'
#' @param x a \code{dfr_lda} object
#' @return a character vector
#' 
#' @export
#'
vocabulary <- function (x) UseMethod("vocabulary")
`vocabulary<-` <- function (x, value) UseMethod("vocabulary<-")
vocabulary.dfr_lda <- function (x) {
    if (!is.null(x$vocab)) {
        x$vocab
    } else if (!is.null(x$model)) {
        x$vocab <<- x$model$getVocabulary()
        x$vocab
    } else {
        stop("Neither the model object nor a pre-loaded vocbulary is available.
             To load the latter, use vocabulary(x) <- readLines(...)")
    }
}
`vocabulary<-.dfr_lda` <- function (x, value) {
    x$vocab <- value
    x
}

#' The topic-words matrix
#'
#' Extracts the matrix from a \code{dfr_lda} model with topics in rows and word weights in columns. The word order is the same as that of the vocabulary for the MALLET InstanceList.
#'
#' The expectation throughout \code{dfrtopics} is that we keep unnormalized and unsmoothed weights as the "raw" form of the model; this is not strictly correct, it is easier to reason about and to do post-hoc calculations with. It is up to the user to apply normalization and smoothing where appropriate.
#'
#' @param x a \code{dfr_lda} object
#' @return a numeric matrix
#'
#' @export
#'
topic_words <- function (x, ...) UseMethod("topic_words")
`topic_words<-` <- function (x, value) UseMethod("topic_words<-")
topic_words.dfr_lda <- function (x, smoothed=F, normalized=F) {
    if (!is.null(x$topic_words)) {
        x$topic_words
    } else if (!is.null(x$model)) {
        x$topic_words <<- mallet.topic.words(x$model, smoothed=smoothed,
                                             normalized=normalized)
        x$topic_words
    } else {
        stop("Neither the model object nor a pre-calculated matrix are available.
             To load the latter, use topic_words(x) <- read.csv(...)")
    }
}
`topic_words<-.dfr_lda` <- function (x, value) {
    x$topic_words <- value
    x
}

#' Topic key words
#'
#' The most common way to summarize topics is to list their top-weighted words, 
#' together with their topic weights. Though every topic assigns some probability to every term in the whole vocabulary, we often disregard all but its most frequent terms.
#'
#' @param x a \code{dfr_lda} object
#' @return a data frame with three columns, \code{topic} (indexed from 1), \code{word} (character), and \code{weight}
#' 
top_words <- function (x, ...) UseMethod("top_words")
`top_words<-` <- function (x, value) UseMethod("top_words<-")
top_words.dfr_lda <- function (x, n=50) {
    if (!is.null(x$top_words) && nrow(x$top_words) / n_topics(x) >= n) {
        result <- x$top_words %>% group_by(topic) %>%
            slice(1:n) %>%
            ungroup()
    } else {
        js <- list()
        tw <- topic_words(x)
        for (i in 1:nrow(tw)) {
            js[[i]] <- order(tw[i, ], decreasing=T)[1:n]
        }
        js <- do.call(c, js)
        topics <- rep(1:nrow(tw), each=n)
        x$top_words <<- data_frame(
            topic=topics, 
            word=vocabulary(x)[js],
            weight=tw[matrix(c(topics, js), ncol=2)]
        )

        result <- x$top_words
    } else {
        stop(
"The model object is not available, and the pre-loaded list of top words is 
either missing or too short.  To load the latter, use
    top_words(x) <- read.csv(...)"
        )
    }

    result
}
`top_words<-.dfr_lda` <- function (x, value) {
    x$top_words <- value
    x
}


#' Convert raw topic-word counts to weighted scores
#'
#' Given an unweighted final sampling state of the assignment of words to topics, produce 
#' a topic-word matrix with weighted scores instead.
#'
#' The default method is that given by Blei and Lafferty: the score for word \eqn{v} in topic \eqn{t} is
#' \deqn{p(t,v)\textrm{log}(p(t,v) / \prod_k p(k,v)^1/K)}
#' where \eqn{K} is the number of topics. The score gives more weight to words which are 
#' ranked highly in fewer topics.
#'
#' Another method is the "relevance" score of Sievert and Shirley: in this case
#' the score is given by
#' \deqn{\lambda log (p(t,v) + (1 - \lambda) log (p(t,v) / p(v)}
#' where \eqn{\lambda} is a weighting parameter which is by default set to 0.6 and 
#' which determines the amount by which words common in the whole corpus are 
#' penalized.
#'
#' To generate "key words" from the new scoring, apply \code{\link{tw_wkf}} to the results 
#' of this function.
#'
#' @param tw the topic-word matrix (assumed to be raw counts of word-topic assignments)
#' @param b \eqn{beta}, the smoothing over words (zero counts are replaced with this value)
#' @param method The scoring method. Two methods, \code{blei_lafferty} and \code{sievert_shirley}, are 
#' currently supported.
#' @param l For \code{sievert_shirley}, the 
#' weighting parameter \eqn{\lambda}, by default 0.6.
#' @return a matrix of weighted scores with topics in rows and words in columns.
#'
#' @references
#' D. Blei and J. Lafferty. Topic Models. In A. Srivastava and M. Sahami, editors, \emph{Text Mining: Classification, Clustering, and Applications}. Chapman & Hall/CRC Data Mining and Knowledge Discovery Series, 2009. \url{http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf}.
#' 
#' C. Sievert and K.E. Shirley. LDAvis: A method for visualizing and interpreting topics. \url{http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf}.
#
#' @seealso \code{\link{tw_wkf}},
#' \code{\link{weighted_keys_frame}}
#'
#' @export
#'
topic_word_scores <- function(x, method="blei_lafferty", l=0.6) {
    tw <- topic_words(x)

    V <- ncol(tw)

    # smooth + normalize weights
    topic_totals <- rowSums(tw) + V * b
    tw <- tw + b

    pw <- Matrix::colSums(tw) / sum(tw)

    tw <- Diagonal(x=1 / topic_totals) %*% tw


    switch(method,
        blei_lafferty=tw_blei_lafferty(tw),
        sievert_shirley,tw_sievert_shirley(tw,pw,l),
        warning("Unknown scoring method."))

}

tw_blei_lafferty <- function (tw) {
    # score(t,v) = p(t,v) log (p(t,v) / Prod_k p(k,v) ^ 1 / K)
    #            = p(t,v) ( log p(t,v) - (1 / K) log( Prod_k p(k,v) ) )
    #            = p(t,v) ( log p(t,v) - (1 / K) Sum_k (log p(k,v) ) )

    n <- nrow(tw)
    log_tw <- log(tw)

    # calculate down-weighting factor for each word.
    # for some unknown reason I seem to need to explicitly dispatch to
    # the Matrix method here
    word_factor <- tw %*% Diagonal(x=Matrix::colSums(log_tw) / n)

    tw * (log_tw) - word_factor
}

tw_sievert_shirley <- function(tw,pw,l = 0.6) {
    # score(t,v) = lambda log p(t,v) + (1 - lambda) log (p(t,v) / p(v))

    log_tw <- log(tw)

    # TODO not sure this works right
    l * log_tw + (1 - l) * t(apply(log_tw,1,'/',pw)) 
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
#' @param smoothed passed on to \code{\link[mallet]{mallet.topic.words}}.
#' @param normalized passed on to \code{\link[mallet]{mallet.topic.words}}. For raw counts, 
#' set \code{smoothed=F,normalized=F}.
#'
#' @seealso
#' \code{\link{read_topic_words}},
#' \code{\link[mallet]{mallet.topic.words}},
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
    message("Saved topic-word matrix to ",topic_words_file)
    vocab <- trainer$getVocabulary()
    writeLines(vocab,vocab_file)
    message("Saved vocabulary to ",vocab_file)
}

#' Read in a numeric matrix
#'
#' Since R does not supply a matrix-reading function, here's one.
#'
#' @return For \code{read_matrix_csv}, an ordinary matrix; for \code{read_Matrix_csv}, a \code{\link[Matrix]{sparseMatrix}}
#'
#' @param f CSV filename, for example \code{topic_words.csv}.
#'
#' @param what datatype to read in (passed on to \code{\link[base]{scan}}). 
#' \code{\link[base]{integer}()} by default; use \code{\link[base]{numeric}()} if 
#' the datafile has proportions.
#'
#' @export
#'
read_matrix_csv <- function (f, what=integer()) {
    m <- scan(f, what=what, sep=",")
    n <- length(scan(f, what=what, sep=",", nlines=1, quiet=T))
    matrix(m, byrow=T, ncol=n)
}
read_Matrix_csv <- function (f, what=integer()) {
    as(read_Matrix_csv(f, what), "sparseMatrix")
}

#' Write out a numeric matrix to a text file
#'
#' Convenience function for saving numeric matrices as text files (not a particularly space-efficient format).
#'
#' @param m matrix or Matrix (e.g. topic-words or document-topics)
#' @param f file connection to write to
#'
#' @export
write_matrix_csv <- function (m, f) {
    write.table(as.matrix(m), f, sep=",",
                row.names=F, col.names=F) 
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

#' Retrieve metadata
#'
#' Get or set the metadata for an \code{dfr_lda} model.
#'
#' The setter method drops any metadata rows that do not match the document IDs
#' for the model.
#'
#' @param x a \code{dfr_lda} object
#' @param value a metadata data frame
#' @return a character vector
#' 
#' @export
#'
doc_metadata <- function (x) UseMethod("doc_metadata")
`doc_metadata<-` <- function (x, value) UseMethod("doc_metadata<-")
doc_metadata.dfr_lda <- function (x) x$metadata
`doc_metadata<-.dfr_lda` <- function (x, value) {
    ids <- doc_ids(x)
    i <- match(ids, value$id)
    if (any(is.na(i))) {
        stop("Supplied data frame does not have rows matching all document IDs.")
    }
    x$metadata <- value[i, ]
    x
}

#' Retrieve estimated model hyperparameters
#'
#' Get or set the estimates for the model hyperparameters \eqn{\alpha} and \eqn{\beta}. These are used in smoothing the document-topic and topic-word matrices respectively.
#'
#' @param x a \code{dfr_lda} object
#' @return a list with two elements, \code{alpha} (a vector, with one value per topic), and \code{beta} (a single number)
#'
#' @export
#'
hyperparameters <- function (x) UseMethod("hyperparameters")
`hyperparameters<-` <- function (x, value) UseMethod("hyperparameters<-")
hyperparameters.dfr_lda <- function (x) {
    if (!is.null(x$hyper)) {
        x$hyper
    } else if (!is.null(x$model)) {
        list(alpha=x$model$getAlpha(),
             beta=x$model$model$beta)
    } else {
        stop(
"Neither the model object nor pre-loaded hyperparameters are available."
        )
    }
}
`hyperparameters<-.dfr_lda` <- function (x, value) {
    x$hyper <- value
    x
}

#' The model object
#'
#' A topic model is a complicated beastie, and exploring it requires keeping track of a number of different kinds of data. The \code{dfr_lda} object strives to encapsulate some of this for you. Normally you will not have to use this explicit constructor.
#' 
#' Any of the parameters can be omitted.
#'
#' @param doc_topics document-topic matrix
#' @param doc_ids vector of document ids corresponding to rows of \code{doc_topics}
#' @param vocab vector of word types corresponding to columns of \code{topic_words}
#' @param top_words data frame with top-ranked words and their weights for each topic
#' @param topic_words topic-word matrix (expected to be sparse)
#' @param params list of modeling parameters
#' @param hyper list of estimated hyperparameters \eqn{\alpha} and \eqn{\beta}
#' @param metadata data frame of metadata
#' @param model reference to an RTopicModel object from MALLET
#' @param state final Gibbs sampling state
#' 
#' @export
dfr_lda <- function (doc_topics=NULL,
                     doc_ids=NULL,
                     vocab=NULL,
                     top_words=NULL,
                     topic_words=NULL,
                     params=NULL,
                     hyper=NULL,
                     metadata=NULL,
                     model=NULL,
                     state=NULL) {
    structure(list(doc_topics=doc_topics,
                   doc_ids=doc_ids,
                   vocab=vocab,
                   top_words=top_words,
                   topic_words=topic_words,
                   hyper=hyper,
                   metadata=metadata,
                   model=model,
                   params=params,
                   state=state),
              class="dfr_lda")
}

#' Read in model outputs from files
#'
#' Load a model object from a set of files (like those produced from \code{\link}{write_dfr_lda}). Leave a filename out to skip loading that piece of the puzzle.
#'
#' @param doc_topics_file document-topic matrix file (CSV)
#' @param doc_ids_file document id file (text, one ID per line)
#' @param vocab_file model vocabulary file (text, one word type per line)
#' @param top_words_file topic top word data frame file (CSV)
#' @param topic_words_file topic-word matrix file (CSV)
#' @param metadata_file metadata file (CSV or TSV)
#' @param params_file modeling parameters file (read with \code{\link[base]{dget}})
#'
#' @return a \code{dfr_lda} object
#'
#' @export
load_dfr_lda <- function(
        doc_topics_file,
        doc_ids_file,
        vocab_file,
        top_words_file=NULL,
        topic_words_file=NULL,
        metadata_file=NULL,
        params_file=NULL) {

    if (!is.null(top_words_file)) {
        top_words <- read.csv(top_words_file, as.is=T)
    } else {
        top_words <- NULL
    }

    if (!is.null(topic_words_file)) {
        library("Matrix")
        topic_words <- as(read_matrix_csv(topic_words_file), "sparseMatrix")
    } else {
        topic_words <- NULL
    }

    if (!is.null(metadata_file)) {
        metadata <- read_dfr_metadata(metadata_file)
    } else {
        metadata <- NULL
    }

    if  (!is.null(params_file)) {
        p <- dget(params_file)
        params <- p$params
        hyper <- p$hyper
    } else {
        params <- NULL
        hyper <- NULL
    }
        

    result <- dfr_lda(
        doc_topics=read_matrix_csv(doc_topics_file),
        doc_ids=readLines(doc_ids_file),
        vocab=readLines(vocab_file),
        top_words=top_words,
        topic_words=topic_words,
        params=params,
        hyper=hyper)

    doc_metadata(result) <- metadata

    result
}

#' Load a model with conventional filenames from a directory
#'
#' If you accept the defaults from \code{\link{write_dfr_lda}}, you can read t results back in simply by passing the directory name here.
#'
#' The expected filenames are \code{doc_topics.csv}, \code{doc_ids.txt}, \code{vocabulary.txt}, \code{top_words.csv}, \code{topic_words.csv}, and \code{params.txt}.
#'
#' @param f directory name
#' @param load_topic_words logical: load the full topic-word matrix?
#' @param metadata_file document metadata file(s) (optional)
#'
#' @return \code{dfr_lda} object
#'
#' @export
load_dfr_lda_directory <- function (f, load_topic_words=F,
                                    metadata_file=NULL) {
    tw <- if (load_topic_words) file.path(f, "topic_words.csv") else NULL
    load_dfr_lda(doc_topics_file=file.path(f, "doc_topics.csv"),
                 doc_ids_file=file.path(f, "doc_ids.txt"),
                 vocab_file=file.path(f, "vocabulary.txt"),
                 top_words_file=file.path(f, "top_words.csv"),
                 topic_words_file=tw,
                 metadata_file=metadata_file,
                 params_file=file.path(f, "params.txt"))
}
                 
#' Load a model with files from dfrtopics 0.1 (unimplemented)
#'
#' The convention for exporting model outputs differed in earlier versions of this package. This file loads in a folder of model outputs on the old conventions.
#'
#' @param f directory name
#' @param load_topic_words
#'
#' @export
load_dfr_lda_legacy <- function (f, load_topic_words=F,
                                 metadata_file=NULL) {
    stop("Unimplemented.")
    # TODO implement this
}




scaled_topic_coordinates <- function (m) {
    cmdscale(
        topic_divergences(
            topic_words(m),
            hyperparameters(m)$beta
        ),
        k=2
    )
}
