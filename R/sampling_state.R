# Functions for manipulating the full Gibbs sampling state.
#
# a.k.a. keeping the words in topic models (as Ben Schmidt says)

#' Save the Gibbs sampling state to a file
#'
#' Saves the MALLET sampling state using MALLET's own state-output routine, which produces 
#' a ginormous gzipped textfile
#'
#' @param trainer the \code{RTopicModel} object
#' @param outfile the output file name
#'
#' @seealso \code{\link{read_simplified_state}}
#'
#' @export
#'
write_mallet_state <- function(trainer,outfile="state.gz") {
    fileobj <- new(J("java.io.File"),outfile)
    trainer$model$printState(fileobj)
}

#' Reduce a MALLET sampling state on disk to a simplified form
#'
#' This function reads in the sampling state output by MALLET and writes a CSV file 
#' giving the assignments of word types to topics in each document.
#' 
#' The resulting file has a header \code{document,word,topic,count} describing its 
#' columns. Use \code{\link{read_simplified_state}} to access the result in R. The MALLET 
#' state is typically too big to handle in memory using R, so the "simplification" is done 
#' using a very simple python script.
#'
#' @param state_file the MALLET state (assumed to be gzipped)
#' @param outfile the name of the output file (will be clobbered)
#' @return the return value from the python script
#'
#' @seealso \code{\link{read_simplified_state}}
#'
#' @export
#'
simplify_state <- function(state_file,outfile) {
    if(file.exists(outfile)) {
        stop("Output file ",outfile," already exists.")
    }
    cmd <- paste("python",
                 file.path(path.package("dfrtopics"),"python",
                           "simplify_state.py"),
                 state_file,">",outfile)
    message("Executing ",cmd)
    system(cmd)
}

#' Read in a Gibbs sampling state
#'
#' This function reads in a Gibbs sampling state represented by
#' \code{document,word,topic,count} rows. The resulting dataframe or
#' \code{big.matrix} gives access to the assignments of individual words
#' to topics within documents. Because of the nature of the JSTOR data,
#' the result aggregates all instances of a given word type in a given document;
#' MALLET itself remembers token order, but this is meaningless when
#' working with JSTOR \code{wordcounts.CSV} files.
#'
#' This uses the \pkg{bigmemory} package (q.v.) to read the 
#' simplified state instead of attempting to hold the result in memory in a data frame.
#'
#' MALLET writes out its final sampling state in a highly redundant
#' form which R is not very happy to read in as is. In order to work with the sampling 
#' state in R, one first needs to toss out redundant columns. This package supplies a 
#' python script (\code{inst/python/simplify_state.py}) for this; if you set
#' \code{generate_file=T} and \code{state_file}, this function will call that script for 
#' you.
#'
#' To recover the meaning of the integer codes for words and documents, use the functions 
#' for reading MALLET \code{InstanceList}s: \code{\link{instances_vocabulary}} and 
#' \code{\link{instances_ids}}. \emph{N.B.} the MALLET sampling state, and the "simplified 
#' state" output by this function to disk, index documents, words, and topics from zero, 
#' but the dataframe returned by this function indexes these from one for convenience 
#' within R.
#'
#' @return a \code{big.matrix} with four columns, 
#' \code{document,word,topic,count}. Documents, words, and topics are \emph{one-indexed} 
#' in the result.
#'
#' @param infile the name of a CSV file holding the simplified state: a CSV with header 
#' row and four columns, \code{document,word,topic,count}, where the documents, words, and 
#' topics are \emph{zero-index}. Create the file from MALLET output using 
#' \code{\link{simplify_state}}.
#' @param data_type the C++ type to store the data in. If all values have magnitude 
#' less than \eqn{2^15}, you can get away with \code{short}, but guess what? Linguistic
#' data hates you, and a typical vocabulary can easily include more word types than that.
#' @param big_wordkir the working directory where \code{\link[bigmemory]{read.big.matrix}} 
#' will store its temporary files. By default, uses \code{\link[base]{tempdir}}, but if 
#' you 
#' have more scratch space elsewhere, use that for handling large sampling states.
#'
#' @seealso
#' \code{\link{simplify_state}},
#' \code{\link{write_mallet_state}},
#' \code{\link{term_year_topic_matrix}},
#' \pkg{bigmemory}
#'
#' @export
#'
read_simplified_state <- function(infile,
                                  data_type="integer",
                                  big_workdir=tempdir()) {

    library(bigmemory)
    message("Loading ",infile," to a big.matrix...")
    state <- read.big.matrix(infile,type=data_type,header=T,sep=",",
                             backingpath=big_workdir,
                             backingfile="state.bin",
                             descriptorfile="state.desc")
    message("Done.")

    # change mallet's 0-based indices to 1-based
    state[,1] <- state[,1] + 1L     # docs
    state[,2] <- state[,2] + 1L     # types
    state[,3] <- state[,3] + 1L     # topics

    state
}

#' Calculate yearly term-topic counts
#'
#' Total up the number of times some words are assigned to a given topic.
#'
#' This function makes use of the "simplified state" to look at yearly trends for words 
#' \emph{within} topics. Normally a given word type is divided between multiple topics in 
#' a single document. This allows you to investigate how the model distributes uses of 
#' certain words over time to topics.
#' 
#' @return a list similar to that returned by \code{\link{term_year_matrix}}:
#' \describe{
#'     \item{\code{tym}}{a \code{\link[Matrix]{sparseMatrix}} with terms in vocab order in rows and years in columns}
#'     \item{\code{yseq}}{a character vector mapping columns to years}
#'     \item{\code{topic}}{the value of the \code{topic} parameter}
#' }
#'
#' @param topic one-based topic number
#' @param ss a \code{big.matrix} holding the "simplified state" as returned by 
#' \code{\link{read_simplified_state}}. Operated on using \code{\link[bigmemory]{mwhich}}.
#'
#' @param id_map a character vector mapping document numbers in \code{ss} to JSTOR id's 
#' that can be matched against \code{metadata$id}
#'
#' @param vocab a character vector mapping word numbers in \code{ss} to words as strings
#'
#' @param metadata the dataframe of metadata as returned by \code{\link{read_metadata}}
#'
#' @seealso \code{\link{read_simplified_state}},
#' \code{\link{term_year_matrix}},
#' \code{\link{term_document_topic_matrix}}
#' 
#' @export
#'
term_year_topic_matrix <- function(topic,ss,id_map,vocab,metadata) {

    tdm_topic <- term_document_topic_matrix(topic,ss,id_map,vocab) 

    result <- term_year_matrix(metadata=metadata,
                               tdm=tdm_topic,
                               id_map=id_map,
                               vocabulary=vocab,
                               big=T)

    result$topic <- topic
    result
}

#' The term-document matrix for a topic
#'
#' Extracts a matrix of counts of words assigned to a given topic in each document from 
#' the "simplified" sampling state.
#'
#' @return a \code{\link[Matrix]{sparseMatrix}} with terms in rows (\code{vocab} 
#' order) and documents in columns (\code{id_map} order). No normalization or smoothing is 
#' applied to word counts.
#'
#' @param ss a \code{big.matrix} holding the "simplified state" as returned by 
#' \code{\link{read_simplified_state}}. Operated on using \code{\link[bigmemory]{mwhich}}.
#'
#' @param id_map a character vector mapping document numbers in \code{ss} to JSTOR id's 
#'
#' @param vocab a character vector mapping word numbers in \code{ss} to words as strings
#'
#' @seealso \code{\link{read_simplified_state}},
#' \code{\link{term_year_matrix}},
#' \code{\link{term_year_topic_matrix}}
#' 
#' @export
#'
term_document_topic_matrix <- function(topic,ss,id_map,vocab) {
    library(bigmemory)
    indices <- mwhich(ss,"topic",topic,"eq")

    sparseMatrix(i=ss[indices,"type"],
                 j=ss[indices,"doc"],
                 x=ss[indices,"count"],
                 dims=c(length(vocab),
                        length(id_map)))
}

#' Get top words for a given topic, conditioned by year
#'
#' This is a convenience function for summarizing the results of 
#' \code{\link{term_year_topic_matrix}}.
#' @return a vector of pasted-together words, with the dates as element names
#' 
#' @param tytm matrix with words in rows and years in columns (assumed conditional on a 
#' topic, though this doesn't figure into the calculation)
#' 
#' @param yseq character vector mapping columns of \code{tytm} to dates
#' @param vocab character vector mapping rows of \code{tytm} to words
#' @param n_words number of top words per date to report
#'
#' @seealso
#' \code{\link{term_year_topic_matrix}},
#' \code{\link{weighted_keys_frame}}
#'
#' @export
#'
topic_yearly_top_words <- function(tytm,yseq,vocab,n_words=5) {
    result <- character(length(yseq))
    for (y in seq_along(yseq)) {
        words <- vocab[order(tytm[,y],decreasing=T)[1:n_words]]
        result[y] <- paste(words,collapse=" ")
    }
    names(result) <- yseq
    result
}

#' Get the time series of words within a topic
#'
#' A convenience function to access the yearly totals of a given word (or words) within a 
#' topic from the \code{\link{term_year_topic_matrix}} results
#'
#' @param word character vector of terms
#'
#' @param tytm matrix with terms in rows and years in columns
#'
#' @param character vector mapping rows of \code{tytm} to words (matched against 
#' \code{word})
#' 
#' @seealso
#' \code{\link{term_year_topic_matrix}}
#'
#' @export
#'
topic_term_time_series <- function(word,tytm,vocab) {
    w <- match(word,vocab)
    tytm[w,,drop=F]
}


