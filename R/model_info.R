# Functions for extracting basic information about the model

#' Get the "top" words in a topic
#'
#' Extracts the most salient words from the weighted key-words frame.
#'
#' The "top" words are those with the maximum weightings for the topic in \code{wkf}. In 
#' addition to raw ranking (the default for \code{\link{weighted_keys_frame}}, consider 
#' the "salience" score calculated by \code{\link{topic_word_scores}}.
#'
#' @param topic the topic number (one-based)
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param n the number of words to take. If NULL, the \code{threshold} is used 
#' instead.
#' @param threshold if \code{n} is NULL, this is used to determine how many words to take: 
#' all words with weight greater than \code{threshold * max(wkf$weight)} are kept.
#'
#' @return a character vector of words
#'
#' @seealso
#' \code{\link{topic_name}},
#' \code{\link{topic_names}},
#' \code{\link{topic_labeller}},
#' \code{\link{weighted_keys_frame}}
#' 
#' @export
#'
topic_top_words <- function(topic,wkf,n=4,threshold=0.5) {
    wkf <- wkf[wkf$topic==topic,]
    if(is.null(n)) {
        threshold <- max(wkf$weight) * 0.5
        wkf <- wkf[wkf$weight >= threshold,]
        words <- wkf$word[order(wkf$weight,decreasing=T)]
    } else {
        words <- wkf$word[order(wkf$weight,decreasing=T)[1:n]]
    }

    words
}

#' Get a label for a topic
#'
#' Returns a label for a topic based on the most prominent words
#'
#' @param topic the topic number (one-based)
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param name_format \code{\link[base]{sprintf}} format string with one slot for the 
#" topic and one for a string
#' @param ... passed on to \code{\link{topic_top_words}}: for example, 
#' \code{n,threshold}.
#' @return a string
#'
#' @seealso
#' \code{\link{topic_top_words}},
#' \code{\link{weighted_keys_frame}}
#' 
#' @export
#'
topic_name <- function(topic,wkf,name_format="%03d %s",...) {
    words <- topic_top_words(topic,wkf,...)

    words_str <- paste(words, collapse=" ")

    sprintf(name_format,topic,words_str)
}

#' Get labels for many topics
#'
#' Returns labels for the specified topics.
#'
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param topics which topics to get labels for (all, by default)
#' @param name_format \code{\link[base]{sprintf}} format string with one slot for the 
#" topic and one for a string
#' @param ... passed on to \code{\link{topic_name}}: for example, 
#' \code{n,threshold}.
#' @return a character vector of labels
#'
#' @seealso
#' \code{\link{topic_name}},
#' \code{\link{topic_top_words}},
#' \code{\link{weighted_keys_frame}}
#' 
#' @export
#'
topic_names <- function(wkf,topics=unique(wkf$topic),name_format="%03d %s",...) {
    ws <- lapply(topics,topic_name,wkf=wkf,name_format=name_format,...)
    sapply(ws,paste,collapse=" ")
}

#' Get a function to label topics
#'
#' Convenience wrapper for currying \code{\link{topic_name}}  to give a function of a 
#' single argument, to be used in conjunction with some plotting functions. 
#'
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param ... passed on to \code{\link{topic_name}}
#'
#' @return a function of a single variable, mapping topic numbers to labels
#'
#' @seealso
#' \code{\link{topic_name}},
#' \code{\link{topic_top_words}},
#' \code{\link{weighted_keys_frame}},
#' \code{\link{topic_yearly_lineplot}},
#' \code{\link{topic_yearly_barplot}},
#' \code{\link{topic_dist_plot}}
#' 
#' @export
#'
topic_labeller <- function(wkf,...)
    function (topic) { topic_name(topic,wkf=wkf,...) }
}

#' Get the "top" documents for a topic
#'
#' Constructs a dataframe with the id's and weights with the "top" documents for a topic.
#'
#' @param topic The topic in question
#' @param doc_topics matrix with documents in rows and topic weights in columns
#' @param id_map character vector mapping rows of \code{doc_topic} to JSTOR doc id's: with 
#' the \code{doc_topics_frame}, simply use the \code{id} column
#' @param n number of top documents to extract
#' @param method the notion of a "top" document is not well-specified. Currently this 
#' function knows only two simple methods: \describe{
#'      \item{\code{"raw"}}{maximum scores in the topic-column of the 
#' \code{doc_topic}}
#'      \item{\code{"max_frac"}}{maximum when document-topic scores are normalized to sum 
#' to 1. Note that a topic may reach its maximum proportion in a document and yet that 
#' document may yet have a larger proportion of another topic.}}
#' @return a data frame with \code{n} rows ordered from weightiest to least weighty, and 
#' two columns: \code{id} and \code{weight}. The id's can be passed to, e.g. 
#' \code{\link{cite_articles}}.
#'
#' @seealso
#' \code{\link{doc_topics_matrix}},
#' \code{\link{doc_topics_frame}},
#' \code{\link{cite_articles}}
#'
#' @export
#'
top_documents <- function(topic,doc_topics,id_map,n=5,method="raw") {
    if(method=="raw") {
        doc_scores <- doc_topics[,topic]
    } else if(method=="max_frac") {
        doc_scores <- doc_topics[,topic] / rowSums(doc_topics)
    } else {
        stop("Unknown method.")
    }

    indices <- order(doc_scores,decreasing=T)[1:n]
    
    ids <- id_map[indices]
    wts <- doc_scores[indices]

    data.frame(id=ids,weight=wts)
}

#' Get top topics for a document
#'
#' This function extracts the most salient topics for a document from a document-topic 
#' matrix.
#'
#' The result means different things, depending on whether the matrix is
#' normalized per topic. If not, one gets the topics that
#' have been assigned the largest number of words in a document. But
#' if of the matrix is column-normalized, then one gets the topics for which the
#' document is comparatively most prominent within that topic.
#'
#' @param id a JSTOR document ID, matched against \code{id_map}
#' @param doc_topics a matrix with documents in rows and topic scores in columns
#' @param id_map mapping from rows of \code{doc_topics} to id's
#' @param n number of topics to extract
#'
#' @return a dataframe with \code{n} rows and two columns, \code{topic} and \code{weight}.
#'
#' @seealso
#' \code{\link{doc_topics_matrix}},
#' \code{\link{doc_topics_frame}}
#' 
#' @export
#'
top_topics <- function(id,doc_topics,id_map,n=5) {
    i <- match(id,id_map)
    indices <- order(doc_topics[i,],decreasing=T)[1:n]

    data.frame(topic=indices,weight=doc_topics[i,indices])
}
