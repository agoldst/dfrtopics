# Functions for extracting basic information about the model

#' Get the "top" words in a topic
#'
#' Extracts the most salient words from the weighted key-words frame.
#' 
#' The "top" words are those with the maximum weightings for the
#' topic in \code{wkf}. In addition to raw ranking (the default for
#' \code{\link{weighted_keys_frame}}, consider the "salience" score
#' calculated by \code{\link{topic_word_scores}}.  
#'
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param topic the topic number (one-based). Vectorized in topics. By default, 
#' all topics are used.
#' @param n the number of words to take. 
#'
#' @return a character matrix with the top words in rows and topics in columns. 
#' The rownames give the topic numbers (as strings).
#'
#' @seealso
#' \code{\link{topic_name}},
#' \code{\link{topic_names}},
#' \code{\link{topic_labeller}},
#' \code{\link{weighted_keys_frame}}
#' 
#' @export
#'
topic_top_words <- function(wkf,topic=wkf$topic,n=4) {
    daply(wkf[wkf$topic %in% topic,],"topic",
          function (d) {
            d$word[order(d$weight,decreasing=T)[1:n]]
          })
}

#' Get a label for a topic
#'
#' Returns a label for a topic based on the most prominent words
#'
#' @param topic the topic number (one-based)
#' @param wkf the weighted key-word frame from \code{\link{weighted_keys_frame}}
#' @param fmt \code{\link[base]{sprintf}} format string with one slot 
#' for the topic number and one for a string
#' @param ... passed on to \code{\link{topic_top_words}}: for example, 
#' \code{n}.
#' @return a string
#'
#' @seealso
#' \code{\link{topic_top_words}},
#' \code{\link{weighted_keys_frame}}
#' 
#' @export
#'
topic_name <- function(wkf,topic=unique(wkf$topic),
                       fmt="%03d %s",...) {
    words <- topic_top_words(wkf,topic,...)

    if(length(unique(topic)) > 1) {
        words_str <- apply(words,1,paste,collapse=" ")
    } else {
        words_str <- paste(words,collapse=" ")
    }

    sprintf(fmt,topic,words_str)
}

#' Get labels for many topics
#'
#' Returns labels for the specified topics. Deprecated: use topic_name (which
#' is vectorized).
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
    topic_name(wkf,topics,name_format,...)
}

#' Get a function to label topics
#'
#' Convenience wrapper for currying \code{\link{topic_name}} to give a
#' function of a single argument, to be used in conjunction with some
#' plotting functions. Can also be spelled \code{\link{topic_labeler}}.  
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
topic_labeller <- function(wkf,...) {
    function (topic) { topic_name(wkf=wkf,topic,...) }
}

#' Get a function to label topics
#'
#' Convenience wrapper for currying \code{\link{topic_name}} to give a
#' function of a single argument, to be used in conjunction with some
#' plotting functions. Can also be spelled \code{\link{topic_labeller}}.  
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
topic_labeler <- topic_labeller

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
