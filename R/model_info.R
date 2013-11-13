
#' ---------------
#' About documents
#' ---------------

#' Return a frame with ids and weights with the "top" documents for a topic
#'
#' method: the notion of a "top" document is not well-specified.
#'       "raw":  maximum scores in the topic-column of the dtm.
#'       "max_frac": maximum after normalizing the topic-column of the dtm. A 
#'       topic may reach its maximum proportion in a document and yet that 
#'       document may yet have a larger proportion of another topic.

top_documents <- function(topic,id_map,dtm,n=5,method="raw") {
    if(method=="raw") {
        doc_scores <- dtm[,topic]
    } else if(method=="max_frac") {
        doc_scores <- dtm[,topic] / rowSums(dtm)
    } else {
        stop("Unknown method.")
    }

    indices <- order(doc_scores,decreasing=T)[1:n]
    
    ids <- id_map[indices]
    wts <- doc_scores[indices]

    data.frame(id=ids,weight=wts)
}

#' This one means different things, depending on whether dtm is
#' normalized per topic. If dtm is raw counts, one gets the topics that
#' have been assigned the largest number of words in a document. But
#' if dtm is column-normalized, then one gets the topics for which the
#' document is comparatively most prominent within that topic.

top_topics <- function(id,id_map,dtm,n=5) {
    i <- match(id,id_map)
    indices <- order(dtm[i,],decreasing=T)[1:n]

    data.frame(topic=indices,weight=dtm[i,indices])
}

#' ------------
#' About topics
#' ------------

topic_name <- function(topic,wkf,n=0,threshold=0.5,
                       name_format="%03d %s") {
    words <- topic_top_words(topic,wkf,n,threshold)

    words_str <- paste(words, collapse=" ")

    sprintf(name_format,topic,words_str)
}

#' or the above applied to many topics at once
topic_names <- function(wkf,n=2,topics=NULL,
                        name_format="%03d %s") {
    if(length(topics) == 0) {
        topics <- 1:length(unique(wkf$topic))
    }
    ws <- lapply(topics,topic_name,wkf=wkf,n=n,
                 name_format=name_format)
    sapply(ws,paste,collapse=" ")
}

topic_labeller <- function(wkf,n=2,name_format="%03d %s") {
    function (topic) { topic_name(topic,wkf,n=n,name_format=name_format) }
}

topic_top_words <- function(topic,wkf,n=0,threshold=0.5) {
    wkf <- wkf[wkf$topic==topic,]
    if(n <= 0) {
        threshold <- max(wkf$weight) * 0.5
        wkf <- wkf[wkf$weight >= threshold,]
        words <- wkf$word[order(wkf$weight,decreasing=T)]
    } else {
        words <- wkf$word[order(wkf$weight,decreasing=T)[1:n]]
    }

    words
}
