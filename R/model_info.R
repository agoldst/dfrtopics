# Functions for extracting basic information about the model

#' Quick shorthands for topics
#' 
#' These "labels" simply name each topic by its 1-indexed number and the top
#' \code{n} words by weight.
#' 
#' @param m \code{dfr_lda} object
#' @param n number of words to use in label
#'   
#' @return a character vector, one label for each topic
#'   
#' @export
topic_labels <- function (m, n=8) {
    top_words(m, n) %>%
        group_by(topic) %>%
        slice(1:n) %>%
        summarize(label=str_c(word, collapse=" ")) %>%
        ungroup() %>%
        transmute(label=str_c(topic, label, sep=" ")) %>%
        unlist() %>%
        unname()
}

#' Top-ranked documents in topics
#' 
#' Extracts a data frame of documents scoring high in each topic. Documents are 
#' represented as numeric indices. The scoring is done on the basis of the 
#' document-topic matrix, but here some care is needed in deciding about cases
#' in which a document has more of its words assigned to a given topic but a
#' smaller proportion of that topic than some other, shorter document. By
#' default all documents are normalized to length 1 before ranking here.
#' 
#' Note also that a topic may reach its maximum proportion in a document even if
#' that document has a yet larger proportion of another topic. To adjust the
#' scoring, pass a function to transform the document-topic matrix in the
#' \code{weighting} parameter. If you wish to use raw weights rather than
#' proportions to rank documents, set \code{weighting=identity}. Raw weights
#' give longer documents an unfair advantage, whereas proportions often give
#' shorter documents an advantage (because short documents tend to be dominated
#' by single topics in LDA).
#' 
#' TODO: alternative scoring methods.
#' 
#' @param m \code{dfr_lda} object
#' @param n number of top documents to extract
#' @param weighting a function to transform the document-topic matrix. By
#'   default \code{\link{dt_smooth_normalize}(m)}, a normalized weighting
#'   function
#' @return a data frame with three columns, \code{topic}, \code{doc}, the
#'   numerical index of the document in \code{\link{doc_ids}(m)}, and
#'   \code{weight}, the weight used in ranking (topic proportion, raw score,
#'   ...)
#'   
#' @seealso \code{\link{doc_topics}}, \code{\link{dt_smooth_normalize}}
#' 
#' @examples
#' \dontrun{
#' # obtain citations for 3 documents with highest proportions of topic 4
#' top_docs(m, 3) %>%
#'     filter(topic == 4) %>%
#'     select(-topic) %>%
#'     mutate(citation=cite_articles(metadata(m)[doc, ]))
#' }
#' 
#' @export
#' 
top_docs <- function (m, n, weighting=dt_smooth_normalize(m)) {
    dtm <- weighting(doc_topics(m))

    ij <- top_n_col(dtm, n) 
    data_frame(topic=ij[ , 2],
               doc=ij[ , 1],
               weight=dtm[ij])
}

#' Top-ranked topics for documents
#' 
#' This function extracts the most salient topics for all documents from the
#' document-topic matrix.
#' 
#' Here as elsewhere "saliency" can be variously defined: though the easiest
#' choice is to choose the topic which captures the largest proportion of a
#' document, and that is the default, we might want to penalize topics which are
#' widespread across the whole corpus. TODO: actually implement the alternative
#' weighting.
#' 
#' @param m \code{dfr_lda} object
#' @param n number of top topics to extract
#' @param weighting a function to transform the document-topic matrix. By
#'   default, the topic proportions are used (same rank as raw weights)
#' @return a data frame with three columns, \code{doc}, the numerical index of
#'   the document in \code{\link{doc_ids}(m)}, \code{topic}, and \code{weight},
#'   the weight used in ranking (topic proportion, by default)
#'   
#' @return a dataframe with \code{n} rows and two columns, \code{topic} and
#'   \code{weight}.
#'   
#' @seealso \code{\link{doc_topics}}
#' 
#' @export
#' 
docs_top_topics <- function (m, n, weighting=dt_smooth_normalize(m)) {
    dtm <- weighting(doc_topics(m))

    ij <- top_n_row(dtm, n)
    data_frame(doc=ij[ , 1],
               topic=ij[ , 2],
               weight=dtm[ij])
}

#' Top-ranked topics for documents
#' 
#' This function extracts the most salient topics for all words in the topic- 
#' word matrix (which must be available).
#' 
#' Here as elsewhere "saliency" can be variously defined: the easiest choice is
#' to choose the topic which captures the largest proportion of a word's usage,
#' and that is the default. TODO: actually implement the alternative weighting.
#' 
#' @param m \code{dfr_lda} object
#' @param n number of top topics to extract
#' @param weighting a function to transform the topic-word matrix. By default,
#'   the topic proportions are used (same rank as raw weights)
#' @return a data frame with three columns, \code{word}, \code{topic}, , and
#'   \code{weight}, the weight used in ranking (topic proportion, by default)
#'   
#' @return a dataframe with \code{n} rows and two columns, \code{topic} and
#'   \code{weight}.
#'   
#' @seealso \code{\link{topic_words}}
#' 
#' @export
#' 
words_top_topics <- function (m, n, weighting=tw_smooth_normalize(m)) {
    tw <- weighting(topic_words(m))

    ij <- top_n_col(tw, n)
    data_frame(word=vocabulary(m)[ij[ , 2]],
               topic=ij[ , 1],
               weight=tw[ij])
}
