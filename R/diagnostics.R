# Functions for access to MALLET's model diagnostics.

#' Save MALLET's topic model diagnostics as XML
#' 
#' Write MALLET's model diagnostics to an XML file.
#' 
#' @param trainer the \code{RTopicModel} object.
#' @param output_file the name of a file to save XML to.
#' @param n_top_words the number of top words per topic to calculate topic-word 
#'   diagnostics for.
#'   
#' @seealso \code{\link{read_diagnostics}}
#' 
#' @export
#' 
write_diagnostics <- function(m, output_file="diagnostics.xml",
                              n_top_words=50L) {
    ptm <- ParallelTopicModel(m)
    if (is.null(ptm)) {
        stop("MALLET model object is not available.")
    }
    d <- .jnew("cc/mallet/topics/TopicModelDiagnostics",
          ptm, as.integer(n_top_words))
    xml <- d$toXML()
    writeLines(xml, output_file)
}

#' Read MALLET model-diagnostic results.
#' 
#' Uses the \pkg{XML} package and \code{libxml} to parse the MALLET diagnostic
#' output.
#' 
#' @param xml_file file holding XML to be parsed.
#'   
#' @return a list of two dataframes of diagnostic information, \code{topics} and
#'   \code{words}. The diagnostics are sparsely documented by the MALLET source 
#'   code (\url{http://hg-iesl.cs.umass.edu/hg/mallet}: see 
#'   \code{src/cc/mallet/topics/TopicModelDiagnostics.java}).
#'   
#'   In \code{topics}, columns include: \describe{
#'   
#'   \item{\code{topic}}{The 1-indexed topic number.}
#'   
#'   \item{\code{corpus_dist}}{The KL-divergence from the corpus. A useful
#'   diagnostic of a topic's distinctiveness.}
#'   
#'   \item{\code{coherence}}{The topic coherence measure defined by Mimno et
#'   al., eq. (1): the sum of log-co-document-document frequency ratios for the
#'   top words in the topic. The number of top words is set in the 
#'   \code{n_top_words} parameter to \code{\link{write_diagnostics}}.}
#'   
#'   }
#'   
#'   The function attempts to coerce numeric values, which \pkg{XML} extracts as
#'   strings, into numbers.
#'   
#' @references David Mimno et al. Optimizing Semantic Coherence in Topic Models.
#' \emph{EMNLP} 2011. 
#' \url{http://www.cs.princeton.edu/~mimno/papers/mimno-semantic-emnlp.pdf}.
#' 
#' @seealso \code{\link{write_diagnostics}}
#' 
#' @export
#' 
read_diagnostics <- function (xml_file) {
    if (!requireNamespace("XML", quietly=T)) {
        stop("The XML package is required to read MALLET diagnostics.")
    }

    d <- xmlParse(file=xml_file)
    # xmlSApply returns a string matrix with topics in *columns*
    topic_attrs <- t(xpathSApply(d, "/model/topic", xmlAttrs))
    # de-stringify: 
    topics <- apply(topic_attrs, 2, as.numeric)
    # add in a 1-indexed "topic" number
    topics <- data.frame(topic=topics[ , "id"] + 1, topics,
                         stringsAsFactors=F)

    word_info <- function(node) {
        w <- xmlValue(node)
        topic <- as.numeric(xmlGetAttr(xmlParent(node), "id")) + 1
        attrs <- xmlAttrs(node)
        c(topic=topic, word=w, attrs)
    }

    # result of this is a string matrix
    wm <- t(xpathSApply(d, "/model/topic/word", word_info))
    w_topics <- as.numeric(wm[ , 1]) # column "topic," re-de-stringified
    w_words <- wm[ , 2] # column "word"

    # de-stringify
    w_rest <- apply(wm[ , 3:ncol(wm)], 2, as.numeric)

    words <- data.frame(topic=w_topics,
                        word=w_words,
                        w_rest,
                        stringsAsFactors=F)


    list(topics=topics, words=words)
}

