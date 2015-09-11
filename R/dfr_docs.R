# Functions for wrangling the DfR wordcount files into MALLET-friendly forms.

#' Convert DfR wordcount files to a long-format data frame
#' 
#' Reads in a bunch of \code{wordcounts*.CSV} files and stacks them up in a 
#' single long-format dataframe. Invoked by
#' \code{\link{read_wordcounts_wordcounts}}, but use it on your own if you wish
#' to operate on the aggregate word-count frame before collapsing it back into
#' documents.
#' 
#' Empty documents are skipped; DfR supplies wordcounts files for documents that
#' have no wordcount data. These will be in DfR's metadata but not in the output
#' dataframe here.
#' 
#' This is slow. An outboard script in python or Perl is faster, but this keeps 
#' us in R and does everything in memory. Memory usage: for N typical journal 
#' articles, the resulting dataframe seems to need about 20N K of memory. So R 
#' on a laptop will hit its limits somewhere around 100K articles of typical 
#' length.
#' 
#' @param files individual filenames to read.
#' @param filename_id function that converts a file name into a document ID. By
#'   default, \code{\link{dfr_filename_doi}} is used.
#' @return A data frame with three columns: \code{id}, the document ID; 
#'   \code{feature}, a feature counted by JSTOR (i.e. a word type, called 
#'   \code{WORDCOUNTS} in DfR source data files); \code{weight}, the count.
#' @seealso \code{\link{read_wordcounts_wordcounts}}, 
#'   \code{\link{instances_term_document_matrix}} for feature counts
#'   \emph{after} stopword removal (etc.).
#'   
#' @export
#' 
read_wordcounts <- function (files, filename_id=dfr_filename_id) {
    result <- data_frame(filename=files) %>%
        group_by(filename) %>%
        do({
            read.csv(.$filename,
                     strip.white=T, header=T, as.is=T,
                     colClasses=c("character", "integer"))
        }) %>%
        ungroup() %>%
        mutate(filename=filename_id(filename))

    colnames(result) <- c("id", "feature", "weight")
    result
}

#' Calculate document lengths
#' 
#' Given a wordcounts long-format dataframe returned by
#' \code{\link{read_wordcounts}}, calculate document lengths. This is just a
#' convenience function for a straightforward \code{\link[dplyr]{summarize}}.
#' It's often useful to filter a set of documents by (unstopped) length before
#' modeling, and it's a good idea to check the distribution of document lengths
#' before modeling, as this can substantially influence modeling outcomes.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @return A dataframe with \code{id} and \code{length} columns
#'   
#' @seealso \code{\link{read_wordcounts}}
#'   
#' @export
#' 
dfr_doc_lengths <- . %>%
    group_by(id) %>%
    summarize(length=sum(weight))

#' Calculate total corpus-wide feature counts
#' 
#' Given a wordcounts long-format dataframe returned by
#' \code{\link{read_wordcounts}}, calculate total corpus-wide counts for each
#' word type. This is just a convenience function for a straightforward 
#' \code{\link[dplyr]{summarize}}.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @return a data frame with \code{feature} and \code{weight} columns
#' @seealso \code{\link{read_wordcounts}},
#'   \code{\link{instances_term_document_matrix}} for feature counts
#'   \emph{after} stopword removal (etc.).
#'   
#' @export
#' 
dfr_feature_totals <- . %>%
    group_by(feature) %>%
    summarize(weight=sum(weight))

#' Remove stopwords from a wordcounts data frame
#' 
#' Filter out a vector of words from a wordcounts dataframe.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @param stoplist character vector
#' @return A new data frame with stopwords removed
#' @export
#' 
dfr_remove_stopwords <- function (counts, stoplist) {
    counts %>%
        filter(!(feature %in% stoplist))
}

#' Remove infrequent terms
#' 
#' Filter out the features in a wordcounts dataframe whose overall frequency is 
#' below a threshold.
#' 
#' It's often useful to prune documents of one-off features (many of which are 
#' OCR errors) before building MALLET instances. This is a convenience function 
#' for doing so.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @param n The maximum rank to keep: all terms with frequency rank below 
#'   \code{n} will be discarded
#' @return A filtered feature-counts dataframe. Because of ties, do not expect 
#'   it to have exactly \code{n} distinct features.
#'   
#' @export
#' 
dfr_remove_rare <- function (counts, n) {
    feature_totals <- counts %>%
        group_by(feature) %>%
        summarize(weight=sum(weight)) %>%
        # min_rank used here, giving a more aggressive filter than dense_rank
        filter(min_rank(desc(weight)) <= n)

    # You'd think you could semi_join here, but that will scramble the order of
    # rows, so we'll use the ugly way:
    counts %>%
        filter(feature %in% feature_totals$feature)
}
    
#' Convert long-format feature-counts into documents
#' 
#' This naively "inflates" feature counts into a bag of words, for sending to 
#' MALLET.
#' 
#' You can directly pass the result from \code{link{read_wordcounts}} to this
#' function, but normally you'll want to filter or otherwise manipulate the
#' features first.
#' 
#' It is not straightforward to supply feature vectors directly to MALLET; 
#' MALLET really wants to featurize each text itself. So our task is to take the
#' wordcounts supplied from DfR and reassemble the texts. If DfR tells us word w
#' occurs N times, we simply paste N copies of w together, separated by spaces 
#' (or the value of \code{sep} if given). Though LDA should not care about word 
#' order, if you are nervous about the effects of the decidedly non-natural 
#' ordering of words this produces on the modeling process, you can randomize 
#' the word order (it still won't be natural). Thanks to David Mimno for 
#' suggesting this via his own \code{\link[mallet]{mallet}} code.
#' 
#' A big waste of memory, but this is the simple way to get DfR files into 
#' MALLET.
#' 
#' @param counts long-format data frame like that returned by 
#'   \code{\link{read_wordcounts}}
#' @param shuffle if \code{TRUE}, randomize word order within document before 
#'   pasting it together. \code{FALSE} by default.
#' @param sep word separator in inflated bags. A space, by default.
#'   
#' @return a dataframe with two columns: \code{id}, the document id; 
#'   \code{text}, the full document text as a single line (with the words in 
#'   meaningless order)
#'   
#' @seealso \code{\link{read_wordcounts}}
#'   
#' @export
#' 
dfr_docs_frame <- function (counts, shuffle=F, sep=" ") {
    counts <- counts %>% group_by(id)
    if (shuffle) {
        counts <- counts %>% sample_frac()
    }
            
    counts %>% 
        summarize(text=str_c(rep(feature, times=weight), collapse=sep))
}
