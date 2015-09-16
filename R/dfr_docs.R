# Functions for wrangling the DfR wordcount files into MALLET-friendly forms.

#' Convert DfR wordcount files to a long-format data frame
#' 
#' Reads in a bunch of \code{wordcounts*.CSV} files and stacks them up in a 
#' single long-format dataframe. These counts can be optionally manipulated,
#' then passed on to \code{\link{wordcounts_texts}} then
#' \code{\link{make_instances}}.
#' 
#' Empty documents are skipped; DfR supplies wordcounts files for documents that
#' have no wordcount data. These will be in DfR's metadata but not in the output
#' dataframe here.
#' 
#' This is slow. An outboard script in python or Perl is faster, but this keeps 
#' us in R and does everything in memory.
#' 
#' Memory usage: for N typical journal articles, the resulting dataframe seems
#' to need about 20N K of memory. So R on a laptop will hit its limits somewhere
#' around 100K articles of typical length.
#' 
#' @param files individual filenames to read.
#' @param filename_id function that converts a file name into a document ID. By 
#'   default, \code{\link{dfr_filename_doi}} is used.
#' @return A data frame with three columns: \code{id}, the document ID; 
#'   \code{word}, a word type or term (called \code{WORDCOUNTS} in DfR source
#'   data files); \code{weight}, the count.
#' @seealso \code{\link{wordcounts_texts}}, \code{\link{instances_Matrix}} for
#'   word counts \emph{after} stopword removal (etc.).
#'   
#' @export
#' 
read_wordcounts <- function (files, filename_id=dfr_filename_id) {
    if (requireNamespace("readr", quietly=T)) {
        read_wordcounts_readr(files, filename_id)
    } else {
        read_wordcounts_base(files, filename_id)
    }
}

read_wordcounts_base <- function (files, filename_id) {
    result <- data.frame(filename=files, stringsAsFactors=F)
    result <- group_by_(result, ~ filename)
    result <- dplyr::do_(result,
        ~ read.csv(.$filename,
            strip.white=TRUE, header=TRUE, as.is=TRUE,
            colClasses=c("character", "integer"),
            col.names=c("word", "weight"))
    )
    mut <- list(id=~ filename_id(filename),
                ~ word,
                ~ weight)
    dplyr::transmute_(dplyr::ungroup(result), .dots=mut)
}

read_wordcounts_readr <- function (files, filename_id) {
    result <- vector("list", length(files))
    for (i in seq_along(files)) {
        frm <- readr::read_csv(files[i],
            col_names=TRUE, col_types="ci", progress=FALSE)
        # degenerate case of header only gives a row of NAs
        # as of readr 0.1.1
        if (is.na(frm[1, 1])) {
            result[[i]] <- NULL
        } else {
            result[[i]] <- data.frame(
                id=filename_id(files[i]),
                word=frm[[1]],
                weight=frm[[2]],
                stringsAsFactors=F
            )
        }
    }
    result <- dplyr::bind_rows(result)
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
wordcounts_doc_lengths <- function (counts) {
    result <- dplyr::group_by_(counts, ~ id)
    dplyr::summarize_(result, .dots=setNames(list(~ sum(weight)), "length"))
}

#' Calculate total corpus-wide word counts
#' 
#' Given a wordcounts long-format dataframe returned by
#' \code{\link{read_wordcounts}}, calculate total corpus-wide counts for each
#' word type. This is just a convenience function for a straightforward 
#' \code{\link[dplyr]{summarize}}.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @return a data frame with \code{word} and \code{weight} columns
#' @seealso \code{\link{read_wordcounts}},
#'   \code{\link{instances_Matrix}} for word counts
#'   \emph{after} stopword removal (etc.).
#'   
#' @export
#' 
wordcounts_word_totals <- function (counts) {
    result <- dplyr::group_by_(counts, ~ word)
    dplyr::summarize_(result, .dots=setNames(list(~ sum(weight)), "weight"))
}

#' Remove stopwords from a wordcounts data frame
#' 
#' Filter out a vector of words from a wordcounts dataframe.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @param stoplist character vector
#' @return A new data frame with stopwords removed
#' @export
#' 
wordcounts_remove_stopwords <- function (counts, stoplist) {
    flt <- lazyeval::interp(~ !(word %in% x), x=stoplist)
    dplyr::filter_(counts, flt)
}

#' Remove infrequent words
#' 
#' Filter out the words in a wordcounts dataframe whose overall frequency is 
#' below a threshold.
#' 
#' It's often useful to prune documents of one-off words (many of which are 
#' OCR errors) before building MALLET instances. This is a convenience function 
#' for doing so.
#' 
#' @param counts The dataframe from \code{\link{read_wordcounts}}
#' @param n The maximum rank to keep: all words with frequency rank below 
#'   \code{n} will be discarded
#' @return A filtered word-counts dataframe. Because of ties, do not expect 
#'   it to have exactly \code{n} distinct words.
#'   
#' @export
#' 
wordcounts_remove_rare <- function (counts, n) {
    word_totals <- wordcounts_word_totals(counts)
    # min_rank used here, giving a more aggressive filter than dense_rank
    keep <- word_totals$word[
        dplyr::min_rank(-word_totals$weight) <= n
    ]

    # You'd think you could semi_join here, but that will scramble the order of
    # rows, so we'll use the ugly way:

    flt <- lazyeval::interp(~ word %in% x, x=keep)
    dplyr::filter_(counts, flt)
}
    
#' Convert long-format word-counts into documents
#' 
#' This naively "inflates" word counts into a bag of words, for sending to 
#' MALLET.
#' 
#' You can directly pass the result from \code{link{read_wordcounts}} to this
#' function, but normally you'll want to filter or otherwise manipulate the
#' words first.
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
wordcounts_texts <- function (counts, shuffle=FALSE, sep=" ") {
    counts <- dplyr::group_by_(counts, ~ id)
    if (shuffle) {
        counts <- dplyr::sample_frac(counts)
    }

    smz <- setNames(list(
        ~ stringr::str_c(rep(word, times=weight), collapse=sep)
        ),
        "text")
    dplyr::summarize_(counts, .dots=smz)
}
