# Functions for wrangling the DfR wordcount files into MALLET-friendly forms.

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
