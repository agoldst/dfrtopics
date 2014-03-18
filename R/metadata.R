# Functions to read and wrangle JSTOR metadata

#' Make a dataframe from \code{citations.CSV} files
#'
#' Reads in metadata from JSTOR's metadata files and returns a combined dataframe.
#'
#' This function assumes that each file has a trailing comma at the end of every line,
#' so we expect \code{\link{read.csv}} to find an extra dummy field. DfR has
#' changed their output data format before, so check results carefully.
#'
#' NB. JSTOR puts a trailing tab at the end of each
#' entry in fields that can contain multiple tab-separated entries (e.g.
#' author)---even if there is only a single entry.
#'
#' @param filenames vector of \code{citations.CSV} filenames
#' @param ... passed on to \code{\link{read.csv}}; the
#' most useful may be \code{strip.white}, which determines whether
#' \code{\link{read.csv}} will strip leading and trailing whitespace
#' from each field. 
#' @return A dataframe with deduplicated metadata. The function issues a warning if there 
#' are lines that have the same id field but are not identical.
#' @seealso \code{\link{pubdate_Date}}
#' @export
#'
read_metadata <- function(filenames,...) {
    all_rows <- do.call(rbind,lapply(filenames,read_citations,...))
    # deduplicate
    result <- unique(all_rows)

    if(any(duplicated(result$id))) {
        warning("Some rows have the same id")
    }

    result
}

#' Read a single \code{citations.CSV} file.
#'
#' This function is mostly a helper for \code{\link{read_metadata}}. It reads a single
#' \code{citations.CSV file}.
#'
#' @param filename the CSV file to read. If \code{NA}, opens the file dialog.
#' @param ... Passed on to \code{\link{read.csv}}.
#' @return A dataframe of metadata.
#' @seealso \code{\link{read_metadata}}
#' @export
#'
read_citations <- function(filename=NA,...) { 
    f <- filename
    if(is.na(filename)) { 
        cat("Select citations.CSV file from jstor dfr...\n")
        ignore <- readline("(press return to open file dialog) ")
        f <- file.choose()
        print(f)
    }

    # the nefarious trailing comma (see read_metadata docs)
    cols <- scan(f,nlines=1,what=character(),sep=",",quiet=T)
    cols <- c(cols,"unused")

    subset(read.csv(f,skip=1,header=F,col.names=cols,quote="",as.is=T,...),
           select=-unused)
}

    
#' Convert JSTOR document id's to \code{wordcounts*.CSV} filenames
#'
#' Convenience function for turning an ID like \code{10.2307/3175328} into a DfR wordcount 
#' filename like \code{wordcounts_10.2307_3175328.CSV}.
#'
#' @param id a character vector of document id's
#' @return a character vector of filenames
#' @examples
#' id_filename("10.2307/3175328")
#' @seealso \code{\link{filename_id}}
#' @export
#'
id_filename <- function(id) {
    result <- paste("wordcounts_",id,".CSV",sep="")
    gsub("/","_",result,fixed=TRUE)
}

#' Convert wordcount filenames to JSTOR document id's
#'
#' Convenience function for turning a file path like 
#' \code{path/to/wordcounts_10.2307_3175328.CSV} into an id like
#' \code{10.2307/3175328}.
#'
#' The file extension can be anything alphabetic.
#'
#' @return id a character vector of document id's
#' @param filename a character vector of filenames
#' @examples
#' filename_id("path/to/wordcounts_10.2307_3175328.CSV")
#' @seealso \code{\link{id_filename}}
#' @export
#'
filename_id <- function(filename) {
  result <- sub("^.*wordcounts_","",filename)
  result <- sub("\\.[[:alpha:]]*$","",result)
  gsub("_","/",result)
}

#' Convert JSTOR pubdate fields to Date objects 
#'
#' @param pubdate a character vector of JSTOR pubdates
#' @return a vector of Dates
#' @export
#'
pubdate_Date <- function(pubdate) {
    as.Date(substr(pubdate,1,10))
}

#' Convert a DfR ID into a JSTOR URL
#'
#' For viewing a document on JSTOR. This works often, but not always.
#'
#' @param id a document id
#' @param jstor_direct if TRUE, try to guess a direct \code{jstor.org/stable/} URL; if FALSE, supply a \code{dx.doi.org} URL (doesn't always resolve).
#' @param proxy added to the URL domain for proxying (e.g. \code{".libraries.example.edu"})
#' @export
#'
dfr_id_url <- function(id,jstor_direct=F,
                       proxy="") {
    if(jstor_direct) {
        sub("^.*\\/","http://www.jstor.org/stable/",id)
    } else {
        paste("http://dx.doi.org",proxy,"/",id,sep="")
    }
}

#' Generate simple citation strings from metadata
#'
#' Given a dataframe with metadata and (optionally) document id's,
#' return a character vector of citations.
#'
#' The generated citations are meant for quick reference, not formal
#' use. They do not handle quotations with quotations correctly and make
#' no effort to scrub the cruft found in some journals' metadata on
#' JSTOR. Authors are simply concatenated into a long "A and B and C"
#' list.
#'
#' @param metadata data frame (from e.g. \code{\link{read_metadata}}) 
#' @param ids character vector of document id's to generate citations for. If NULL, 
#' generate citations for all rows of \code{metadata}
#' @return a character vector of citations
#'
#' @export
#'
cite_articles <- function(metadata,ids=NULL)  {
    if(!is.null(ids)) {
        metadata <- metadata[metadata$id %in% ids,] 
        metadata <- metadata[match(ids,metadata$id),]
    }
    authors <- strsplit(metadata$author,"\t")
    authors <- sapply(authors,paste,collapse=" and ")
    authors[authors==""] <- "[Anonymous]"

    dates <- pubdate_Date(metadata$pubdate)
    dates <- strftime(dates,"%B %Y")
    pp <- gsub("^p?p\\. ","",metadata$pagerange)
    result <- with(metadata,
                   paste(authors,', "',title,'," *',
                         journaltitle,'* ',volume,", no. ",
                         issue," (",dates,"): ",pp,".",
                         sep=""))

    result <- gsub("_",",",result)
    result <- gsub("\t","",result)
    result
}


#copublication_matrix <- function(metadata) {
#    stop("Unimplemented.")

    # TODO implemement
    # split author fields on tabs to get multiple authors
    # determine issues by journaltitle + volume + issue
    # M_ij = 1 iff author_i and author_j copublish in an issue
    # return M and author index
#}
