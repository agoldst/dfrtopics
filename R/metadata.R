#' Make a dataframe from \code{citations.CSV} or \code{citations.tsv} metadata 
#' files
#' 
#' Reads in metadata from any number of tabular data files and returns a 
#' combined dataframe. Most of the work is done by 
#' \code{\link{read_dfr_citations}}.
#' 
#' @param filenames vector of \code{citations.CSV|tsv} filenames
#' @return A dataframe with deduplicated metadata. The function issues a warning
#'   if there are lines that have the same id field but are not identical.
#' @seealso \code{\link{read_dfr_citations}}
#' @export
#' 
read_dfr_metadata <- function (filenames) {
    all_rows <- do.call(rbind, lapply(filenames, read_dfr_citations))
    # deduplicate
    result <- unique(all_rows)

    if(any(duplicated(result$id))) {
        warning("Some rows have the same id")
    }

    result
}

#' Read a single \code{citations.CSV} or \code{citations.tsv} file.
#' 
#' This function reads in a single \code{citations.CSV} (2013 and earlier) or 
#' \code{citations.tsv} (2014 and after) from JSTOR DfR. It knows about the 
#' eccentricities of these formats. Use \code{\link{read_dfr_metadata}} to load 
#' and aggregate multiple files.
#' 
#' This function assumes that each file has a trailing delimeter at the end of 
#' every line. DfR has changed their output data format before, so check results
#' carefully.
#' 
#' We do some minimal post-processing of the data. White space is trimmed by
#' default. Publication dates in the \code{pubdate} column are converted to
#' \code{Date} objects (but beware the false precision of these dates; see
#' \code{\link{pubdate_Date}}. The \code{author} column may contain multiple
#' names, so this is split into a list column. The \code{type} column is
#' converted to a factor.
#' 
#' Notes about other fields: the \code{doi} column is, in my experience, always
#' identical to the \code{id} field, but it is kept here just in case. The
#' \code{title} and \code{abstract} fields may contain markup (HTML or even
#' LaTeX). Most DfR documents lack abstracts in the metadata.
#' 
#' Extra parameters to this function are passed on to \code{read.csv} or
#' \code{read.table}.
#' 
#' @param filename the file to read. If \code{NA}, opens the file dialog.
#' @param strip.white passed to \code{read.table}: by default, white space is 
#'   stripped.
#' @param ... Passed on to \code{\link{read.csv}} or \code{\link{read.table}}.
#' @return A dataframe of metadata.
#' @seealso \code{\link{read_dfr_metadata}}, \code{\link{pubdate_Date}}
#' @export
#' 
read_dfr_citations <- function (filename, strip.white=T, ...) {
    if (grepl("\\.tsv", filename, ignore.case=T)) {
        # new (2014) metadata format: TSV

        # nefarious trailing comma now a nefarious trailing tab
        cols <- scan(filename, nlines=1, what=character(), sep="\t", quiet=T)
        if (length(cols) != 13) {
            warning("Expected 13 tab-delimited columns but found ",
                    length(cols), "\nResults may not be valid")
        }
        cols <- c(cols, "unused")

        result <- read.table(filename, header=F, skip=1, sep="\t",
                             col.names=cols, quote="", as.is=T,
                             comment="", strip.white=strip.white, ...)
        result <- result[ , -length(cols)]
        author_sep <- ", "
    } else {
        # assume old (2013) metadata format: CSV

        # the nefarious trailing comma
        cols <- scan(filename, nlines=1, what=character(), sep=",", quiet=T)
        cols <- c(cols, "unused")

        result <- read.csv(filename, skip=1, header=F, col.names=cols,
                           quote="", as.is=T, comment="",
                           strip.white=strip.white, ...)
        result <- result[ , -length(cols)]
        author_sep <- "\t"
    }

    result$pubdate <- pubdate_Date(result$pubdate)
    result$author <- str_split(result$author, author_sep)
    result$type <- factor(result$type)
    result
}

    
#' Convert JSTOR document id's to \code{wordcounts*.CSV} filenames
#' 
#' Convenience function for turning an ID like \code{10.2307/3175328} into a DfR
#' wordcount filename like \code{wordcounts_10.2307_3175328.CSV}.
#' 
#' @param id a character vector of document id's
#' @return a character vector of filenames
#' @examples
#' id_dfr_filename("10.2307/3175328")
#' @seealso \code{\link{filename_id}}
#' @export
#' 
id_dfr_filename <- function (id) {
    result <- paste("wordcounts_", id, ".CSV", sep="")
    gsub("/", "_", result, fixed=TRUE)
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
#' dfr_filename_id("path/to/wordcounts_10.2307_3175328.CSV")
#' @seealso \code{\link{id_filename}}
#' @export
#' 
dfr_filename_id <- function (filename) {
    result <- sub("^.*wordcounts_", "", filename)
    result <- sub("\\.[[:alpha:]]*$", "", result)
    gsub("_", "/", result)
}

#' Convert JSTOR pubdate strings to Date objects
#' 
#' This function converts JSTOR publication-date metadata into Date objects, 
#' which are more suitable to arithmetic and visualization. The \code{lubridate}
#' package is extremely useful for handling these.
#' \code{\link{read_dfr_citations}} uses this function in reading JSTOR
#' metadata, so you should only use this yourself if you are loading metadata
#' another way.
#' 
#' JSTOR represents publication dates, with considerable false precision, as ISO
#' 8601 date-time strings, e.g. \code{1912-10-01T00:00:00Z}. In my experience
#' all such dates are given with a time of midnight UTC. This function ignores
#' the time specification and keeps only the date (otherwise, we have to worry
#' about surprises in different time zones). Even this has only a remote
#' relation to actual publication dates (volumes with no day or month of
#' publication are assigned to the 1st and to January; and in any case
#' periodical publication dates often have a complex relationship to the
#' realities of foot-dragging contributors, dilatory printers, etc.
#' 
#' @param pubdate a character vector of JSTOR pubdates
#' @return a vector of Dates
#' @export
#' 
pubdate_Date <- function (pubdate) {
    as.Date(substr(pubdate, 1, 10))
}

#' Convert a DfR ID into a JSTOR URL
#' 
#' For viewing a document on JSTOR. This works often, but not always.
#' 
#' @param id a document id (usually also the DOI)
#' @param jstor_direct if \code{TRUE} (default), try to guess a direct 
#'   \code{jstor.org/stable/} URL; otherwise, supply a \code{doi.org} URL. In 
#'   practice the direct link is much more likely to work than the DOI (go 
#'   figure).
#' @export
#' 
dfr_id_url <- function(id, jstor_direct=T) {
    ifelse(jstor_direct,
        paste("http://www.jstor.org", "/stable/", id, sep=""), 
        paste("http://doi.org", "/", id, sep="")
    )
}

#' Generate simple citation strings from metadata
#' 
#' Given a metadata frame, return a character vector of citations.
#' 
#' The generated citations are meant for quick reference, not formal use. They 
#' do not handle quotations within quotations correctly and make no effort to 
#' scrub the cruft found in some journals' metadata on JSTOR. Authors are simply
#' concatenated into a long "A and B and C" list.
#' 
#' @param metadata data frame (from e.g. \code{\link{read_dfr_metadata}}). Often
#'   you will want to row-subscript the metadata.
#' @return a character vector of citations
#'   
#' @examples
#' 
#' md <- data_frame(
#'     id="10.2307/432680",
#'     doi="10.2307/432680",
#'     title='Sidney\'s "Arcadia" and "The Tryall of Chevalry"',
#'     author=list('C. R. Baskervill'),
#'     journaltitle="Modern Philology",
#'     volume=10,
#'     issue=2,
#'     pubdate=as.Date("1912-10-01"),
#'     pagerange="pp. 197-201",
#'     publisher="The University of Chicago Press",
#'     type="fla",
#'     reviewed.work=NA,
#'     abstract=NA)
#' cite_articles(md)
#' 
#' \dontrun{
#' # Given a model m with stored document metadata, cite a document by id:
#' cite_articles(metadata(m)[doc_ids(m) == "10.2307/432680", ])
#' }
#' 
#' @export
#' 
cite_articles <- function (metadata)  {
    authors <- vapply(metadata$author,
        function (a) {
            if (a == "") {
                "[Anonymous]"
            } else {
                str_c(a, collapse=" and ")
            }
        },
        character(1))

    dates <- strftime(metadata$pubdate, "%B %Y")
    pp <- gsub("^p?p\\. ", "", metadata$pagerange)
    result <- str_c(
        authors, ', "', metadata$title, '," *',
        metadata$journaltitle, '* ', metadata$volume, ", no. ",
        metadata$issue, " (", dates, "): ", pp, ".",
        sep="")

    result <- gsub("_", ", ", result)
    result <- gsub("\t", "", result)
    result
}
