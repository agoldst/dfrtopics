write_zip <- function (writer, file_base, file_ext=".json", no_zip=FALSE,
                       overwrite=FALSE) {
    if (no_zip) {
        f_out <- stringr::str_c(file_base, file_ext)
        writer(f_out)
    } else {
        f_temp <- file.path(tempdir(),
                            stringr::str_c(basename(file_base), file_ext))
        writer(f_temp)
        f_out <- stringr::str_c(file_base, file_ext, ".zip")
        if (file.exists(f_out)) {
            if (overwrite) {
                message("Removing existing ", f_out)
                unlink(f_out)
            } else {
                stop(f_out, " already exists and overwrite=FALSE")
            }
        }
        status <- zip(f_out, f_temp, flags="-9Xj")
        if (status != 0) {
            message("zip ", f_out, " failed")
        }
        unlink(f_temp)
    }

    if (file.exists(f_out)) {
        message("Saved ", f_out)
    } else {
        message("Unable to save ", f_out)
    }
}

#' Output data files for dfr-browser
#' 
#' Transform and save modeling results in a format suitable for use by 
#' \href{http://agoldst.github.io/dfr-browser}{dfr-browser}, the web-browser 
#' based model browser.
#' 
#' This routine reports on its progress. By default, it saves zipped versions of
#' the document-topics matrix and metadata files; dfr-browser supports 
#' client-side unzipping. This function compresses files using R's 
#' \code{\link{zip}} command. If that fails, set \code{zipped=F} (and, if you 
#' wish, zip the files using another program).
#' 
#' A detailed description of the output files can be found in the dfr-browser
#' technical notes at \url{http://github.com/agoldst/dfr-browser}.
#'
#' This package includes a copy of the dfr-browser files necessary to run the 
#' browser. By default, this routine only exports data files. To also copy 
#" over the dfr-browser source (javascript, HTML, and CSS), pass 
#' \code{supporting_files=T}.
#' 
#' If you are working with non-JSTOR documents, the one file that will reflect
#' this is the exported metadata. dfr-browser expects seven metadata columns:
#' \code{id,title,author,journaltitle,volume,issue,pubdate,pagerange}. This
#' function looks for these seven columns and, if it finds them, writes the
#' metadata with these columns in this order. Any remaining columns are pushed
#' all the way to the right of the output. (dfr-browser ignores them unless you
#' customize it.) If any these columns is not present in \code{metadata(m)},
#' then \code{export_browser_data} will simply save all the metadata as is,
#' adjusting only the CSV format to match the baseline expectation of
#' dfr-browser (namely, a headerless CSV conforming to
#' \href{http://tools.ietf.org/html/rfc4180}{RFC 4180}.). If your metadata does
#' not match these expectations, you may have to write out the metadata yourself
#' and/or customize dfr-browser to get satisfactory results.
#' 
#' Note that you can adjust the metadata held on the model object by assigning
#' to \code{metadata(m)} before exporting the browser data. In particular, if
#' you have many documents, you may wish to conserve space by eliminating
#' metadata columns that are not used by the visualization: for example,
#' \code{metadata(m)$publisher <- NULL}. Earlier versions of dfrtopics tried to
#' eliminate such columns automatically, but this more conservative approach
#' aims to allow you more flexibility about what gets exported.
#' 
#' 
#' @param m \code{mallet_model} object from \code{\link{train_model}} or 
#'   \code{\link{load_mallet_model}}
#' @param out_dir directory for output. If \code{supporting_files} is TRUE, the 
#'   exported data files will go in a \code{"data"} directory under 
#'   \code{out_dir}.
#' @param zipped should the larger data files be zipped?
#' @param n_top_words how many top words per topic to save?
#' @param n_scaled_words how many word types to use in scaled coordinates 
#'   calculation?
#' @param supporting_files if TRUE (FALSE is default), all the files
#'   needed to run the browser are copied to \code{out_dir}, with the
#'   exported data placed appropriately. From a shell in \code{out_dir},
#'   run \code{bin/server} to launch a local web server.  
#' @param overwrite if TRUE, this will clobber existing files
#'   
#' @examples
#' 
#' \dontrun{
#' m <- model_dfr_documents("citations.CSV", "wordcounts",
#'     "stoplist.txt", n_topics=40)
#' export_browser_data(m, out_dir="data")
#' }
#' 
#' @seealso \code{\link{model_dfr_documents}}, \code{\link{train_model}}, 
#'   \code{\link{topic_scaled_2d}}
#'   
#' @export
export_browser_data <- function (m, out_dir, zipped=TRUE,
                                 n_top_words=50,
                                 n_scaled_words=1000,
                                 supporting_files=FALSE,
                                 overwrite=FALSE) {
    if (!requireNamespace("jsonlite", quietly=TRUE)) {
        stop("jsonlite package required for browser export. Install from CRAN.")
    }

    if (file.exists(out_dir)) {
        if (!file.info(out_dir)$isdir) {
            stop(paste(out_dir, "exists and is a file; expected a directory"))
        }
    } else {
        dir.create(out_dir)
    }

    if (supporting_files) { 
        dfb_files <- list.files(file.path(path.package("dfrtopics"), "dfb"))

        if (!overwrite) {
            for (f in file.path(out_dir, list.files(dfb_files,
                                                    recursive=TRUE))) {
                if (file.exists(f)) {
                    stop(paste(f, "already exists.
Set overwrite=TRUE to overwrite existing files."
                    ))
                }
            }
        }

        message("Copying dfr-browser supporting files...")
        file.copy(file.path(path.package("dfrtopics"), "dfb", dfb_files),
                  out_dir,
                  recursive=TRUE, overwrite=overwrite)

        # the server script doesn't download with execute permission, so:
        server <- file.path(out_dir, "bin", "server")
        Sys.chmod(server, mode=file.info(server)$mode | "0744")

        out_dir <- file.path(out_dir, "data")
        if (!file.exists(out_dir)) {
            dir.create(out_dir)
        }
    }

    keys <- top_words(m, n_top_words)
    tw_file <- file.path(out_dir, "tw.json")
    if (!is.null(keys)) {
        tw <- list(
            alpha=hyperparameters(m)$alpha,
            tw=lapply(1:n_topics(m),
                function (i) {
                    setNames(
                        keys[(i - 1) * n_top_words + 1:n_top_words,
                             c("word", "weight")],
                        c("words", "weights")
                    )
                }
            )
        )

        if (!overwrite) {
            stopifnot(!file.exists(tw_file))
        }
        writeLines(jsonlite::toJSON(tw, dataframe="columns"), tw_file)
        message("Wrote ", tw_file)
    } else {
        warning("Topic top words unavailable; unable to write tw.json")
    }


    if (!is.null(doc_topics(m))) {
        dtm <- Matrix::Matrix(doc_topics(m), sparse=TRUE)
            # could compress much more aggressively considering that weights are
            # integers, so could be stored as binary data rather than ASCII

        dtm_json <- jsonlite::toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x))
        write_zip(function (f) { writeLines(dtm_json, f) },
                  file.path(out_dir, "dt"), ".json", no_zip=!zipped,
                  overwrite=overwrite)
    } else {
        warning("Document topics unavailable; unable to write dt.json.zip")
    }

    md_frame <- metadata(m)
    if (!is.null(md_frame)) {
        dfb_expected <- c(
            "id", "title", "author", "journaltitle", "volume", "issue",
            "pubdate", "pagerange")

        if (any(! dfb_expected %in% colnames(md_frame))) {
            warning(
"Not all expected metadata columns are present. All available metadata
will be written, without any column rearrangement. dfr-browser document
display may not work as expected. See ?export_browser_data for details."
)
        } else {
            # reorder columns
            rest_cols <- setdiff(colnames(md_frame), dfb_expected)
            md_frame <- md_frame[ , c(dfb_expected, rest_cols)]
        }

        write_zip(function (f) {
            write.table(md_frame,f,
                        quote=TRUE, sep=",",
                        col.names=FALSE, row.names=FALSE,
                        # d3.csv.* expects RFC 4180 compliance
                        qmethod="double")},
                    file.path(out_dir, "meta"),
                    ".csv", no_zip=!zipped,
                    overwrite=overwrite)
    } else {
        warning(
"Metadata frame unavailable, so document metadata has not been written."
)
    }

    if (!is.null(topic_words(m))) {

        scaled_file <- file.path(out_dir, "topic_scaled.csv")
        if (!overwrite) {
            stopifnot(!file.exists(scaled_file))
        }
        write.table(topic_scaled_2d(m, n_scaled_words), scaled_file,
                    quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
    } else {
        warning(
"Topic-word matrix unavailable, so scaled coordinates have not been written."
        )
    }

    message("Checking for info.json file...")
    info_file <- file.path(out_dir, "info.json")
    if (file.exists(info_file)) {
        message(info_file, " ok")
    }
    else {
        writeLines(
'{
    "title": "",
    "meta_info": "<h2><\\/h2>",
    "VIS": { "overview_words": 15 }
}'
            , info_file)
        message(info_file, " was missing. A stub file has been created.")
    }
}

