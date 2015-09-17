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
#' @param m \code{mallet_model} object from \code{\link{train_model}} or
#'   \code{\link{load_mallet_model}}
#' @param out_dir directory for output. If \code{download_dfb} is TRUE, the
#'   exported data files will go in a \code{"data"} directory under
#'   \code{out_dir}.
#' @param zipped should the larger data files be zipped?
#' @param n_top_words how many top words per topic to save?
#' @param download_dfb if TRUE (FALSE is default), all the files needed to run
#'   the browser and the exported data placed appropriately. From a shell in
#'   \code{out_dir}, run \code{bin/server} to launch a local web server.
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
                                 download_dfb=FALSE,
                                 overwrite=FALSE) {
    if (!requireNamespace("jsonlite", quietly=TRUE)) {
        stop("jsonlite package required for browser export. Install from CRAN.")
    }

    if (file.exists(out_dir)) {
        if (!dir.exists(out_dir)) {
            stop(paste(out_dir, "exists and is a file; expected a directory"))
        }
    } else {
        dir.create(out_dir)
    }

    if (download_dfb) {
        if (!requireNamespace("httr", quietly=TRUE)) {
            stop("httr package required for download. Install from CRAN.")
        }
        message("Downloading dfr-browser files from github to ", out_dir)
        r <- httr::GET(
            "https://github.com/agoldst/dfr-browser/archive/master.zip")
        httr::stop_for_status(r)
        z <- tempfile()
        writeBin(httr::content(r, "raw"), z)
        dfb_keep <- c(
            "bin/", "css/", "fonts/", "js/", "lib/", "index.html", "LICENSE"
        )
        dfb_files <- unzip(z, list=TRUE)$Name
        dfb_files <- dfb_files[
            stringr::str_detect(dfb_files, paste(dfb_keep, collapse="|"))
        ]
        tmp <- tempfile()
        dir.create(tmp)
        unzip(z, files=dfb_files, exdir=tmp)
        unlink(z)

        dfb_files <- list.files(file.path(tmp, "dfr-browser-master"))

        if (!overwrite) {
            for (f in file.path(out_dir, list.files(dfb_files, recursive=T))) {
                if (file.exists(f)) {
                    stop(paste(f, "already exists.
Set overwrite=TRUE to overwrite existing files."
                    ))
                }
            }
        }

        file.copy(file.path(tmp, "dfr-browser-master", dfb_files),
                  out_dir,
                  recursive=TRUE, overwrite=overwrite)
        unlink(tmp, recursive=TRUE)

        # the server script doesn't download with execute permission, so:
        server <- file.path(out_dir, "bin", "server")
        Sys.chmod(server, mode=file.info(server)$mode | "0744")

        out_dir <- file.path(out_dir, "data")
        if (!file.exists(out_dir)) {
            dir.create(out_dir)
        }
    }

    keys <- top_words(m, n_top_words)
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

        tw_file <- file.path(out_dir, "tw.json")
        if (!overwrite) {
            stopifnot(!file.exists(tw_file))
        }
        writeLines(jsonlite::toJSON(tw, dataframe="columns"), tw_file)
        message("Wrote ", tw_file)
    } else {
        warning("Unable to write ", tw_file)
    }


    dtm <- Matrix::Matrix(doc_topics(m), sparse=T)
        # could compress much more aggressively considering that weights are
        # integers, so could be stored as binary data rather than ASCII

    dtm_json <- jsonlite::toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x))
    write_zip(function (f) { writeLines(dtm_json, f) },
              file.path(out_dir, "dt"), ".json", no_zip=!zipped,
              overwrite=overwrite)

    md_frame <- metadata(m)
    drops <- match(c("publisher", "reviewed.work", "doi"),
                   names(md_frame))
    md_frame <- md_frame[ , -drops]

    write_zip(function (f) {
        write.table(md_frame,f,
                    quote=T, sep=",",
                    col.names=F, row.names=F,
                    # d3.csv.* expects RFC 4180 compliance
                    qmethod="double")},
                file.path(out_dir, "meta"),
                ".csv", no_zip=!zipped,
                overwrite=overwrite)

    if (!is.null(topic_words(m))) {

        scaled_file <- file.path(out_dir, "topic_scaled.csv")
        if (!overwrite) {
            stopifnot(!file.exists(scaled_file))
        }
        write.table(topic_scaled_2d(m), scaled_file,
                    quote=F, sep=",", row.names=F, col.names=F)
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

