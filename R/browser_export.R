write_zip <- function (writer, file_base, file_ext=".json", no_zip=F) {
    if(no_zip) {
        f_out <- stringr::str_c(file_base, file_ext)
        writer(f_out)
    } else {
        f_temp <- file.path(tempdir(),
                            stringr::str_c(basename(file_base), file_ext))
        writer(f_temp)
        f_out <- stringr::str_c(file_base, file_ext, ".zip")
        if(file.exists(f_out)) {
            message("Removing existing ", f_out)
            unlink(f_out)
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
#' \url{http://agoldst.github.io/dfr-browser}{dfr-browser}, the web-browser
#' based model browser.
#' 
#' This routine reports on its progress. By default, it saves zipped versions of
#' the document-topics matrix and metadata files; dfr-browser supports
#' client-side unzipping. This function compresses files using R's
#' \code{\link{zip}} command. If that fails, set \code{zipped=F} (and, if you
#' wish, zip the files using another program).
#' 
#' @param m \code{dfr_lda} object from \code{\link{train_model}} or 
#'   \code{\link{load_dfr_lda}}
#' @param out_dir directory for output data files
#' @param zipped should the larger data files be zipped?
#' @param n_top_words how many top words per topic to save?
#'   
#' @examples
#' 
#' \dontrun{
#' m <- model_documents("citations.CSV", "wordcounts", "stoplist.txt",
#'                      n_topics=40)
#' export_browser_data(m, out_dir="data")
#' }
#' 
#' @seealso \code{\link{model_documents}} \code{\link{topic_scaled_2d}}
#' 
#' @export
export_browser_data <- function (m, out_dir="data", zipped=TRUE,
                                 n_top_words=50) {
    if (!requireNamespace("jsonlite", quietly=TRUE)) {
        stop("jsonlite package required for browser export. Install from CRAN.")
    }

    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    keys <- top_words(m, n_top_words)
    if (!is.null(keys)) {
        tw <- list(
            alpha=hyperparameters(m)$alpha,
            words=matrix(keys$word, nrow=n_topics(m), byrow=T),
            weights=matrix(keys$weight, nrow=n_topics(m), byrow=T)
        )

        tw_file <- file.path(out_dir, "tw.json")
        writeLines(jsonlite::toJSON(tw), tw_file)
        message("Wrote ", tw_file)
    } else {
        warning("Unable to write ", tw_file)
    }


    dtm <- Matrix::Matrix(doc_topics(m), sparse=T)
        # could compress much more aggressively considering that weights are 
        # integers, so could be stored as binary data rather than ASCII

    dtm_json <- jsonlite::toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x))
    write_zip(function (f) { writeLines(dtm_json, f) },
              file.path(out_dir, "dt"), ".json", no_zip=!zipped)

    md_frame <- metadata(m)
    drops <- match(c("publisher", "reviewed.work", "doi"),
                   names(md_frame))
    md_frame <- md_frame[ , -drops]
    md_frame$author <- vapply(md_frame$author,
                              stringr::str_c,
                              character(1),
                              collapse="\t")

    write_zip(function (f) {
        write.table(md_frame,f,
                    quote=T, sep=",",
                    col.names=F, row.names=F,
                    # d3.csv.* expects RFC 4180 compliance
                    qmethod="double")},
                file.path(out_dir, "meta"),
                ".csv", no_zip=!zipped)

    if (!is.null(topic_words(m))) {
        write.table(topic_scaled_2d(m), file.path(out_dir, "topic_scaled.csv"),
                    quote=F, sep=",", row.names=F, col.names=F)
    } else {
        warning(
"Topic-word matrix unavailable, so scaled coordinates have not been written."
        )
    }

    message("Checking for info.json file...")
    info_file <- file.path(out_dir, "info.json")
    if(file.exists(info_file)) {
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

