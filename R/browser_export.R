# internal function for saving individual files
# txt: source data as character vector
# f: target file name
# zip: zip? if so, f.zip is the output file
# overwrite: what it says
# index: if non-null, assumed to be index.html. txt is stored in a <script>
# element as a JSON-escaped string, and the element has ID m__DATA__f* where f* is basename(f) without the suffix (so: m__DATA__info, m__DATA__tw, etc.), as expected by dfr-browser's dfb.load_data routine

write_dfb_file <- function (txt, f, zip=TRUE,
                       overwrite=FALSE, index=NULL) {

    if (getOption("dfrtopics.verbose"))
        blurt <- message
    else
        blurt <- function (...) { }

    # internalize if index specified
    if (!is.null(index)) {
        if (!file.exists(index)) {
            stop("Couldn't read index.html for writing internalized data.")
        }
        if (!overwrite) {
            stop("For internalized data, overwrite must be TRUE.")
        }

        src <- readLines(index)
        ins <- which(src == "<!-- __DATA__ -->")
        if (length(ins) != 1) {
            stop("Couldn't locate insertion point in file")
        }
        # use jsonlite to escape the string, then stuff in script element
        txt <- paste0(
            '<script type="application/json" id="m__DATA__',
            gsub("\\..*$", "", gsub("/", "_", f)),
            '">',
            jsonlite::toJSON(paste(txt, collapse="\n"), auto_unbox=TRUE),
            '</script>'
        )
        # writing protocol: this element will be inserted right _before_
        # the __DATA__ comment
        writeLines(
            c(src[1:(ins - 1)], txt, src[ins:length(src)]),
            index)

        blurt("Rewrote ", index, " with ", basename(f), " data")

        return()
    }

    # otherwise, output separate file
    if (!zip) {
        if (file.exists(f) && !overwrite) {
            stop(f, " already exists and overwrite=FALSE")
        }

        writeLines(txt, f)
        f_out <- f
    } else {
        f_temp <- file.path(tempdir(), basename(f))
        writeLines(txt, f_temp)
        f_out <- paste0(f, ".zip")
        if (file.exists(f_out)) {
            if (overwrite) {
                blurt("Removing existing ", f_out)
                unlink(f_out)
            } else {
                stop(f_out, " already exists and overwrite=FALSE")
            }
        }
        if (getOption("dfrtopics.verbose"))
            flags <- "-9Xj"
        else
            flags <- "-9Xjq"

        status <- zip(f_out, f_temp, flags=flags)
        if (status != 0) {
            blurt("zip ", f_out, " failed")
        }
        unlink(f_temp)
    }

    if (file.exists(f_out)) {
        blurt("Saved ", f_out)
    } else {
        blurt("Unable to save ", f_out)
    }
}

#' Output data files for dfr-browser
#'
#' Transform and save modeling results in a format suitable for use by
#' \href{http://agoldst.github.io/dfr-browser}{dfr-browser}, the web-browser
#' based model browser. For a quick export and immediate viewing, see also
#' \code{\link{dfr_browser}}.
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
#' over the dfr-browser source (javascript, HTML, and CSS), pass
#' \code{supporting_files=T}. To insert the data directly into the main
#' \code{index.html} file, also passed \code{internalize=T}. I recommend this
#' last option for local viewing only, not for web hosting; in the latter case,
#' separate data files will allow for asynchronous loading.
#'
#' @section Metadata format:
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
#' @param m \code{mallet_model} object from \code{\link{train_model}} or
#'   \code{\link{load_mallet_model}}
#' @param out_dir directory for output. If \code{supporting_files} is TRUE, the
#'   exported data files will go in a \code{"data"} directory under
#'   \code{out_dir}.
#' @param zipped should the larger data files be zipped? (If TRUE, uses
#' \code{\link[utils]{zip}}, which requires the \code{zip} utility be
#' available.)
#' @param n_top_words how many top words per topic to save?
#' @param n_scaled_words how many word types to use in scaled coordinates
#'   calculation?
#' @param supporting_files if TRUE (FALSE is default), all the files
#'   needed to run the browser are copied to \code{out_dir}, with the
#'   exported data placed appropriately. From a shell in \code{out_dir},
#'   run \code{bin/server} to launch a local web server.
#' @param overwrite if TRUE, this will clobber existing files
#' @param internalize if TRUE, write data directly into the browser's
#' \code{index.html} source instead of into a series of separate files.
#' @param info a list of dfr-browser parameters. Converted to JSON with
#' \code{\link[jsonlite]{toJSON}} and stored in \code{info.json}. If omitted,
#' default values (\code{getOption("dfrtopics.browser_info")}) are used. No
#' file is written if \code{info=FALSE}.
#' @param proper if TRUE, the document-topic and topic-word matrices will be
#' smoothed by the hyperparameters alpha and beta (respectively) and normalized
#' before export, instead of the "raw" sampling weights (which is the default).
#' For MALLET models, moothed and normalized weights then give the maximum a
#' posteriori estimates of the corresponding probabilities, which is "properly"
#' what the modeling process yields (but may disguise the effects of variations
#' in document length---and increase the storage space required).
#' @param digits if \code{proper} is TRUE, probabilities are rounded to this
#' decimal place, yielding a somewhat sparser doc-topics matrix (the topic-word
#' matrix is more aggressively truncated anyway). Set to NULL for no rounding.
#' Rounded weights are renormalized within dfr-browser itself.
#' @param permute if non-NULL, specifies a renumbering of the topics: the new
#' topic \code{k} is old topic \code{permute[k]}. (If you have the inverse, use
#' \code{\link{order}(permute)} to invert it back.)
#'
#' @examples
#'
#' \dontrun{
#' m <- model_dfr_documents("citations.CSV", "wordcounts",
#'     "stoplist.txt", n_topics=40)
#'
#' # export all files needed for browser program
#' export_browser_data(m, out_dir="browser", supporting_files=T)
#'
#' # or: overwrite model data only for an already-existing browser
#' export_browser_data(m, out_dir="browser/data",
#'     supporting_files=F, overwrite=T)
#' }
#'
#' @seealso \code{\link{dfr_browser}}, \code{\link{model_dfr_documents}},
#' \code{\link{train_model}}, \code{\link{topic_scaled_2d}}, and the functions
#' for outputting individual custom files:
#' \code{\link{export_browser_topic_words}},
#' \code{\link{export_browser_doc_topics}},
#' \code{\link{export_browser_metadata}},
#' \code{\link{export_browser_topic_scaled}},
#' \code{\link{export_browser_info}}.
#'
#' @export
export_browser_data <- function (m, out_dir, zipped=TRUE,
                                 n_top_words=50,
                                 n_scaled_words=1000,
                                 supporting_files=FALSE,
                                 overwrite=FALSE,
                                 internalize=FALSE,
                                 info=NULL,
                                 proper=FALSE,
                                 digits=getOption("digits"),
                                 permute=NULL,
                                 data_dir) {
    if (!requireNamespace("jsonlite", quietly=TRUE)) {
        stop("jsonlite package required for browser export. Install from CRAN.")
    }

    if (internalize && !supporting_files) {
        stop("To export internalized data, supporting_files must be TRUE.")
    }

    if (getOption("dfrtopics.verbose"))
        blurt <- message
    else
        blurt <- function (...) { }


    if (internalize) {
        index <- file.path(out_dir, "index.html")
    } else {
        index <- NULL
    }

    if (file.exists(out_dir)) {
        if (!file.info(out_dir)$isdir) {
            stop(paste(out_dir, "exists and is a file; expected a directory"))
        }
    } else {
        dir.create(out_dir)
    }

    if (supporting_files) {
        export_dfb_supporting_files(out_dir, overwrite)
    }
    if (missing(data_dir)) {
        if (supporting_files) 
            data_dir <- file.path(out_dir, "data")
        else
            data_dir <- out_dir
    }

    if (!file.exists(data_dir)) {
        dir.create(data_dir)
    }

    # validate permute
    if (!is.null(permute) && !identical(sort(permute), 1:n_topics(m))) {
        warning("ignoring invalid permute parameter")
        permute <- NULL
    }

    if (proper) {
        keys <- top_words(m, n_top_words, tw_smooth_normalize(m))
        if (!is.null(keys) && is.numeric(digits)) {
            keys$weight <- round(keys$weight, digits)
        }
    } else {
        keys <- top_words(m, n_top_words)
    }
    if (!is.null(keys)) {
        export_browser_topic_words(
            file=paste0(file.path(data_dir, "tw"), ".json"),
            keys=keys,
            alpha=hyperparameters(m)$alpha,
            digits=digits,  # irrelevant unless proper is TRUE
            overwrite= overwrite || internalize,
            index=index,
            permute
        )
    } else {
        warning("Topic top words unavailable; unable to write tw.json")
    }


    if (!is.null(doc_topics(m))) {
        if (proper) {
            dtm <- dt_smooth_normalize(m)(doc_topics(m))
            if (is.numeric(digits)) {
                dtm <- Matrix::drop0(Matrix::zapsmall(dtm, digits))
            }
        } else {
            # could compress much more aggressively considering that weights are
            # integers, so could be stored as binary data rather than ASCII
            dtm <- Matrix::Matrix(doc_topics(m), sparse=TRUE)
        }

        export_browser_doc_topics(
            file=paste0(file.path(data_dir, "dt"), ".json"),
            dtm=dtm, 
            digits=digits,  # irrelevant unless proper is TRUE
            zip=zipped,
            overwrite=overwrite || internalize, index=index,
            permute
        ) 
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

        export_browser_metadata(
            file=paste0(file.path(data_dir, "meta"), ".csv"),
            meta=md_frame,
            zip=zipped,
            overwrite=overwrite || internalize,
            index=index
        )
    } else {
        warning(
"Metadata frame unavailable, so document metadata has not been written."
)
    }

    if (!is.null(topic_words(m))) {
        export_browser_topic_scaled( 
            file=paste0(file.path(data_dir, "topic_scaled"), ".csv"),
            scaled=topic_scaled_2d(m, n_scaled_words),
            overwrite=overwrite || internalize,
            index=index,
            permute
        )
    } else {
        warning(
"Topic-word matrix unavailable, so scaled coordinates have not been written."
        )
    }

    info_file <- paste0(file.path(data_dir, "info"), ".json")
    write_info <- !identical(info, FALSE)
    if (write_info && !internalize && is.null(info)) {
        blurt("Checking for info.json file...")
        write_info <- !file.exists(info_file)
        if (!write_info) blurt(info_file, " ok")
    }
    if (write_info) {
        if (is.null(info)) {
            # default stub info
            info <- getOption("dfrtopics.browser_info")
        }
        export_browser_info(
            info_file,
            info,
            overwrite=overwrite || internalize,
            index=index
        )
    }
}

export_dfb_supporting_files <- function (out_dir, overwrite) {
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

    if (getOption("dfrtopics.verbose"))
        message("Copying dfr-browser supporting files...")

    file.copy(file.path(path.package("dfrtopics"), "dfb", dfb_files),
              out_dir,
              recursive=TRUE, overwrite=overwrite)

    # the server script doesn't download with execute permission, so:
    server <- file.path(out_dir, "bin", "server")
    Sys.chmod(server, mode=file.info(server)$mode | "0744")
}

#' Export topic-word file for dfr-browser
#'
#' Exports the \code{tw.json} representation of the topic-word matrix as used
#' by dfr-browser. Use this to customize the output (e.g. by changing the
#' weighting of words in topics).
#'
#' @param file output file name
#' @param keys data frame with \code{topic}, \code{word}, \code{weight} columns
#' and \eqn{n} rows for each topic (as in the result of \code{\link{top_words}}
#' @param alpha vector of topic hyperparameters \eqn{a}. The number of topics
#' \eqn{K} is guessed from the length of this vector, so even if the alpha
#' parameter is not meaningful, supply a vector of zeroes \emph{of length equal
#' to} \eqn{K}.
#' @param digits numerical rounding (passed on to
#' \code{\link[jsonlite]{toJSON}})
#' @param overwrite clobber existing file?
#' @param index if non-NULL, output is assumed to go into an element with ID
#' \code{m__DATA__tw} in an HTML file at this path. \code{file} is ignored.
#' @param permute if non-NULL, exported topic \code{k} will correspond to the
#' topic numbered \code{permute[k]} in the data
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#' 
export_browser_topic_words <- function (file, keys, alpha, digits=4,
                                        overwrite, index,
                                        permute) {
    if (!is.null(permute)) {
        keys$topic <- match(keys$topic, permute)
        alpha <- alpha[permute]
    }
    keys <- dplyr::arrange_(keys, ~ topic, ~ desc(weight))
    n_top_words <- nrow(keys) / length(alpha)
    if (!is.null(index)) {
        file <- "tw.json"
    }
    tw <- list(
        alpha=alpha,
        tw=lapply(seq_along(alpha),
            function (i) {
                setNames(
                    keys[(i - 1) * n_top_words + 1:n_top_words,
                         c("word", "weight")],
                    c("words", "weights")
                )
            }
        )
    )

    write_dfb_file(jsonlite::toJSON(tw, dataframe="columns", digits=digits),
        file, zip=FALSE, overwrite=overwrite, index=index
    )
}

#' Export document-topic file for dfr-browser
#'
#' Exports the \code{dt.json[.zip]} representation of the document-topic matrix
#' as used by dfr-browser. Use this function to customize the output (e.g. by
#' changing the weighting).
#'
#' @param file output file name
#' @param dtm document-topic matrix (will be coerced to sparse if necessary)
#' @param digits numerical rounding (passed on to \code{\link[jsonlite]{toJSON}})
#' @param zipped compress output?
#' @param overwrite clobber existing file?
#' @param index if non-NULL, output is assumed to go into an element with ID
#' \code{m__DATA__dt} in an HTML file at this path. \code{file} is ignored.
#' @param permute if non-NULL, exported topic \code{k} will correspond to the
#' topic numbered \code{permute[k]} in the data
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#'
export_browser_doc_topics <- function (file, dtm, digits=4,
                                       zipped, overwrite, index, permute) { 
    dtm <- as(dtm, "CsparseMatrix")
    if (!is.null(permute)) {
        dtm <- dtm[ , permute]
    }
    if (!is.null(index)) {
        file <- "dt.json"
    }
    write_dfb_file(jsonlite::toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x),
                                    digits=digits),
        file, zip=zipped, overwrite=overwrite, index=index
    )
}

#' Export metadata file for dfr-browser
#'
#' Exports the \code{meta.csv[.zip]} metadata file for dfr-browser. This is a
#' convenience function in case you wish to re-export metadata without changing
#' anything else. Equivalent results can be achieved by modifying the metadata
#' on the model object before calling \code{\link{export_browser_data}}.
#'
#' This file simply writes the supplied \code{meta} frame as a headerless CSV
#' according to the conventions expected by the default settings in
#' dfr-browser.
#'
#' @param file output file name
#' @param meta data frame with metadata, one row per document
#' @param zipped compress output?
#' @param overwrite clobber existing file?
#' @param index if non-NULL, output is assumed to go into an element with ID
#' \code{m__DATA__meta} in an HTML file at this path. \code{file} is ignored.
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#'
export_browser_metadata <- function (file, meta, zipped, overwrite, index) {
    if (!is.null(index)) {
        file <- "meta.csv"
    }
    md_txt <- capture.output(
        write.table(meta,
                    quote=TRUE, sep=",",
                    col.names=FALSE, row.names=FALSE,
                    # d3.csv.* expects RFC 4180 compliance
                    qmethod="double")
    )
    write_dfb_file(md_txt,
        file, zip=zipped, overwrite=overwrite, index=index)
}

#' Export scaled topic coordinates for dfr-browser
#'
#' Exports the \code{topic_scaled.csv} file for dfr-browser's "Scaled" view of
#' a topic model. Use this function to export different coordinates than those
#' generated by default in \code{\link{export_browser_data}} (say, with a
#' different dimension-reduction method, like t-SNE).
#'
#' @param file output file name
#' @param scaled two-column coordinate matrix, one row per topic
#' @param overwrite clobber existing file?
#' @param index if non-NULL, output is assumed to go into an element with ID
#' \code{m__DATA__topic_scaled} in an HTML file at this path. \code{file} is
#' ignored.
#' @param permute if non-NULL, exported topic \code{k} will correspond to the
#' topic numbered \code{permute[k]} in the data
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#'
export_browser_topic_scaled <- function (file, scaled, overwrite, index,
                                         permute) {
    if (!is.null(index)) {
        file <- "topic_scaled.csv"
    }
    if (!is.null(permute)) {
        scaled <- scaled[permute]
    }
    write_dfb_file(capture.output(
        write.table(scaled, quote=FALSE, sep=",", row.names=FALSE,
                    col.names=FALSE)
        ),
        file, zip=FALSE, overwrite=overwrite, index=index
    )
}

#' Export browser information/configuration for dfr-browser
#'
#' Exports the \code{info.json} file for dfr-browser. This is a convenience
#' function in case you do not wish to re-export all other browser data files.
#' An equivalent result can be achieved by passing the desired configuration as
#' the \code{info} parameter to \code{\link{export_browser_data}}.
#'
#' @param file output file name
#' @param info list, to be converted to JSON for export.
#' @param overwrite clobber existing file?
#' @param index if non-NULL, output is assumed to go into an element with ID
#' \code{m__DATA__info} in an HTML file at this path. \code{file} is
#' ignored.
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#'
export_browser_info <- function (file, info, overwrite, index) {
    if (!is.null(index)) {
        file <- "info.json"
    }

    info <- jsonlite::toJSON(info, auto_unbox=TRUE, pretty=4)

    write_dfb_file(info, file, zip=FALSE,
        overwrite=overwrite, index=index)
}

#' Create and launch a model browser
#'
#' Export model data and all supporting files needed to browse a model
#' or models interactively using \href{http://agoldst.github.io/dfr-browser}{dfr-browser},
#' then (optionally) open a web browser. It is also possible to browse a list of models.
#'
#' There are two ways to store the model data in the exported files. Either the
#' data can be part of the source for the web page (\code{internalize=TRUE}) or
#' it can be filed in separate files (\code{internalize=FALSE}).  For web
#' hosting, the latter is better, because dfr-browser can load data
#' asynchronously rather than all at once, resulting in a more responsive
#' initial page view for web visitors.  The former, "internalized" option is
#' intended to be more convenient for local browsing, since a web browser can
#' simply be pointed to the file on disk (this is what \code{browse=TRUE} does).
#' However, this method may not always work, depending on your system's
#' implementation of \code{\link[utils]{browseURL}} and your web browser. Thus,
#' RStudio appears to launch a web server to serve files given by a
#' \code{file://} URL. This allows for browsing regardless of
#' \code{internalize}. By contrast, opening an \code{internalize}d dfr-browser's
#' \code{index.html} file directly currently works in Firefox but not Chrome
#' (which refuses to load the associated Web Worker from disk).
#'
#' For more control over the export, including the option to export data files
#' only, if for example you have modified the HTML/CSS/JS of an existing
#' dfr-browser, use \code{\link{export_browser_data}}.
#'
#' @section Browsing multiple models at once:
#'
#' dfr-browser can be configured to retrieve data from more than one model, or
#' to show topic proportions in a single model conditioned on more than one
#' metadata variable, or both. \code{dfr_browser} can generate the necessary
#' data and configuration files, though \code{internalize=TRUE} is currently
#' not supported in this case. Passing \code{dfr_browser} a list of model
#' objects will generate a single dfr-browser with a menu for swapping among
#' the models. Passing a single \code{mallet_model} and a vector of variable
#' names as \code{condition} will generate a dfr-browser with a menu for
#' swapping among covariates (internally, dfr-browser treats these two
#' possibilities the same). Passing both a list of models and a vector of
#' variable names will generate all the model-covariate pairs. Passing a list
#' of models and a list of the same length of variable names will use the
#' corresponding element of \code{condition} as the chosen covariate options
#' for the given model. Finally, in place of a list of models, you can supply
#' the result of \code{\link{align_topics}}. In that case the topics in each
#' model are relabeled according to their assigned cluster number. It is
#' possible to achieve the same result more manually by passing a list of
#' models and specifying a \code{topic_ids} vector for each model in the
#' \code{sub_info} parameter.
#'
#' @param m \code{mallet_model} object from \code{\link{train_model}} or
#'   \code{\link{load_mallet_model}}
#' @param out_dir directory for output. By default, files are saved to a
#'   temporary directory
#' @param browse if TRUE, launch web browser after export for viewing
#' @param internalize if TRUE, model data is in the browser home page rather
#'   than separate files. See Details.
#' @param condition dfr-browser displays topic proportions conditioned on (bins
#' of) a chosen metadata variable (by default, publication date). Any variable
#' in \code{metadata(m)} may be specified. A vector or list of such variables
#' may also be supplied (see Details). To adjust the binning of continuous or
#' time covariates, change the dfr-browser configuration via \code{info} or
#' \code{sub_info}.
#' @param ids character vector. If multiple models are specified in \code{m},
#' the corresponding element of \code{ids} is used as a model ID in
#' dfr-browser.
#' @param ... passed on to \code{\link{export_browser_data}}, q.v., especially
#' the parameters \code{overwrite}, \code{n_scaled_words}, \code{info}, 
#' \code{proper}, and \code{permute}
#'
#' @seealso \code{\link{export_browser_data}} which does the work of exporting
#'   files, \code{\link{model_dfr_documents}}, \code{\link{train_model}},
#'   \code{\link{topic_scaled_2d}}
#'
#' @examples
#'
#' \dontrun{
#' m <- model_dfr_documents("citations.CSV", "wordcounts",
#'     "stoplist.txt", n_topics=40)
#' # launch browser
#' dfr_browser(m)
#'
#' # generate a second model and align its topics with the first for more
#' # convenient comparisons
#' m2 <- model_dfr_documents("citations.CSV", "wordcounts",
#'     "stoplist.txt", n_topics=40)
#' cl <- model_distances(list(m, m2), n_words=40) %>% align_topics()
#' dfr_browser(m2, permute=match(cl$clusters[[1]], cl$clusters[[2]])))
#' 
#' }
#'
#' @export
dfr_browser <- function (m, ...) UseMethod("dfr_browser")

#' @export
dfr_browser.mallet_model <- function (m, out_dir=tempfile("dfr-browser"),
        internalize=TRUE, browse=TRUE, condition="pubdate", ...) {

    if (length(condition) > 1) {
        # then it's a multi-model and we'll hand this off
        dfr_browser(list(m),
            out_dir=out_dir, ids=condition,
            condition=list(condition),
            internalize=internalize,
            browse=browse, ...
        )
        return()
    }

    export_browser_data(m, out_dir, supporting_files=TRUE,
        internalize=internalize, ...)

    if (browse)
        browse_dfb(out_dir)
}

#' @export
dfr_browser.list <- function (m, out_dir, ids=seq_along(m),
                              condition="pubdate", sub_info=NULL,
                              browse=TRUE, info=NULL, overwrite=TRUE, ...) {

    if (!all(vapply(inherits, m, TRUE, "mallet_model"))) {
        stop(
"The first argument to dfr_browser() must be a list of mallet_model objects,
a topic_alignment object, or a single mallet_model object"
        )
    }

    if (is.null(info)) {
        info <- getOption("dfrtopics.browser_info")
        info$VIS <- NULL
    }
    info$models <- list()

    if (is.character(condition)) {
        condition <- rep(list(condition), length(m))
    }
    if (is.null(sub_info)) {
        sub_info <- rep(vector("list", length(condition)), length(m))
    }

    export_dfb_supporting_files(out_dir, overwrite)
    for (i in seq_along(m)) {
        subdir <- file.path(out_dir, "data", ids[i])

        # export model data files (but not info.json)
        export_browser_data(m, out_dir=subdir,
            info=FALSE, internalize=FALSE, supporting_files=FALSE,
            metadata_header=TRUE, ...)

        # we generate an info.json for each model + var combo
        mods <- browser_model_files(subdir, condition[[i]], ids[i])
        info$models <- c(info$models, mods)

        for (j in seq_along(condition[[i]])) {
            var <- condition[[i]][j]
            subi <- sub_info[[ids[i]]][[var]]
            subi$metadata <- subi$metadata %n% list()

            subi$metadata$type <- "base"
            cond <- subi$condition %n% list()
            cond$spec <- cond$spec %n% list()
            cond$spec$field <- var
            mv <- metadata(m[[i]])[[var]]
            if (is.null(cond$type)) {
                if (is.numeric(mv)) {
                    cond$type <- "continuous"
                } else if (inherits(mv, "Date")) {
                    cond$type <- "date"
                } else { # default to ordinal
                    cond$type <- "ordinal"
                }
            }
            if (is.null(cond$spec$step)) {
                if (cond$type == "continuous") {
                    # default binning uses nclass.Sturges via hist
                    cond$spec$step <- cond$spec$step %n% diff(
                        hist(mv, plot=FALSE)$breaks[1:2]
                    )
                } else if (cond$type == "date") {
                    cond$spec$step <- 1
                    cond$spec$unit <- "year"
                    cond$spec$format <- "%Y"
                }
            }

            subi$condition <- cond
            export_browser_info(mods[[j]]$files$info, subi, ...)
        }
    }

    export_browser_info(file.path(out_dir, "data", "info.json"), info)

    if (browse)
        browse_dfb(out_dir)
}

#' @export
#' 
dfr_browser.topic_alignment <- function (m, out_dir, ids,
                                         condition="pubdate", sub_info=NULL, ...) {
    if (is.character(condition)) {
        condition <- rep(list(condition), length(m$clusters))
    }

    if (is.null(sub_info)) {
        sub_info <- rep(vector("list", length(condition)), length(m$clusters))
    }

    # default model ids are m1k40, m2k50, etc. where k is no. of topics
    if (missing(ids)) {
        ids <- paste0("m", seq_along(m$clusters), "k",
                      vapply(m$clusters, length, 1))
    }
    
    for (i in seq_along(m$clusters)) {
        sub_info[[i]] <- lapply(sub_info[[i]], function (s) {
            s$topic_ids <- m$clusters[[i]]
            s
        })
    }

    dfr_browser(m$dst$ms, out_dir, ids, condition, sub_info, ...)
}

#' @export
#'
suggest_date_interval <- function (xs, breaks=nclass.Sturges(xs)) {
    lv <- sort(levels(cut.Date(xs, breaks)))[1:2]
    stp <- round(difftime(lv[2], lv[1]))
    list(step=as.double(stp),
         unit=units(stp))
}

# internal function for launching a browser
browse_dfb <- function (out_dir) browseURL(
    paste0("file://", file.path(normalizePath(out_dir), "index.html"))
)

# internal function for getting default filenames for dfb data files (used with 
# multimodel export)
browser_model_files <- function (out_dir, vars, id) {
    result <- vector("list", length(vars))
    for (i in seq_along(vars)) {
        files <- list(info=paste0("info-", vars[i], ".json"),
                      meta="meta.csv.zip",
                      dt="dt.json.zip",
                      tw="tw.json",
                      topic_scaled="topic_scaled.csv")
        files <- lapply(files, function (f) file.path(out_dir, f))
        result[[i]] <- list(
            id=paste0(id, "-", vars[i]),
            files=files
        )
    }
    # but simplify to plain old info.json if there's only one variable
    if (length(result) == 1) {
        result[[1]]$files$info <- file.path(out_dir, "info.json")
    }

    result
}
