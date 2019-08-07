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
#' \code{supporting_files=T}. 
#'
#' @section Metadata format:
#'
#' If you are working with non-JSTOR documents, the one file that will reflect
#' this is the exported metadata. dfr-browser expects seven metadata columns by default:
#' \code{id,title,author,journaltitle,volume,issue,pubdate,pagerange}. This
#' function looks for these seven columns and, if it finds them, writes the
#' metadata with these columns in this order. Any remaining columns are pushed
#' all the way to the right of the output. (dfr-browser ignores them unless you
#' customize it.) If any these columns is not present in \code{metadata(m)},
#' then \code{export_browser_data} will simply save all the metadata as is,
#' adjusting only the CSV format to match the baseline expectation of
#' dfr-browser (namely, a headerless CSV conforming to
#' \href{http://tools.ietf.org/html/rfc4180}{RFC 4180}.).
#'
#' If your metadata does not match these expectations, an alternative is to set
#' dfr-browser's configuration parameters \code{VIS.metadata.type} and
#' \code{VIS.bib.type} to "base" (using the \code{info} parameter) and to write
#' out a metadata file \emph{with} a header by passing \code{metadata_header=T}
#' to this function or \code{\link{dfr_browser}}. For polished results more
#' customization of dfr-browser might be necessary.
#'
#' Note that you can adjust the metadata held on the model object by assigning
#' to \code{metadata(m)} before exporting the browser data. In particular, if
#' you have many documents, you may wish to conserve space by eliminating
#' metadata columns that are not used by the visualization: for example,
#' \code{metadata(m)$publisher <- NULL}. Earlier versions of dfrtopics tried to
#' eliminate such columns automatically, but this more conservative approach
#' aims to allow you more flexibility about what gets exported.
#'
#' @section Deprecated option:
#'
#' To insert the data directly into the main \code{index.html} file, pass
#' \code{internalize=T}. This behavior is now deprecated and will be removed
#' in a future version.
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
#' @param internalize always set to FALSE. If TRUE, model data is in the
#' browser home page rather than separate files, but this behavior is
#' deprecated. See Details.
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
#' @param metadata_header if TRUE (FALSE is default), the exported metadata CSV
#' will have a header row (not expected by dfr-browser by default)
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
                                 metadata_header=FALSE) {
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
        data_dir <- file.path(out_dir, "data")
    } else {
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
        # TODO clean up
        #
        # This whole business with the metadata columns is a bad idea and needs
        # to be simplified both here and in dfr-brwser
        
        dfb_expected <- c(
            "id", "title", "author", "journaltitle", "volume", "issue",
            "pubdate", "pagerange")

        if (any(! dfb_expected %in% colnames(md_frame))) {
            message(
"Not all expected DfR metadata columns are present. All available metadata will
be written, without any column rearrangement. dfr-browser document display may
not work as expected without additional configuration. See ?export_browser_data
for details."
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
            index=index,
            header=metadata_header
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
#' @param header if TRUE, output header row (by default, dfr-browser does not
#' expect one)
#'
#' @seealso \code{\link{export_browser_data}} for a more automated export of
#' all model information at once
#' @export
#'
export_browser_metadata <- function (file, meta, zipped, overwrite, index,
                                     header=FALSE) {
    if (!is.null(index)) {
        file <- "meta.csv"
    }
    md_txt <- capture.output(
        write.table(meta,
                    quote=TRUE, sep=",",
                    col.names=header, row.names=FALSE,
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
#' Export model data and all supporting files needed to browse a model or
#' models interactively using
#' \href{http://agoldst.github.io/dfr-browser}{dfr-browser}, then (optionally)
#' open a web browser. It is also possible to browse a list of models.
#'
#' If \code{browse=T}, the function attempts to start a local webserver and
#' open a web browser pointing to the appropriate URL. This functionality
#' requires the \code{servr} package. 
#'
#' For more control over the export, including the option to export data files
#' only, if for example you have modified the HTML/CSS/JS of an existing
#' dfr-browser, use \code{\link{export_browser_data}} (q.v.).
#'
#' @section Browsing multiple models at once:
#'
#' dfr-browser can be configured to retrieve data from more than one model, or
#' to show topic proportions in a single model conditioned on more than one
#' metadata variable, or both. \code{dfr_browser} can generate the necessary
#' data and configuration files. Passing \code{dfr_browser} a list of model
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
#' @section Deprecated option:
#'
#' \code{internalize=TRUE} will cause all model data to be stored in the
#' index.html file, whence it can be loaded by dfr-browser. This behavior is no
#' longer useful; the model data should always be exported as separate files
#' for asynchronous loading by dfr-browser. (In former days one could avoid
#' having to launch a web server and simply point a browser at the index.html
#' file. dfr-browser no longer works in this circumstance because it relies on
#' a Web Worker, which modern web browsers will refuse to load from a
#' \code{file:///} URL.)
#'
#' @param m \code{mallet_model} object from \code{\link{train_model}} or
#' \code{\link{load_mallet_model}}, or a list of such models, or the results of
#' \code{\link{align_topics}}; see Details.
#' @param out_dir directory for output. By default, files are saved to a
#'   temporary directory.
#' @param browse if TRUE, launch web browser after export for viewing (requires
#' \pkg{servr} package).
#' @param internalize always set to FALSE. If TRUE, model data is in the
#' browser home page rather than separate files, but this behavior is
#' deprecated. See Details.
#' @param overwrite if TRUE (default), overwrite any pre-existing files.
#' @param condition dfr-browser displays topic proportions conditioned on (bins
#' of) a chosen metadata variable (by default, publication date). Any variable
#' in \code{metadata(m)} may be specified. A vector or list of such variables
#' may also be supplied (see Details). To adjust the binning of continuous or
#' time covariates, change the dfr-browser configuration via \code{info} or
#' \code{sub_info}.
#' @param info dfr-browser configuration object (see
#' \code{\link{export_browser_data}}).
#' @param sub_info list of lists of browser configurations (multiple-model
#' display only). The elements of this list should have names corresponding to
#' \code{ids}, and each of the elements of its elements should have names
#' corresponding to \code{condition}. If NULL, default values are used.
#' @param metadata_header if TRUE (FALSE is default), the exported metadata CSV
#' will have a header row (not expected by dfr-browser by default, but see the
#' example below)
#' @param ids character vector. If multiple models are specified in \code{m},
#' the corresponding element of \code{ids} is used as a model ID in
#' dfr-browser.
#' @param insert_alignment_table if \code{m} is a \code{topic_alignment} object
#' and this is TRUE, the "About" page of the generated browser will include a
#' table of the topics in each model with aligned topics in the same row.
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
#' # or simply:
#' dfr_browser(cl)
#'
#' # create a browser that allows you to toggle among 2 metadata variables
#' dfr_browser(m, condition=c("pubdate", "journaltitle"))
#'
#' # create a browser for metadata in a non-DfR format; note info specification
#'
#' dfr_browser(m, condition="something_else",
#'      metadata_header=T,
#'      info=list(VIS=list(
#'          metadata=list(type="base"),
#'          bib=list(type="base"),
#'          bib_view=list(major="all", minor="raw")
#'      )),
#' )
#' }
#'
#' @export
dfr_browser <- function (m, ...) UseMethod("dfr_browser")

#' @export
dfr_browser.mallet_model <- function (m, out_dir=tempfile("dfr-browser"),
                                      browse=TRUE,
                                      internalize=FALSE,
                                      overwrite=TRUE,
                                      condition="pubdate",
                                      info=NULL, ...) {

    if (length(condition) > 1) {
        # then it's a multi-model and we'll hand this off
        dfr_browser(list(m),
            out_dir=out_dir,
            condition=list(condition),
            browse=browse, ...
        )
        return()
    }

    if (is.null(info)) {
        info <- getOption("dfrtopics.browser_info")
    }

    if (is.null(info$VIS) || is.null(info$VIS$condition)) {
        info$VIS <- browser_condition_config(
            info=info$VIS,
            var=condition,
            data=metadata(m)[[condition]]
        )
    }

    export_browser_data(m, out_dir, supporting_files=TRUE,
        internalize=internalize, overwrite=overwrite,
        info=info, ...)

    if (browse)
        browse_dfb(out_dir)
}

# internal function for generating default dfr-browser configuration settings
# for a metadata variable. A modified copy of the info parameter is returned.
# For a single-model browser, this should be assigned to the VIS property of
# the overall contents of info.json; for a multi-model browser, it should be
# the contents of the model-specific info.json.
browser_condition_config <- function (info, var, data) {
    info <- info %n% list()

    info$metadata <- info$metadata %n% list()

    info$metadata$type <- info$metadata$type %n% "dfr"
    info$bib$type <- info$bib$type %n% "dfr"
    cond <- info$condition %n% list()
    cond$spec <- cond$spec %n% list()
    if (info$metadata$type == "dfr") {
        # deal with my own stupid choice to rename some default dfr field names
        # in dfr-browser but not here

        # TODO clean this horrible mess up
        if (var == "pubdate")
            var <- "date"

        if (var == "journaltitle")
            var <- "journal"
    }
    cond$spec$field <- var
    if (is.null(cond$type)) {
        if (is.numeric(data)) {
            cond$type <- "continuous"
        } else if (inherits(data, "Date")) {
            cond$type <- "time"
            info$metadata$spec <- list(date_field=var)
        } else { # default to ordinal
            cond$type <- "ordinal"
        }
    }
    if (is.null(cond$spec$n)) {
        if (cond$type == "continuous") {
            # default binning uses nclass.Sturges via hist
            cond$spec$n <- cond$spec$n %n% diff(
                hist(data, plot=FALSE)$breaks[1:2]
            )
        } else if (cond$type == "time") {
            # dates: single years by default
            cond$spec$n <- 1
            cond$spec$unit <- "year"
            cond$spec$format <- "%Y"
        }
    }

    info$condition <- cond
    info
}

#' @export
dfr_browser.list <- function (m, out_dir=tempfile("dfr-browser"),
                              browse=TRUE, 
                              condition="pubdate", 
                              overwrite=TRUE,
                              ids=paste0("model", seq_along(m)),
                              info=NULL,
                              sub_info=NULL,
                              metadata_header=FALSE,
                              ...) {

    if (!all(vapply(m, inherits, TRUE, "mallet_model"))) {
        stop(
"The first argument to dfr_browser() must be a list of mallet_model objects,
a topic_alignment object, or a single mallet_model object"
        )
    }

    if (is.null(info)) {
        info <- getOption("dfrtopics.browser_info")
    }
    info$models <- list()

    if (is.character(condition)) {
        condition <- rep(list(condition), length(m))
    }

    if (file.exists(out_dir)) {
        if (!file.info(out_dir)$isdir) {
            stop(paste(out_dir, "exists and is a file; expected a directory"))
        }
    } else {
        dir.create(out_dir)
    }

    export_dfb_supporting_files(out_dir, overwrite=overwrite)

    data_dir <- file.path(out_dir, "data")
    if (!dir.exists(data_dir)) {
        dir.create(data_dir)
    }

    # set up sub_info: empty by default
    sub_info <- sub_info %n% list()
    if (length(m) == 1 && identical(names(sub_info), condition)) {
        # OR if single model and sub_info has elements named for each of the 
        # conditions, create the necessary single extra layer of hierarchy
        sub_info <- list(ids=sub_info)
    } else if (length(condition) == 1 && identical(names(sub_info), ids)) {
        # ditto if single condition and sub_info has elements named for each
        # of the models
        for (i in seq_along(sub_info)) {
            sub_info[[i]] <- setNames(list(sub_info[[i]]), condition)
        }
    }

    for (i in seq_along(m)) {
        # if only one model but multiple conditions, we won't use separate
        # subdirectories for each configuration, just separate info-*.json files
        if (length(m) == 1) {
            subdir <- "data"
        } else {
            # otherwise, one subdirectory per model
            subdir <- file.path("data", ids[i])
        }

        # export model data files (but not info.json)
        export_browser_data(m[[i]], out_dir=file.path(out_dir, subdir),
            info=FALSE, internalize=FALSE, supporting_files=FALSE,
            overwrite=overwrite,
            metadata_header=metadata_header, ...)

        mods <- browser_model_files(subdir, condition[[i]], ids[i])
        # build up model list for overall data/info.json
        info$models <- c(info$models, mods)

        sub_info[[ids[i]]] <- sub_info[[ids[i]]] %n% list()

        # generate an info.json for each model + var combo
        for (j in seq_along(condition[[i]])) {
            var <- condition[[i]][j]
            subi <- sub_info[[ids[i]]][[var]] %n% list()
            subi <- browser_condition_config(
                info=subi,
                var=var,
                data=metadata(m[[i]])[[var]]
            )
            export_browser_info(
                file.path(out_dir, mods[[j]]$files$info),
                subi, index=NULL, overwrite=overwrite, ...)
        }
    }

    # export overall data/info.json
    export_browser_info(file.path(data_dir, "info.json"), info, index=NULL,
                        overwrite=overwrite)

    if (browse)
        browse_dfb(out_dir)
}

#' @export
#' 
dfr_browser.topic_alignment <- function (m, ids,
                                         condition="pubdate",
                                         info=NULL,
                                         sub_info=NULL,
                                         insert_alignment_table=TRUE,
                                         ...) {
    if (is.character(condition)) {
        condition <- rep(list(condition), length(m$clusters))
    }

    # default model ids are m1k40, m2k50, etc. where k is no. of topics
    if (missing(ids)) {
        ids <- paste0("m", seq_along(m$clusters), "k",
                      vapply(m$clusters, length, 1))
    }
    
    sub_info <- sub_info %n% list()

    names(m$clusters) <- ids
    for (i in ids) {
        sub_info[[i]] <- sub_info[[i]] %n% list()
        for (cond in condition) {
            sub_info[[i]][[cond]] <- sub_info[[i]][[cond]] %n% list()
            sub_info[[i]][[cond]]$topic_ids <- m$clusters[[i]]
        }
    }

    if (is.null(info)) {
        info <- getOption("dfrtopics.browser_info")
    }

    info$meta_info <- info$meta_info %n% ""
    if (insert_alignment_table) {
        info$meta_info <- paste(info$meta_info,
                                browser_alignment_table(m, ids, condition))
    }

    # hand off to dfr_browser.list
    dfr_browser(m$model_distances$ms, ids=ids,
                condition=condition,
                info=info,
                sub_info=sub_info, ...)
}

#' Generate an HTML table from a topic clustering
#'
#' This function is mainly meant for internal use but might conceivably help
#' with creating custom visualizations of multiple models.
#'
#' @param cl clustering from \code{\link{align_topics}}
#' @param ids character vector of model ids
#' @param condition vector or list of metadata covariates as in \code{\link{dfr_browser}}
#' @param n number of topic top words to display in table
#'
#' @return string with HTML table
#'
#' @export
#'
browser_alignment_table <- function (cl, ids, condition, n=4) {
    ms <- cl$model_distances$ms
    rows <- vector("list", length(cl$clusters))
    for (m in seq_along(ms)) {
        rows[[m]] <- top_words(ms[[m]], n=n) %>%
            dplyr::group_by(topic) %>%
            dplyr::summarize(label=paste(word, collapse=" ")) %>%
            dplyr::mutate(cluster=cl$clusters[[m]]) %>% # cluster == topic_id
            dplyr::mutate(label=paste(cluster, label)) %>%
            dplyr::mutate(model=
                if (length(condition[[m]]) > 1)
                    paste0(ids[m], "-", condition[[1]])
                else
                    ids[m]) %>%
            dplyr::mutate(label=paste0(
                "<a href=\"/#/", ids[m], "/topic/", cluster, "\">", label,
                "</a>"))
    }
    rows <- dplyr::bind_rows(rows) %>%
        dplyr::select(cluster, model, label) %>%
        tidyr::spread(model, label, fill="") %>%
        dplyr::select(-cluster) %>%
        dplyr::mutate_all(~ paste0("<td>", .x, "</td>")) %>%
        as.matrix()

    tbody <- rows %>% 
        apply(1, paste0, collapse="")
    tbody <- paste0("<tr>", tbody, "</tr>")
    tbody <- paste(tbody, collapse="\n")
    tbody <- paste0("<tbody>", tbody, "</tbody>")

    thead <- paste0("<th>", colnames(rows), "</th>")
    thead <- paste(thead, collapse="")
    thead <- paste0("<thead><tr>", thead, "</tr></thead>")

    paste0("<table class=\"table table-condensed\">",
           thead, tbody,
           "</table>")
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
browse_dfb <- function (out_dir) {
    if (requireNamespace("servr", quietly=TRUE)) {
        servr::httd(out_dir, browser=TRUE)
    } else {
        warning(
"dfr-browser requires a running web server. Try
    install.packages(\"servr\")
and re-run dfr_browser(), or start the web server of your choice from ",
            out_dir
        )
    }
}

# internal function for getting default filenames for dfb data files (used with 
# multimodel export)
browser_model_files <- function (out_dir, vars, id) {
    result <- vector("list", length(vars))
    univar <- length(vars) == 1
    for (i in seq_along(vars)) {
        files <- list(info=if (univar)
                               "info.json"
                           else
                               paste0("info-", vars[i], ".json"),
                      meta="meta.csv.zip",
                      dt="dt.json.zip",
                      tw="tw.json",
                      topic_scaled="topic_scaled.csv")
        files <- lapply(files, function (f) file.path(out_dir, f))
        result[[i]] <- list(
            id=if (univar) id else paste0(id, "-", vars[i]),
            files=files
        )
    }

    result
}


