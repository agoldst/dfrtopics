# Functions for manipulating the full Gibbs sampling state.
#
# a.k.a. keeping the words in topic models (as Ben Schmidt says)

#' Save the Gibbs sampling state to a file
#'
#' Saves the MALLET sampling state using MALLET's own state-output routine,
#' which produces a ginormous gzipped textfile.
#'
#' @param m the \code{mallet_model} model object
#' @param outfile the output file name
#'
#' @seealso \code{\link{read_sampling_state}}
#'
#' @export
#'
write_mallet_state <- function(m, outfile="state.gz") {
    ptm <- ParallelTopicModel(m)
    if (is.null(ptm)) {
        stop("MALLET model object is not available.")
    }
    f <- rJava::.jnew("java/io/File", path.expand(outfile))
    rJava::.jcall(ptm, "V", "printState", f)
}

#' Reduce a MALLET sampling state on disk to a simplified form
#'
#' This function is principally for internal use. The main interface to the
#' Gibbs sampling state output from MALLET is \code{\link{load_sampling_state}}
#' (which calls this function when needed).  This function reads in the Gibbs
#' sampling state output by MALLET (a gzipped text file) and writes a CSV file
#' giving the number of times each word type in each document is assigned to
#' each document. Because the MALLET state file is often too big to handle in
#' memory all at once, the "simplification" is done by reading and writing in
#' chunks. This will not be as fast as it should be (arRgh!).
#'
#' The resulting file has a header \code{document,word,topic,count} describing
#' its columns.  Note that this file uses zero-based indices for topics, words,
#' and documents, not 1-based indices. It can be loaded with
#' \code{\link{read_sampling_state}}, but the recommended interface is
#' \code{\link{load_sampling_state}} (q.v.).
#'
#' This function formerly relied on a Python script, but in order to reduce
#' external dependencies it now uses R code only. However, R's gzip support is
#' somewhat flaky. If this function reports errors from \code{gzcon} or
#' \code{zlib} or similar, try manually decompressing the file and passing
#' \code{state_file=file("unzipped-state.txt")}.
#'
#' @param state_file the MALLET state file. Supply either a file name or a
#' connection
#'
#' @param outfile the name of the output file (will be clobbered)
#'
#' @param chunk_size number of lines to read at a time (sometimes multiple
#' chunks are written at once)
#'
#' @seealso \code{\link{load_sampling_state}}, \code{\link{sampling_state}},
#' \code{\link{read_sampling_state}}
#'
#' @export
#'
simplify_state <- function (state_file, outfile,
        chunk_size=getOption("dfrtopics.state_chunk_size")) {

    if (is.character(state_file)) {
        state_file <- gzcon(file(state_file, "rb"))
        on.exit(close(state_file))
    } else {
        if (!inherits(state_file, "connection")) {
            stop(
    'state_file should either be an input file name, like "state.gz",
    or a connection'
            )
        }
        if (!isOpen(state_file)) {
            open(state_file, "r")
            on.exit(close(state_file))
        }
    }

    # header
    writeLines("doc,type,topic,count", outfile)

    # if available, use readr::write_csv for faster write
    if (requireNamespace("readr", quietly=TRUE))
        write <- function (x) readr::write_csv(x, outfile, append=TRUE)
    else
        write <- function (x) write.table(x, outfile, sep=",",
            row.names=FALSE, col.names=FALSE, append=TRUE)

    # Sampling state is in document order. We can't write doc, type, topic
    # triples for a document until we're sure we're done getting rows for that
    # doc, so we'll always hold the last document in a chunk for writing when
    # we're sure we won't see any more rows for that doc

    acc <- data.frame(doc=integer(), type=integer(), topic=integer(),
        count=integer())
    n <- -1

    while (n != 0) {
        chunk <- read_gibbs(state_file, nrows=chunk_size)
        n <- nrow(chunk)
        if (n > 0) {
            last <- chunk$doc[n]
            # aggregate rows of token topic assignments into counts
            chunk <- dplyr::count_(chunk, c("doc", "type", "topic"))
            names(chunk)[4] <- "count"

            # join acc to chunk; group-summarize should only affect one group,
            # so this is wasteful but simple to write
            acc <- dplyr::bind_rows(acc, chunk)
            acc <- dplyr::group_by_(acc, ~ doc, ~ type, ~ topic)
            acc <- dplyr::summarize_(acc, count= ~ sum(count))

            # split off the last doc and keep it in acc for the next round
            to_write <- acc$doc != last
            write(acc[to_write, ])
            acc <- acc[!to_write, ]
        }
    }

    # write the final chunk
    write(acc)
}

# read MALLET sampling state rows from a .gz file. The exported
# read_sampling_state is for simplified state files, and the main interface is
# supposed to be load_sampling_state.
read_gibbs <- function (f, nrows=-1) {
    # it would be nice to use readr, but it doesn't like already-opened
    # gzcon(file()) streams
    #
    # result <- readr::read_delim(f, delim=" ", quote="", comment="#",
    #     col_names=c("doc", "type", "topic"), col_types="i__i_i", n_max=nrows)

    # neither does read.table, but readLines manages okay
    ll <- readLines(f, n=nrows)
    if (length(ll) > 0) {
        result <- read.table(text=ll, nrows=nrows,
            header=FALSE, sep="", quote="", row.names=NULL,
            col.names=c("doc", "src", "pos", "type", "word", "topic"),
            as.is=TRUE,
            colClasses=c("integer", "NULL", "NULL",
                         "integer", "NULL", "integer"),
            comment.char="#")
    } else {
        result <- data.frame()
    }

    result
}

#' Read in a Gibbs sampling state
#'
#' This function reads in a Gibbs sampling state represented by
#' \code{document,word,topic,count} rows to a
#' \code{\link[bigmemory]{big.matrix}}. This gives the model's assignments of
#' words to topics within documents. MALLET itself remembers token order, but
#' in ordinary LDA the words are assumed exchangeable within documents. The
#' recommended interface to this sampling state is
#' \code{\link{load_sampling_state}}, which calls this function.
#'
#' \emph{N.B.} The MALLET sampling state, and the "simplified state" output by
#' this function to disk, index documents, words, and topics from zero, but the
#' dataframe returned by this function indexes these from one, for convenience
#' within R.
#'
#' @return a \code{big.matrix} with four columns,
#'   \code{document,word,topic,count}. Documents, words, and topics are
#'   \emph{one-indexed} in the result, so these values may be used as indices to
#'   the vectors returned by \code{\link{doc_ids}}, \code{\link{vocabulary}},
#'   \code{\link{doc_topics}}, etc.
#'
#' @param filename the name of a CSV file holding the simplified state: a CSV
#'   with header row and four columns, \code{document,word,topic,count}, where
#'   the documents, words, and topics are \emph{zero-index}. Create the file
#'   from MALLET output using \code{\link{simplify_state}}.
#'
#' @param data_type the C++ type to store the data in. If all values have
#'   magnitude less than \eqn{2^15}, you can get away with \code{"short"}, but
#'   guess what? Linguistic data hates you, and a typical vocabulary can easily
#'   include more word types than that, so the default is \code{"integer"}.
#'
#' @param big_workdir the working directory where
#'   \code{\link[bigmemory]{read.big.matrix}} will store its temporary files. By
#'   default, uses \code{\link[base]{tempdir}}, but if you have more scratch
#'   space elsewhere, use that for handling large sampling states.
#'
#' @seealso \code{\link{load_mallet_state}},  \code{\link{write_mallet_state}},
#' \code{\link{tdm_topic}}, \code{\link{simplify_state}}, and package
#' \pkg{bigmemory}.
#'
#' @export
#'
read_sampling_state <- function(filename,
                                data_type="integer",
                                big_workdir=tempdir()) {
    if (!requireNamespace("bigmemory", quietly=TRUE)) {
        stop("The bigmemory package is needed to work with sampling states.")
    }
    message("Loading ", filename, " to a big.matrix...")
    state <- bigmemory::read.big.matrix(
        filename, type=data_type, header=TRUE, sep=",",
        backingpath=big_workdir,
        # use tempfile for guarantee that filename is unused
        backingfile=basename(tempfile("state", tmpdir=big_workdir, ".bin")),
        descriptorfile=basename(tempfile("state", tmpdir=big_workdir, ".desc"))
    )
    message("Done.")

    # change mallet's 0-based indices to 1-based
    state[ , 1] <- state[ , 1] + 1L     # docs
    state[ , 2] <- state[ , 2] + 1L     # types
    state[ , 3] <- state[ , 3] + 1L     # topics

    state
}

#' @export
sampling_state <- function (m) UseMethod("sampling_state")

#' @export
sampling_state.mallet_model <- function (m) {
    if (is.null(m$ss) && !is.null(m$model)) {
        message(
'To retrieve the sampling state, it must first be loaded:

m <- load_sampling_state(m)'
        )
    }
    m$ss
}

#' @export
`sampling_state<-` <- function (m, value) UseMethod("sampling_state<-")

#' @export
`sampling_state<-.mallet_model` <- function (m, value) {
    m$ss <- value
    m
}

#' Load Gibbs sampling state into model object
#'
#' Load the Gibbs sampling state into a model object for access via
#' \code{\link{sampling_state}}. The state must be available for loading,
#' \emph{either} from MALLET's own model object in memory, \emph{or} in a
#' sampling state file from MALLET, \emph{or} in a simplified sampling state
#' file. The latter two files (which can be very large) will be created if
#' necessary.
#'
#' @param m \code{mallet_model} object. Either its
#' \code{\link{ParallelTopicModel}} must be available or one of the other two
#' parameters must be an already-existing file
#'
#' @param simplified_state_file name of simplified sampling state file. If the
#' file exists, it is read in. If it does not exist, it is created; if this
#' parameter is NULL, a temporary file is used instead
#'
#' @param mallet_state_file name of file with MALLET's own gzipped
#' sampling-state output (from \code{\link{write_mallet_state}} or command-line
#' mallet). If this file does not exist, it will be created if necessary (i.e.
#' if \code{simplified_state_file} does not already exist); if this parameter
#' is NULL, a temporary is used.
#'
#' @return a copy of \code{m} with the sampling state loaded (available via
#' \code{sampling_state(m)}
#'
#' @export
#'
load_sampling_state <- function (m,
                                 simplified_state_file=NULL,
                                 mallet_state_file=NULL) {
    tmp_ss <- FALSE
    tmp_ms <- FALSE
    if (is.null(simplified_state_file)) {
        simplified_state_file <- tempfile()
        tmp_ss <- TRUE
    }

    if (is.null(mallet_state_file)) {
        mallet_state_file <- tempfile()
        tmp_ms <- TRUE
    }

    if (!file.exists(simplified_state_file)) {
        if (!file.exists(mallet_state_file)) {
            message("Writing MALLET state to ",
                ifelse(tmp_ms, "temporary file", mallet_state_file))
            write_mallet_state(m, mallet_state_file)
        }

        message("Writing simplified sampling state to ",
            ifelse(tmp_ss, "temporary file", simplified_state_file))

        simplify_state(mallet_state_file, simplified_state_file)
        if (tmp_ms)  {
            message("Removing temporary MALLET state file")
            unlink(mallet_state_file)
        }
    }

    sampling_state(m) <- read_sampling_state(simplified_state_file)

    if (tmp_ss) {
        message("Removing temporary simplified state file")
        unlink(simplified_state_file)
    }
    m
}

#' The term-document matrix for a topic
#'
#' Extracts a matrix of counts of words assigned to a given topic in each
#' document from the model's final Gibbs sampling state.
#'
#' This is useful for studying a topic conditional on some metadata covariate:
#' it is important to realize that frequent words in the overall topic
#' distribution may not be the same as very frequent words in that distribution
#' over some sub-group of documents, particularly if the corpus contains widely
#' varying language use. If, for example, the corpus stretches over a long time
#' period, consider comparing the early and late parts of each of the
#' within-topic term-document matrices.
#'
#' @return a \code{\link[Matrix]{sparseMatrix}} of \emph{within-topic} word
#'   weights (unsmoothed and unnormalized) with words in rows and documents in
#'   columns (same ordering as \code{vocabulary(m)} and \code{doc_ids(m)})
#'
#' @param m a \code{mallet_model} object with the sampling state loaded
#'   \code{\link{read_sampling_state}}. Operated on using
#'   \code{\link[bigmemory]{mwhich}}.
#'
#' @param topic topic (indexed from 1) to find the term-document weights for
#'
#' @seealso \code{\link{read_sampling_state}}, \code{\link{mallet_model}},
#'   \code{\link{load_sampling_state}}, \code{\link{top_n_row}},
#'   \code{\link{sum_col_groups}}
#'
#' @export
#'
tdm_topic <- function (m, topic) {
    ss <- sampling_state(m)
    if (is.null(ss)) {
        stop("The sampling state must be loaded. Use load_sampling_state().")
    }

    indices <- bigmemory::mwhich(ss, "topic", topic, "eq")

    Matrix::sparseMatrix(i=ss[indices, "type"],
                         j=ss[indices, "doc"],
                         x=ss[indices, "count"],
                         dims=c(length(vocabulary(m)),
                                n_docs(m)))
}

#' The topic-document matrix for a specific word
#'
#' Extracts a matrix of counts of a word's weight in each topic within each
#' document from the model's final Gibbs sampling state. (The matrix is quite
#' sparse.)
#'
#' This is useful for studying a word's distribution over topics conditional on
#' some metadata covariate. It is important to realize that the model does not
#' distribute the word among topics uniformly across the corpus.
#'
#' @return a \code{\link[Matrix]{sparseMatrix}} of \emph{within-document} word
#'   weights for \code{word} (columns are in \code{doc_ids(m)} order)
#'
#' @param m a \code{mallet_model} object with the sampling state loaded
#'   \code{\link{read_sampling_state}}. Operated on using
#'   \code{\link[bigmemory]{mwhich}}.
#'
#' @seealso \code{\link{tdm_topic}}, \code{\link{read_sampling_state}},
#'   \code{\link{mallet_model}}, \code{\link{load_sampling_state}},
#'   \code{\link{top_n_row}}, \code{\link{sum_col_groups}}
#'
#' @export
#'
#'
#' @export
topic_docs_word <- function (m, word) {
    ss <- sampling_state(m)
    if (is.null(ss)) {
        stop("The sampling state must be loaded. Use load_sampling_state().")
    }

    indices <- bigmemory::mwhich(ss, "type", word_ids(m, word), "eq")

    Matrix::sparseMatrix(i=ss[indices, "topic"],
                         j=ss[indices, "doc"],
                         x=ss[indices, "count"],
                         dims=c(n_topics(m),
                                n_docs(m)))
}
