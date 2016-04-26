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
#' This function reads in the sampling state output by MALLET and writes a CSV
#' file giving the assignments of word types to topics in each document. Because
#' the MALLET state file is often too big to handle in memory using R, the
#' "simplification" is done using a very simple python script instead.
#'
#' The resulting file has a header \code{document,word,topic,count} describing
#' its columns. Use \code{\link{read_sampling_state}} to access the result in R
#' if it is too large to handle in memory. (If the file is big but fits in
#' memory, I recommend \code{read_csv} from the \code{readr} package, which will
#' yield an ordinary data frame.)
#'
#' Note that this file uses zero-based indices for topics, words, and documents,
#' not 1-based indices.
#'
#'
#' @param state_file the MALLET state (assumed to be gzipped)
#' @param outfile the name of the output file (will be clobbered)
#' @return the return value from the python script
#'
#' @seealso \code{\link{read_sampling_state}}
#'
#' @export
#'
simplify_state <- function (state_file, outfile) {
    if (Sys.which("python") == "") {
        stop("This function requires python to run.")
    }
    scpt <- file.path(path.package("dfrtopics"), "python",
                      "simplify_state.py")
    system2("python", args=c(scpt, state_file), stdout=outfile)
}

#' Read in a Gibbs sampling state
#'
#' This function reads in a Gibbs sampling state represented by
#' \code{document,word,topic,count} rows to a
#' \code{\link[bigmemory]{big.matrix}}. This gives the model's assignments of
#' words to topics within documents. MALLET itself remembers token order, but
#' this is meaningless when working with JSTOR wordcounts file, and more
#' generally in ordinary LDA the words are assumed exchangeable within
#' documents.
#'
#'
#' for reading MALLET \code{InstanceList}s: \code{\link{instances_vocabulary}}
#' and \code{\link{instances_ids}}. \emph{N.B.} the MALLET sampling state, and
#' the "simplified state" output by this function to disk, index documents,
#' words, and topics from zero, but the dataframe returned by this function
#' indexes these from one for convenience within R.
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
#' @param data_type the C++ type to store the data in. If all values have
#'   magnitude less than \eqn{2^15}, you can get away with \code{"short"}, but
#'   guess what? Linguistic data hates you, and a typical vocabulary can easily
#'   include more word types than that, so the default is \code{"integer"}.
#' @param big_workdir the working directory where
#'   \code{\link[bigmemory]{read.big.matrix}} will store its temporary files. By
#'   default, uses \code{\link[base]{tempdir}}, but if you have more scratch
#'   space elsewhere, use that for handling large sampling states.
#'
#' @seealso \code{\link{simplify_state}}, \code{\link{write_mallet_state}},
#' \code{\link{tdm_topic}}, \pkg{bigmemory}
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
'The sampling state is unavailable directly. To retrieve the state, use:

m <- load_state(m)'
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
#' This is a convenience function for loading the sampling state into a model
#' object. If no file names are supplied, the state information will be written
#' to (possibly very large) temporary files before being read back into memory.
#'
#' @param m \code{mallet_model} object
#' @param simplified_state_file Name of simplified state file (from
#'   \code{\link{simplify_state}}. If NULL, a temporary file will be created
#' @param mallet_state_file Name of file with mallet's own gzipped
#'   sampling-state output (from \code{\link{write_mallet_state}} or
#'   command-line mallet). If NULL, the state will be written to a temporary
#'   file
#'
#' @return a copy \code{m} with the sampling state loaded (available via
#'   \code{sampling_state(m)}
#'
#' @export
#'
load_sampling_state <- function (m,
                                 simplified_state_file=NULL,
                                 mallet_state_file=NULL) {
    tmp_ss <- F
    tmp_ms <- F
    if (is.null(simplified_state_file)) {
        simplified_state_file <- tempfile()
        tmp_ss <- T
        if (is.null(mallet_state_file)) {
            mallet_state_file <- tempfile()
            tmp_ms <- T
            message("Writing MALLET state to temporary file")
            write_mallet_state(m, mallet_state_file)
        }
        message("Writing simplified sampling state to temporary file")
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
