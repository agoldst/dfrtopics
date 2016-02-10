#' Instantaneous mutual information of words and documents in a topic
#' 
#' Calculates the instantaneous mutual information (IMI) for words and documents
#' within a given topic. This measures the degree to which words assigned to
#' that topic are independently distributed over documents. With a specified
#' document-grouping \code{groups}, this instead measures the degree to which
#' words are distributed independently over those groups of documents.
#' 
#' In ordinary LDA, the distribution of words over topics is independent of
#' documents: that is, in the model's assignment of words to topics, knowing
#' which document a word is in shouldn't tell you anything about more about that
#' word than knowing its topic. In practice, this independence assumption is
#' always violated by the estimated topics. For a given topic \eqn{k}, the IMI
#' measures a given word's contribution to this violation as
#' 
#' \deqn{ H(D|K=k) - H(D|W=w, K=k) } where \eqn{H} denotes the entropy; i.e.,
#' the IMI is calculated as
#' 
#' \deqn{ -\sum_d p(d|k) \log p(d|k) + \sum_d p(d|w, k) \log p(d|w, k) }
#' 
#' The probabilities are simply found from the counts of word tokens within
#' documents \eqn{d} assigned to topic \eqn{k}, as these are recorded in the
#' final Gibbs sampling state.
#' 
#' The overall independence violation for topic \eqn{k} is the expectation of 
#' this quantity over words in that topic,
#' 
#' \deqn{ \sum_w p(w|k) (H(D|k) - H(D|w, k)) }
#' 
#' For obtaining the sum, see \code{\link{mi_topic}}.
#' 
#' If a grouping factor \code{groups} is given, the IMI is instead taken not
#' over documents but over groups of documents For example, suppose that the
#' documents are articles drawn from three different periodicals; we might
#' measure the degree to which knowing which periodical the document comes from
#' tells us about which words have been assigned to the topic. Sampled word
#' counts are simply summed over the document groups and then the calculation
#' proceeds with groups in place of documents \eqn{d} in the formulas above.
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param words vector of words to calculate IMI values for.
#'   
#' @param groups optional grouping factor with one element for each document. If
#'   not NULL, IMIs are calculated over document groups rather than documents.
#'   
#' @return a vector of scores, in the same order as \code{w}
#'   
#' @seealso \code{\link{mi_topic}}, \code{\link{calc_imi_topic}} for the
#'   calculation
#'   
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models.
#'   \emph{Empirical Methods in Natural Language Processing}.
#'   \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'   
#' @examples \dontrun{
#' # obtain imi scores for a topic's top words
#' library(dplyr)
#' k <- 15
#' top_words(m, n=10) %>%
#'     filter(topic == k) %>%
#'     mutate(imi=imi_topic(m, k, word))
#' }
#' 
#' 
#' @export
#' 
imi_topic <- function (m, k, words=vocabulary(m), groups=NULL) {
    doc_topics_k <- doc_topics(m)[ , k]
    w <- match(words, vocabulary(m))

    # TODO could skip getting the full tdm_k using bigsplit
    term_doc_k <- tdm_topic(m, k)[w, , drop=FALSE]

    # apply grouping if given
    if (!is.null(groups)) {
        groups <- as.factor(groups)
        stopifnot(length(groups) == n_docs(m))
        doc_topics_k <- tapply(doc_topics_k, groups, sum)
        term_doc_k <- sum_col_groups(term_doc_k, groups)
    }

    calc_imi(doc_topics_k, term_doc_k)
}

#' IMI calculation routine
#' 
#' The actual computation of IMI (instantaneous mutual information) values for
#' \code{\link{imi_topic}} is done by this routine. If you are calling
#' \code{imi_topic} repeatedly for a given topic, you can gain some speed by
#' pre-calculating the inputs and supplying them directly to this function.
#' 
#' @param doc_topics_k vector of \emph{counts}, one for each document, of words
#'   assigned to topic \eqn{k}
#'   
#' @param term_doc_k matrix of counts, where element \eqn{i,j} is the number of
#'   words of type \eqn{i} in document (or document-group) \eqn{j} is assigned
#'   to topic \eqn{k}
#'   
#' @return a vector of scores, one for each row of \code{term_doc_k}
#'   
#' @seealso \code{\link{imi_topic}} which calls this; see under "Details" there
#'   for more detail on IMI
#'   
#' @references I have referred to a helpful note by David Mimno here:
#'   \url{https://lists.cs.princeton.edu/pipermail/topic-models/2012-March/001779.html}.
#'   
#'   
#' @export
#' 
calc_imi <- function (doc_topics_k, term_doc_k) {
    p_d <- doc_topics_k / sum(doc_topics_k)

    # H(D|k)
    H_D <- entropy(p_d)

    # p(d|w) = N(w, d) / N(w) = N(w, d) / sum_d N(w, d)
    p_dw <- normalize_rows(term_doc_k) 
    H_Dw <- row_entropies(p_dw)

    H_D - H_Dw
}

#' Entropy of sparse-matrix rows
#' 
#' Given a sparse matrix in which each row is a discrete probability
#' distribution over the index, takes the entropy \eqn{-\sum_j x_j \log_2 x_j}
#' row-wise (considering only non-zero entries).
#' 
#' @param m sparse \code{\link[Matrix]{Matrix}}.
#'   
#' @return ordinary vector of entropies.
#'   
#' @export
row_entropies <- function (m) {
    if (!is(m, "sparseMatrix")) {
        m <- Matrix::Matrix(m, sparse=TRUE)
    }
    calc_row_entropies(m)
}

#' Mutual information of words and documents in a topic
#' 
#' Calculates the mutual information of words and documents within a given
#' topic. This measures the degree to which the estimated distribution over
#' words within the topic violates the assumption that it is independent of the
#' distribution of words over documents.
#' 
#' The mutual information is given by
#' 
#' \deqn{ MI(W, D|K=k) = \sum_{w, d} p(w, d|k) \log\frac{p(w, d|k)}{p(w|k)
#' p(d|k)} }
#' 
#' In the limit of true independence, the fraction in the log is one and the MI 
#' is zero. In general, we can rewrite the sum as
#' 
#' \deqn{ \sum_d p(d|k) \sum_w p(w|d, k) \log\frac{p(w|d, k)}{p(w|k)} }
#' 
#' which is \eqn{E_D(KL(W|d, W)}, the expected divergence of the conditional
#' distribution from the marginal distribution. It can be shown with some
#' algebra that
#' 
#' \deqn{ MI(W, D|k) = \sum_{w} p(w|k) IMI(w|k) }
#' 
#' where the IMI is defined as specified in the Details for
#' \code{\link{imi_topic}}. This is the formula used for calculation here.
#' 
#' We can replace \eqn{D} with a grouping over documents and the formulas carry 
#' over without further change, now expressing the mutual information of those
#' groupings and words within the topic.
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param groups optional grouping factor for documents. If omitted, the MI over
#'   documents is calculated.
#'   
#' @return a single value, giving the estimated mutual information.
#'   
#' @seealso \code{\link{imi_topic}}, \code{\link{imi_check}},
#'   \code{\link{mi_check}}
#'   
#' @export
mi_topic <- function (m, k, groups=NULL) {
    # TODO should we bother with smoothing?
    pw <- topic_words(m)[k, ] / sum(topic_words(m)[k, ])
    imis <- imi_topic(m, k, groups=groups)
    sum(pw * imis)
}


#' Posterior predictive checking for individual words
#' 
#' This function provides a way to check the fit of the topic model at the
#' individual words-in-topics level by comparing the obtained instantaneous
#' mutual information for those words to scores derived from simulations from
#' the posterior. Large deviations from simulated values may indicate a poorer
#' fit. In particular, large negative deviations indicate words which are more
#' uniformly distributed across documents that the model expects (e.g.,
#' boilerplate text appearing in every document), and large positive deviations
#' indicate words which are more sharply localized than the model expects.
#' 
#' For a given topic \eqn{k}, a simulation draws a new term-document matrix from
#' the posterior for \eqn{d}. Since a topic is simply a multinomial distribution
#' over the words, for a given document \eqn{d} we simply draw the same number
#' of samples from this multinomial as there were words allocated to topic
#' \eqn{k} in \eqn{d} in the model we are checking. Under the assumptions of the
#' model, this is how the distribution \eqn{p(w, d|k)} arises. With this
#' simulated topic-specific term-document matrix in hand, we recalculate the IMI
#' scores for the given \code{words}. The process is replicated to obtain a
#' reference distribution to compare the values from \code{\link{imi_topic}} to.
#' 
#' A reasonable way to make the comparison is to standardize the "actual" IMI
#' values by the mean and standard deviation of the simulated values. Mimno and
#' Blei (2011) call this the "deviance" measure, recommending over \eqn{p}
#' values because the latter are likely to vanish.
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param words vector of words to calculate IMI values for
#'   
#' @param groups optional grouping factor for documents. If supplied, the IMI
#'   values will be for words over groups rather than over individual documents
#'   
#' @param n_reps number of simulations
#'   
#' @return a data frame with \code{word}, \code{imi}, and \code{deviance}
#'   columns. The latter is the IMI standardized by the mean and standard
#'   deviation of the simulated values. The matrix of simulated values (one row
#'   per word) is available as the \code{"simulated"} attribute of the returned
#'   data frame.
#'   
#' @seealso \code{\link{imi_simulate}} for just the simulation results,
#'   \code{\link{mi_check}}, \code{\link{imi_topic}}
#'   
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models.
#'   \emph{Empirical Methods in Natural Language Processing}.
#'   \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'   
#' @export
#' 
imi_check <- function (m, k, words, groups=NULL, n_reps=20) {
    sims <- imi_simulate(m, k, words, groups, n_reps)

    imi <- imi_topic(m, k, words, groups)
    result <- data.frame(
        word=words,
        imi=imi,
        deviance=(imi - rowMeans(sims)) / apply(sims, 1, sd),
        stringsAsFactors=FALSE)
    attr(result, "simulated") <- sims
    result
}


#' Simulated IMI values
#' 
#' Given a topic and particular words, this function calculates IMI scores for
#' those words based on a simulated term-document matrix (conditioned on the
#' topic). These simulations are the basis for the posterior check in
#' \code{\link{imi_check}} (q.v.).
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param words vector of words to calculate IMI values for
#'   
#' @param groups optional grouping factor for documents. If supplied, the IMI
#'   values will be for words over groups rather than over individual documents
#'   
#' @param n_reps number of simulations
#'   
#' @return a matrix of simulated IMI values, with one row for each element of
#'   \code{words} and one column for each simulation
#'   
#' @seealso \code{\link{imi_check}}
#'   
#' @export
#' 
imi_simulate <- function (m, k, words, groups=NULL, n_reps=20) {
    N_w <- topic_words(m)[k, ]

    dt_k <- doc_topics(m)[ , k]
    if (!is.null(groups)) {
        groups <- factor(groups)
        dt_k <- tapply(dt_k, groups, sum)
    }
    w <- match(words, vocabulary(m))

    # TODO verify this is not stupid:
    # there's no point simulating frequencies for words we're not scoring 
    # since IMI(w|k) depends only on p(d|k) and p(d|w, k)
    if (length(w) < length(N_w)) {
        N_w <- c(N_w[w], sum(N_w[-w]))
        skipped_words <- TRUE
    } else {
        skipped_words <- FALSE
    }

    imi_rep <- replicate(n_reps,
        calc_imi(dt_k, rmultinom_sparse(dt_k, N_w))
    )

    if (skipped_words) {
        imi_rep <- imi_rep[-length(N_w), , drop=FALSE]
    }

    rownames(imi_rep) <- words

    imi_rep
}


#' Posterior predictive checking for topics
#' 
#' This function provides a way to check the fit of the topic model by comparing
#' the obtained mutual information for topics to values derived from simulations
#' from the posterior. Large deviations from simulated values may indicate a
#' poorer fit.
#' 
#' For a given topic \eqn{k}, a simulation draws a new term-document matrix from
#' the posterior for \eqn{d}. Since a topic is simply a multinomial distribution
#' over the words, for a given document \eqn{d} we simply draw the same number
#' of samples from this multinomial as there were words allocated to topic
#' \eqn{k} in \eqn{d} in the model we are checking. Under the assumptions of the
#' model, this is how the distribution \eqn{p(w, d|k)} arises. With this
#' simulated topic-specific term-document matrix in hand, we recalculate the MI.
#' The process is replicated to obtain a reference distribution to compare the
#' values from \code{\link{mi_topic}} to.
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param groups optional grouping factor for documents. If supplied, the IMI
#'   values will be for words over groups rather than over individual documents
#'   
#' @param n_reps number of simulations
#'   
#' @return a single-row data frame with \code{topic}, \code{mi}, and
#'   \code{deviance} columns. The latter is the MI standardized by the mean and
#'   standard deviation of the simulated values. The vector of simulated values
#'   is available as the \code{"simulated"} attribute of the returned data
#'   frame.
#'   
#' @seealso \code{\link{imi_check}}, \code{\link{mi_topic}}
#'   
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models.
#'   \emph{Empirical Methods in Natural Language Processing}.
#'   \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'   
#' @export
#' 
mi_check <- function (m, k, groups=NULL, n_reps=20) {
    sims <- mi_simulate(m, k, groups, n_reps)
    mi <- mi_topic(m, k, groups)
    result <- data.frame(
        topic=k,
        mi=mi,
        deviance=(mi - mean(sims)) / sd(sims)
    )
    attr(result, "simulated") <- sims
    result
}

#' Simulated MI values
#' 
#' Given a topic, this function resamples its distribution over words and
#' documents from the posterior. These simulations are the basis for the
#' posterior check in \code{\link{mi_check}} (q.v.).
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (calculations are only done for one topic at a time)
#'   
#' @param groups optional grouping factor for documents. If supplied, the IMI
#'   values will be for words over groups rather than over individual documents
#'   
#' @param n_reps number of simulations
#'   
#' @return a vector of MI scores for the simulations
#'   
#' @seealso \code{\link{mi_check}}, \code{\link{imi_simulate}}
#'   
#' @export
#' 
mi_simulate <- function (m, k, groups=NULL, n_reps=20) {
    N_w <- topic_words(m)[k, ]
    p_w <- N_w / sum(N_w)
    dt_k <- doc_topics(m)[ , k]
    if (!is.null(groups)) {
        groups <- factor(groups)
        dt_k <- tapply(dt_k, groups, sum)
    }

    replicate(n_reps,
        sum(p_w * calc_imi(dt_k, rmultinom_sparse(dt_k, N_w)))
    )
}



#' Visualize IMI scores for the top words in topics
#' 
#' As a diagnostic visualization, this function displays the IMI scores of the
#' top-weighted words in a topic, together with simulated values, on a scale set
#' by the distribution of simulated values. Extreme deviations of the actual IMI
#' scores indicate departures from the multinomial assumption of the model.
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (only one topic at a time)
#'   
#' @param groups optional grouping factor with one element for each document
#'   
#' @param n_reps number of simulations
#'   
#' @param ... passed on to \code{\link{top_words}}: use to specify number of top
#'   words and/or weighting function
#'   
#' @return \code{ggplot2} plot object
#'   
#' @seealso \code{\link{imi_topic}}, \code{\link{imi_check}} 
#'   \code{\link{plot_imi_check}}
#'   
#' @export
#' 
plot_imi_check <- function (m, k,
                            groups=NULL, n_reps=20, ...) {
    if (!requireNamespace("ggplot2", quietly=TRUE)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    top <- top_words(m, ...)
    top <- top[top$topic == k, ]
    ck <- imi_check(m, k, top$word, groups, n_reps)
    tidy_check(ck) %>%
        ggplot2::ggplot(
            ggplot2::aes(deviance, word, color=type, alpha=type)
        ) +
            ggplot2::geom_point(ggplot2::aes(color=type, alpha=type)) +
            ggplot2::scale_alpha_manual(values=c(actual=1, simulated=0.25)) +
            ggplot2::scale_color_manual(
                values=c(actual="red", simulated="black")
            ) +
            ggplot2::scale_y_discrete(limits=rev(top$word)) +
            ggplot2::theme(
                axis.title.y=ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank(),
                axis.text.y=ggplot2::element_text(color="black")
            ) +
        ggplot2::ggtitle(
            paste0("Instantaneous mutual information of words in topic ", k)
        )
}

#' Visualize a topic's MI score in comparison to simulated values
#' 
#' As a diagnostic visualization, this function displays the MI score of a topic
#' in comparison with the distribution of simulated values, on a scale
#' normalized to the latter. Extreme deviations of the actual MI score indicates
#' a departure from the multinomial assumption of the model. Exploring these
#' deviations over various document groupings may help to reveal the driving
#' factor of those deviations (e.g. variation over time).
#' 
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via
#'   \code{\link{load_sampling_state}}
#'   
#' @param k topic number (only one topic at a time)
#'   
#' @param groups optional grouping factor with one element for each document
#'   
#' @param n_reps number of simulations
#'   
#' @return \code{ggplot2} plot object
#'   
#' @seealso \code{\link{mi_topic}}, \code{\link{mi_check}},
#'   \code{\link{plot_imi_check}}
#'   
#' @export
#' 
plot_mi_check <- function (m, k, groups=NULL, n_reps=20) {
    if (!requireNamespace("ggplot2", quietly=TRUE)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    ck <- tidy_check(mi_check(m, k, groups, n_reps))
    act_dev <- ck$deviance[ck$type == "actual"]
    ggplot2::ggplot(ck[ck$type == "simulated", ],
        ggplot2::aes(deviance)) +
        ggplot2::geom_histogram(binwidth=0.75, fill="grey70") +
        ggplot2::annotate(geom="text",
            label="actual", x=act_dev, y=n_reps / 10, vjust=0) +
        ggplot2::annotate(geom="segment", x=act_dev, y=0,
                 xend=act_dev, yend=n_reps / 10) +
        ggplot2::ggtitle(paste0("Simulated mutual information: ",
                                topic_labels(m, 5)[k]))
}

#' Tidy results of posterior checks
#' 
#' This is a little utility function for bringing the results of
#' \code{\link{imi_check}} and \code{\link{mi_check}} into a tidy data-frame
#' form.
#' 
#' @param x result of \code{\link{imi_check}} or \code{\link{mi_check}}
#'   
#' @return data frame with columns corresponding to the checking unit (word or
#'   topic), the discrepancy (IMI or MI), the rescaled discrepancy (deviance),
#'   and an indicator, \code{type}, of whether the value is simulated or actual
#'   
#' @seealso \code{\link{mi_check}}, \code{\link{imi_check}}
#'   
#' @export
#' 
tidy_check <- function (x) { 
    key <- names(x)[1]
    score <- names(x)[2]
    sims <- attr(x, "simulated")
    if (is.vector(sims)) {  # mi_check just gives a vector of sims
        sims_frm <- data.frame(key=x[[key]], score=sims)
        names(sims_frm) <- c(key, score)
        sims_frm$deviance <- scale(sims)
    } else if (is.matrix(sims)) {
        sims_frm <- gather_matrix(sims,
            col_names=c(key, "trial", score),
            row_major=TRUE)

        # scale() operates on columns, not rows; when we unroll the
        # transpose by columns, we'll get the (rescaled) original by rows 
        sims_frm$deviance <- as.numeric(scale(t(sims)))
    } else {
        stop('"simulated" attribute is missing or of the wrong type.')
    }
    result <- dplyr::bind_rows(
        actual=x[, c(key, score, "deviance")],
        simulated=sims_frm[ , c(key, score, "deviance")],
        .id="type")
    result[ , c(key, score, "deviance", "type")]
}

