#' Instantaneous mutual information of words and documents
#'
#' Calculates the instantaneous mutual information (IMI) for words and documents within a given topic. This measures the degree to which words assigned to that topic are independently distributed over documents. With a specified document-grouping \code{groups}, this instead measures the degree to which words are distributed independently over those groups of documents.
#'
#' In ordinary LDA, the distribution of words over topics is independent of documents: that is, in the model's assignment of words to topics, knowing which document a word is in shouldn't tell you anything about more about that word than knowing its topic. In practice, this independence assumption is always violated by the estimated topics. For a given topic \eqn{k}, the IMI measures a given word's contribution to this violation as
#'
#' \deqn{
#' H(D|K=k) - H(D|W=w, K=k)
#' }
#' where \eqn{H} denotes the entropy, that is,
#'
#' \deqn{
#' -\sum_d p(d|k) \log p(d|k) + \sum_d p(d|w, k) \log p(d|w, k)
#' }
#'
#' The probabilities are simply found from the counts of word tokens within documents \eqn{d} assigned to topic \eqn{k}, as these are recorded in the final Gibbs sampling state.
#'
#' The overall independence violation for topic \eqn{k} is the expectation of 
#' this quantity over words in that topic,
#'
#' \deqn{
#' \sum_w p(w|k) (H(D|k) - H(D|w, k)) 
#' }
#'
#' For obtaining the sum, see \code{\link{mi_topic}}.
#'
#' If a grouping factor \code{groups} is given, the IMI is instead taken not over documents but over groups of documents For example, suppose that the documents are articles drawn from three different periodicals; we might measure the degree to which knowing which periodical the document comes from tells us about which words have been assigned to the topic. Sampled word counts are simply summed over the document groups and then the calculation proceeds with groups in place of documents \eqn{d} in the formulas above.
#'
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via \code{\link{load_sampling_state}}
#'
#' @param k topic number (calculations are only done for one topic at a time)
#'
#' @param words vector of words to calculate IMI values for.
#'
#' @param groups optional grouping factor with one element for each document. If not NULL, IMIs are calculated over document groups rather than documents.
#'
#' @return a vector of scores, in the same order as \code{w}
#'
#' @seealso \code{\link{mi_topic}}, \code{\link{calc_imi}} for the calculation
#'
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models. \emph{Empirical Methods in Natural Language Processing}. \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'
#' @export
#'
imi <- function (m, k, words=vocabulary(m), groups=NULL) {
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
#' The actual computation of IMI (instantaneous mutual information) values for \code{\link{imi}} is done by
#' this routine. If you are calling \code{imi} repeatedly
#' for a given topic, you can gain some speed by pre-calculating the
#' inputs and supplying them directly to this function.
#'
#' @param doc_topics_k vector of \emph{counts}, one for each document, of words assigned to topic \eqn{k}
#'
#' @param term_doc_k matrix of counts, where element \eqn{i,j} is the number of words of type \eqn{i} in document (or document-group) \eqn{j} is assigned to topic \eqn{k}
#'
#' @return a vector of scores, one for each row of \code{term_doc_k}
#'
#' @seealso \code{\link{imi}} which calls this; see under "Details" there for more detail on IMI
#'
#' @references I have referred to a helpful note by David Mimno here: \url{https://lists.cs.princeton.edu/pipermail/topic-models/2012-March/001779.html}.
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
#' Given a sparse matrix in which each row is a discrete probability distribution over the index, takes the entropy \eqn{-\sum_j x_j \log_2 x_j} row-wise (considering only non-zero entries).
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
#' Calculates the mutual information of words and documents within a given topic. This measures the degree to which the estimated distribution over words within the topic violates the assumption that it is independent of the distribution of words over documents.
#'
#' The mutual information is given by
#'
#' \deqn{
#' MI(W, D|K=k) = \sum_{w, d} p(w, d|k) \log\frac{p(w, d|k)}{p(w|k) p(d|k)}
#' }
#'
#' In the limit of true independence, the fraction in the log is one and the MI 
#' is zero. In general, we can rewrite the sum as
#'
#' \deqn{
#' \sum_d p(d|k) \sum_w p(w|d, k) \log\frac{p(w|d, k)}{p(w|k)}
#' }
#'
#' which is \eqn{E_D(KL(W|d, W)}, the expected divergence of the conditional distribution from the marginal distribution. It can be shown with some algebra that
#'
#' \deqn{
#' MI(W, D|k) = \sum_{w} p(w|k) IMI(w|k)
#' }
#'
#' where the IMI is defined as specified in the Details for \code{\link{imi}}. This is the formula used for calculation here.
#'
#' We can replace \eqn{D} with a grouping over documents and the formulas carry 
#' over without further change, now expressing the mutual information of those groupings and words within the topic.
#'
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via \code{\link{load_sampling_state}}
#'
#' @param k topic number (calculations are only done for one topic at a time)
#'
#' @param groups optional grouping factor for documents. If omitted, the MI over documents is calculated.
#'
#' @return a single value, giving the estimated mutual information.
#'
#' @seealso \code{\link{imi}}, \code{\link{imi_check}}, \code{\link{mi_check}}
#'
#' @export
mi_topic <- function (m, k, groups=NULL) {
    pw <- tw_smooth_normalize(m)(topic_words(m))[k, ] 
    imis <- imi(m, k, groups=groups)
    sum(pw * imis)
}

#' Calculate IMI scores for the top words in a topic
#'
#' As a convenience, this function extracts a topic's top words and returns a data frame with their IMI scores over documents or document groups.
#'
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via \code{\link{load_sampling_state}}
#'
#' @param k topic number (calculations are only done for one topic at a time)
#'
#' @param groups optional grouping factor with one element for each document
#'
#' @param ... passed on to \code{\link{top_words}}: use to specify number of top words and/or weighting function
#'
#' @return the data frame from \code{\link{top_words}} with an addition \code{imi} column
#'
#' @seealso \code{\link{imi}}, \code{\link{mi_topic}}, \code{\link{top_words}}
#' 
#' @export
#'
top_words_imi <- function (m, k, groups=NULL, ...) {
    result <- top_words(m, ...)
    result <- result[result$topic == k, ]
    imis <- imi(m, k, result$word, groups)
    result$imi <- imis
    result
}

#' Posterior predictive checking for individual words
#'
#' This function provides a way to check the fit of the topic model at the individual words-in-topics level by comparing the obtained instantaneous mutual information for those words to scores derived from simulations from the posterior. Large deviations from simulated values may indicate a poorer fit. In particular, large negative deviations indicate words which are more uniformly distributed across documents that the model expects (e.g., boilerplate text appearing in every document), and large positive deviations indicate words which are more sharply localized than the model expects.
#'
#' For a given topic \eqn{k}, a simulation draws a new term-document matrix from the posterior for \eqn{d}. Since a topic is simply a multinomial distribution over the words, for a given document \eqn{d} we simply draw the same number of samples from this multinomial as there were words allocated to topic \eqn{k} in \eqn{d} in the model we are checking. Under the assumptions of the model, this is how the distribution \eqn{p(w, d|k)} arises. With this simulated topic-specific term-document matrix in hand, we recalculate the IMI scores for the given \code{words}. The process is replicated to obtain a reference distribution to compare the values from \code{\link{imi}} to.
#'
#' A reasonable way to make the comparison is to standardize the "actual" IMI values by the mean and standard deviation of the simulated values. Mimno and Blei (2011) call this the "deviance" measure, recommending over \eqn{p} values because the latter are likely to vanish.
#'
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via \code{\link{load_sampling_state}}
#'
#' @param k topic number (calculations are only done for one topic at a time)
#'
#' @param words vector of words to calculate IMI values for
#'
#' @param groups optional grouping factor for documents. If supplied, the IMI values will be for words over groups rather than over individual documents
#'
#' @param n_reps number of simulations
#'
#' @return a matrix of simulated IMI values, with one row for each element of \code{words} and one column for each simulation
#'
#' @seealso \code{\link{mi_check}}, \code{\link{imi}}
#'
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models. \emph{Empirical Methods in Natural Language Processing}. \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'
#' @export
#' 
imi_check <- function (m, k, words, groups=NULL, n_reps=10) {
    p_w <- topic_words(m)[k, ] / sum(topic_words(m)[k, ])

    dt_k <- doc_topics(m)[ , k]
    if (!is.null(groups)) {
        groups <- factor(groups)
        dt_k <- tapply(dt_k, groups, sum)
    }
    w <- match(words, vocabulary(m))

    # TODO verify this is not stupid

    # there's no point simulating frequencies for words we're not scoring 
    # since IMI(w|k) depends only on p(d|k) and p(d|w, k)
    if (length(w) < length(p_w)) {
        p_w <- c(p_w[w], sum(p_w[-w]))
        skipped_words <- TRUE
    } else {
        skipped_words <- FALSE
    }

    imi_rep <- replicate(n_reps,
        calc_imi(dt_k, simulate_tdm_topic(dt_k, p_w))
    )

    if (skipped_words) {
        imi_rep <- imi_rep[-length(p_w), ]
    }

    rownames(imi_rep) <- words

    imi_rep
}


#' Posterior predictive checking for topics
#'
#' This function provides a way to check the fit of the topic model by comparing the obtained mutual information for topics to values derived from simulations from the posterior. Large deviations from simulated values may indicate a poorer fit.
#'
#' For a given topic \eqn{k}, a simulation draws a new term-document matrix from the posterior for \eqn{d}. Since a topic is simply a multinomial distribution over the words, for a given document \eqn{d} we simply draw the same number of samples from this multinomial as there were words allocated to topic \eqn{k} in \eqn{d} in the model we are checking. Under the assumptions of the model, this is how the distribution \eqn{p(w, d|k)} arises. With this simulated topic-specific term-document matrix in hand, we recalculate the MI. The process is replicated to obtain a reference distribution to compare the values from \code{\link{mi_topic}} to.
#'
#' @param m \code{mallet_model} object \emph{with sampling state loaded} via \code{\link{load_sampling_state}}
#'
#' @param k topic number (calculations are only done for one topic at a time)
#'
#' @param groups optional grouping factor for documents. If supplied, the IMI values will be for words over groups rather than over individual documents
#'
#' @param n_reps number of simulations
#'
#' @return a vector of simulated MI values
#'
#' @seealso \code{\link{imi_check}}, \code{\link{mi_topic}}
#'
#' @references Mimno, D., and Blei, D. 2011. Bayesian Checking for Topic Models. \emph{Empirical Methods in Natural Language Processing}. \url{http://www.cs.columbia.edu/~blei/papers/MimnoBlei2011.pdf}.
#'
#' @export
#' 
mi_check <- function (m, k, groups=NULL, n_reps=10) {
    p_w <- topic_words(m)[k, ] / sum(topic_words(m)[k, ])
    dt_k <- doc_topics(m)[ , k]
    if (!is.null(groups)) {
        groups <- factor(groups)
        dt_k <- tapply(dt_k, groups, sum)
    }

    replicate(n_reps,
        sum(p_w * calc_imi(dt_k, simulate_tdm_topic(dt_k, p_w)))
    )
}

# Simulation function
#
# dt_k: vector of counts of words assigned to topic k in documents, N(d|k)
# p_w_k: vector of word probabilities in topic k, p(w|k)

simulate_tdm_topic <- function (dt_k, p_w_k) {

    # for each document d, all that matters is the total number of words
    # assigned to topic k. This gives the number of words to draw from k
    # in the simulation.
    #
    # rmultinom is not vectorized in the sample size parameter, so we
    # resort to vapply; FUN.VALUE just gives vapply the length of the
    # rvector
    #
    # TODO speed and space, dude. Considering that the rmultinom
    # algorithm is just to take a binomial for category 1, then take
    # another binomial for category 2 using the remaining trials, etc.,
    # we could just implement this sparsely ourselves by terminating
    # when we reach the needed number of words and then returning
    # triplets instead of a vector padded out with zeroes

    vapply(dt_k, rmultinom, FUN.VALUE=p_w_k,
           n=1, prob=p_w_k)
}


