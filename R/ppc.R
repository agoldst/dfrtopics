imi <- function (m, k, w=vocab(m)) {
    doc_topics_k <- doc_topics(m)[ , k]
    term_doc_k <- tdm_topic(m, k)
    calc_imi(doc_topics_k, term_doc_k, match(w, vocab(m)))
}

imi_group <- function (m, k, g, w=vocab(m)) {
    g <- as.factor(g)
    stopifnot(length(g) == n_docs(m))
    group_topics_k <- tapply(doc_topics(m)[ , k], g, sum)
    term_group_k <- sum_row_groups(tdm_topic(m, k), g)
    calc_imi(group_topics_k, term_group_k, match(w, vocab(m)))
}

# David M helps out here:
# https://lists.cs.princeton.edu/pipermail/topic-models/2012-March/001779.html
calc_imi <- function (doc_topics_k, term_doc_k, w) {
    # add pseudo-count of 1 so we never take log 0
    # TODO would it be better just to throw out docs with weight 0 ?
    num_docs <- length(doc_topics_k)
    p_d <- (doc_topics_k + 1) /
        (sum(doc_topics_k) + num_docs)
    H_D <- -sum(p_d * log2(p_d))
    # p(d|w) = N(w, d) / N(w) = N(w, d) / sum_d N(w, d)
    # and add pseudo-count of 1

    # TODO RCpp or RCppParallel or gtfo
    p_dw <- normalize_rows(
        term_doc_k[w, ] + 1
    )

    H_Dw <- -rowSums(p_dw * log2(p_dw))

    H_D - H_Dw
}

topic_imi <- function (m, k, n_words) { 
    if (missing(n_words)) { 
        topw <- top_words(m)
    } else {
        topw <- top_words(m, n_words)
    }

    imi(m, k, topw$word[topw$topic == k])
}

topic_mi <- function (m, k, g) {
    pw <- tw_smooth_normalize(m)(topic_words(m))[k, ] 
    if (missing(g)) {
        imis <- imi(m, k)
    } else {
        imis <- imi_group(m, k, g)
    }
    calc_topic_mi(pw, imis)
}

calc_topic_mi <- function (pw, imi_w) sum(pw * imi_w)

ppc_word <- function (m, k, words, n_reps=10) {
    p_w <- topic_words(m)[k, ] / sum(topic_words(m)[k, ])

    dt_k <- doc_topics(m)[k, ]
    w <- match(words, vocab(m))
    imi_rep <- replicate(n_reps,
        calc_imi(dt_k, simulate_tdm_topic(dt_k, p_w), w)
    )

    rownames(imi_rep) <- words

    imi_rep
}

simulate_tdm_topic <- function (dt_k, p_w_k) {
    # for each document d, all that matters is the total number of words
    # assigned to topic k. This gives the number of words to draw from k
    # in the simulation.
    # FUN.VALUE just gives vapply the length of the vector (V)
    vapply(dt_k, rmultinom, FUN.VALUE=p_w_k,
           n=1, prob=p_w_k)
}



# TODO GROUPS
