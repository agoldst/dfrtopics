context("Mutual Information")

# 5 vectors summing to 1 
p <- t(rmultinom(5, 100, runif(8)) / 100)

test_that("entropy is correct", {
    p1 <- p[1, ]
    h <- -sum(p1[p1 != 0] * log2(p1[p1 != 0]))
    expect_equal(entropy(p1), h)
})

test_that("row_entropies is correct", {
    lp <- log2(p)
    lp[!is.finite(lp)] <- 0
    h <- -rowSums(p * lp)
    expect_equal(row_entropies(Matrix::Matrix(p, sparse=TRUE)), h)
    expect_equal(row_entropies(p), h, info="cast from ordinary matrix")
})

test_that("calc_imi is correct", {
    # example from
    # https://lists.cs.princeton.edu/pipermail/topic-models/2012-March/001779.html
    doc_counts <- c(2, 6, 2)
    tdm <- Matrix::Matrix(c(1, 1, 1,
                            1, 5, 1), nrow=2, byrow=TRUE, sparse=TRUE)
    imis <- calc_imi(doc_counts, tdm)
    expect_equal(imis, 1.37 - c(1.58, 1.15),
        tolerance=0.02)
})



# run a scrap model

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"),
                 full.names=T)[61:120]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

n_topics <- 8

insts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(1000) %>%
    wordcounts_texts() %>%
    make_instances(stoplist_file)

m <- train_model(
    insts,
    n_topics=n_topics,
    n_iters=200,
    threads=1, 
    alpha_sum=5,
    beta=0.01,
    n_hyper_iters=20,
    n_burn_in=20,
    n_max_iters=10,
    seed=42,
    metadata=read_dfr_metadata(file.path(data_dir, "citations.tsv"))
)

m <- load_sampling_state(m)

# testing topic
k <- 5

test_that("An IMI calculation does what we think", {
    score <- imi(m, k, words="chaucer")
    Nd <- doc_topics(m)[ , k]
    N <- sum(Nd)
    Hd <- -sum(Nd[Nd != 0] / N * log2(Nd[Nd != 0] / N))
    w <- match("chaucer", vocabulary(m))
    Nwd <- tdm_topic(m, k)[w, ]
    Nw <- topic_words(m)[k, w]
    Hdw <- -sum(Nwd[Nwd != 0] / Nw * log2(Nwd[Nwd != 0] / Nw))
    expect_equal(score, Hd - Hdw)
})

test_that("Getting IMIs for top words proceeds as expected", {
    scores <- top_words_imi(m, k, n=10)
    expect_equal(names(scores), c("topic", "word", "weight", "imi"))
})

test_that("Scoring for overall MI proceeds as expected", {
    mi5 <- mi_topic(m, k)
    expect_equal(length(mi5), 1)
    expect_true(is.finite(mi5))
})

year <- factor(substr(metadata(m)$pubdate, 1, 4))

test_that("Grouping for IMIs does what we think", {
    imi_chaucer <- imi(m, k, words="chaucer", groups=year)
    Nd <- sum_row_groups(doc_topics(m), year)[ , k]
    N <- sum(doc_topics(m)[ , k])
    Hd <- -sum(Nd[Nd != 0] / N * log2(Nd[Nd != 0] / N))
    w <- match("chaucer", vocabulary(m))
    Nwd <- sum_col_groups(tdm_topic(m, k), year)[w, ]
    Nw <- topic_words(m)[k, w]
    Hdw <- -sum(Nwd[Nwd != 0] / Nw * log2(Nwd[Nwd != 0] / Nw))
    expect_equal(imi_chaucer, Hd - Hdw)
})

test_that("Grouping for MI proceeds as we expect", {
    mi5 <- mi_topic(m, k, groups=year)
    expect_equal(length(mi5), 1)
    expect_true(is.finite(mi5))
})

test_that("Simulated topic TDM has the right look", {
    x <- rmultinom_sparse(doc_topics(m)[ , k],
       tw_smooth_normalize(m)(topic_words(m))[k, ])
    expect_equal(dim(x), c(length(vocabulary(m)), n_docs(m)))
    expect_equal(Matrix::colSums(x), doc_topics(m)[ , k])
    expect_true(all(x >= 0))
})

test_that("IMI PPC at least yields something of the right shape", {
    ppc <- imi_check(m, k, c("chaucer", "tale"), n_reps=20)
    expect_equal(dim(ppc), c(2, 20))
    expect_true(all(is.finite(ppc)))
})


test_that("MI PPC at least yields something of the right shape", {
    ppc <- mi_check(m, k, n_reps=20)
    expect_equal(length(ppc), 20)
    expect_true(all(is.finite(ppc)))
})
