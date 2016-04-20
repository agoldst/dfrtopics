context("model alignment")

library(dplyr)

# run a scrap model: we're just doing manipulation here, not worrying
# about quality

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"), full.names=T)[1:60]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

insts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(200) %>%
    wordcounts_texts() %>%
    make_instances(stoplist_file)

V <- length(instances_vocabulary(insts))
meta <- read_dfr_metadata(file.path(data_dir, "citations.tsv"))

M <- 4
K <- 8
ms <- replicate(M, train_model(
        insts,
        n_topics=K,
        n_iters=200,
        threads=1, 
        alpha_sum=5,
        beta=0.01,
        n_hyper_iters=20,
        n_burn_in=20,
        n_max_iters=10,
        metadata=meta
    ),
    simplify=F
)

# use all words
dst <- model_distances(ms, V)

test_that("naive_cluster does the right thing with trivial data", {
    expect_equal(dfrtopics:::naive_cluster(c(1, 100, 100, 1), 2, 2, 1),
                 c(0, 1, 0, 1))
    # setting a big threshold should make no difference
    expect_equal(dfrtopics:::naive_cluster(c(1, 100, 100, 1), 2, 2, 1000),
                 c(0, 1, 0, 1))
})

test_that("model_distances returns something of the right form", {
    expect_is(dst, "model_distances")
    expect_equal(length(dst), M - 1)
    expect_equal(sapply(dst, length), seq(M - 1, 1))
    expect_equal(lapply(do.call(c, dst), dim),
        rep(list(c(K, K)), M * (M - 1) / 2))

    # calculate 1.1:M.K
    jsd <- JS_divergence(
        tw_smooth_normalize(ms[[1]])(topic_words(ms[[1]]))[1, ],
        tw_smooth_normalize(ms[[M]])(topic_words(ms[[M]]))[K, ]
    )
    # which should be retrieved as follows
    expect_equal(dst[[1]][[M - 1]][K, 1], jsd)
    # or as follows
    expect_equal(dst[1, M, 1, K], jsd)
    # or as follows
    dst_flat <- do.call(c, lapply(dst, do.call, what=c))
    expect_equal(dst_flat[K^2 * (M - 2) + K], jsd)
})

test_that("unnesting distances is correct", {
    dummy <- list(
        list(matrix(1:4, nrow=2), matrix(5:8, nrow=2)),
        list(matrix(9:12, nrow=2))
    )
    expect_equal(dfrtopics:::unnest_model_distances(dummy), 1:12)
})

test_that("clustering two models with high threshold fully aligns them", {
    dd <- model_distances(ms[1:2], 20)
    cl <- align_topics(dd, 10000000)
    expect_equal(sort(cl$cluster[cl$model == 1]), 1:K)
    expect_equal(sort(cl$cluster[cl$model == 2]), 1:K)

})

test_that("in clustering two models, the closest pair is indeed grouped", {
    dd <- model_distances(ms[1:2], 20)
    cl <- align_topics(dd, 10000000)
    closest <- which(dd[[1]][[1]] == min(dd[[1]][[1]]), arr.ind=TRUE)
    expect_equal(cl$cluster[cl$model == 1 & cl$topic == closest[2]],
        cl$cluster[cl$model == 2 & cl$topic == closest[1]])
})

test_that("clustering meets up-to-one constraint", {
    cl <- align_topics(dst)
    expect_equal(length(cl$cluster), M * K)
    expect_true(all(1 <= cl$cluster & cl$cluster <= M * K))
    ck <- as.data.frame(cl) %>%
        group_by(cluster) %>%
        summarize(no_dupes=anyDuplicated(model) == 0)
    expect_true(all(ck$no_dupes))
})




