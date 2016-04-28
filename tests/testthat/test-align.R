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
s <- sample(10000, 1)
set.seed(s)

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
        metadata=meta,
        seed=sample(10000, 1)
    ),
    simplify=F
)

# use all words
dst <- model_distances(ms, V)

test_that("naive_cluster does the right thing with trivial data", {
    expect_equal(
        dfrtopics:::naive_cluster(c(1, 100, 100, 1), c(2, 2), 1)$clusters,
        list(0:1, 0:1)
    )
    # setting a big threshold should make no difference
    expect_equal(
        dfrtopics:::naive_cluster(c(1, 100, 100, 1), c(2, 2), 1000)$clusters,
        list(0:1, 0:1)
    )
})

test_that("model_distances returns something of the right form", {
    expect_is(dst, "model_distances")
    expect_equal(length(dst$d), M - 1)
    expect_equal(sapply(dst$d, length), seq(M - 1, 1))
    expect_equal(lapply(do.call(c, dst$d), dim),
        rep(list(c(K, K)), M * (M - 1) / 2))

    # calculate 1.1:M.K
    jsd <- JS_divergence(
        tw_smooth_normalize(ms[[1]])(topic_words(ms[[1]]))[1, ],
        tw_smooth_normalize(ms[[M]])(topic_words(ms[[M]]))[K, ]
    )
    # which should be retrieved as follows
    expect_equal(dst$d[[1]][[M - 1]][1, K], jsd)
    # or as follows
    expect_equal(dst[1, M, 1, K], jsd)
})

test_that("unnesting distances is correct", {
    dummy <- list(d=list(
        list(matrix(1:4, nrow=2, byrow=T), matrix(5:8, nrow=2, byrow=T)),
        list(matrix(9:12, nrow=2, byrow=T))
    ))
    expect_equal(dfrtopics:::unnest_model_distances(dummy), 1:12)
})

test_that("we can take distances between non-overlapping models", {
    m_voc <- list.files(file.path(data_dir, "wordcounts"),
                        full.names=T)[61:120] %>% 
        read_wordcounts() %>%
        wordcounts_remove_rare(200) %>%
        wordcounts_texts() %>%
        make_instances(stoplist_file) %>%
        train_model(n_topics=K, n_iters=200,
            threads=1,
            alpha_sum=5,
            beta=0.01,
            n_hyper_iters=20,
            n_burn_in=20,
            n_max_iters=10,
            metadata=meta
        )

    dd <- model_distances(list(ms[[1]], ms[[2]], m_voc), 20)
    expect_true(all(is.finite(
        dfrtopics:::unnest_model_distances(dd)
    )))
})

test_that("clustering two models with high threshold fully aligns them", {
    dd <- model_distances(ms[1:2], 20)
    cl <- align_topics(dd, 10000000)
    expect_equal(sort(cl$clusters[[1]]), 1:K)
    expect_equal(sort(cl$clusters[[2]]), 1:K)

})

test_that("in clustering two models, the closest pair is indeed grouped", {
    dd <- model_distances(ms[1:2], 20)
    cl <- align_topics(dd, 10000000)
    closest <- which(dd$d[[1]][[1]] == min(dd$d[[1]][[1]]), arr.ind=TRUE)
    expect_equal(cl$clusters[[1]][closest[1]], cl$clusters[[2]][closest[2]])
})

test_that("clustering meets up-to-one constraint", {
    cl <- matrix(unlist(align_topics(dst)$clusters), byrow=T, nrow=M)
    expect_equal(dim(cl), c(M, K))
    expect_true(all(1 <= cl & cl <= M * K))
    ck <- gather_matrix(cl, col_names=c("model", "topic", "cluster")) %>%
        group_by(cluster) %>%
        summarize(no_dupes=anyDuplicated(model) == 0)
    expect_true(all(ck$no_dupes))
})

test_that("clustering with default infinite threshold leaves no isolates", {
    cl <- align_topics(dst)$clusters
    # it can leave some non-full clusters, because of the greedy algorithm
    # and the mapping constraint
    expect_true(!(1 %in% tabulate(unlist(cl))))
})

test_that("clustering with low threshold leaves some isolates", {
    cldst <- unlist(align_topics(dst)$distances)
    thresh <- quantile(cldst[cldst > 0], 0.25)
    cl <- align_topics(dst, thresh)
    expect_true(length(unique(unlist(cl$clusters))) > K)
    expect_true(all(unlist(cl$distances) <= thresh))
})

test_that("alignment_frame gives an expected result", {
    cl <- align_topics(dst)
    frm <- alignment_frame(cl)
    expect_equal(colnames(frm),
        c("cluster", "model", "topic", "label", "d"))
    expect_equal(nrow(frm), K * M)
})

test_that("an obvious clustering is found", {
    faked <- structure(
        list(d=list( # triangle inequality, shmiangle inequality
            list(
                matrix(c(100, 1,  # remember these are transposed
                         1, 100), nrow=2),
                matrix(c(1, 100,
                         1, 100), nrow=2)),
             list(
                matrix(c(100, 1,
                         1, 100), nrow=2)))),
        class="model_distances")
    cl <- align_topics(faked)
    expect_equal(cl$clusters, list(1:2, 2:1, 1:2))
})

test_that("a trivial clustering is found", {
    dtriv <- model_distances(ms[c(1, 1, 1)], V)
    cl <- align_topics(dtriv)$clusters
    expect_equal(cl, rep(cl[1], 3))
})

test_that("cluster widths are right", {
    cl <- align_topics(dst)
    wd1 <- widths(cl)
    clmat <- matrix(unlist(cl$clusters), byrow=T, nrow=M)
    # find widths by (even more) brute force
    n_clust <- max(unlist(cl$clusters))
    wd2 <- sapply(1:n_clust, function (clst) {
        members <- which(clmat == clst, arr.ind=T)
        if (nrow(members) <= 1) {
            NA
        } else {
            max(apply(combn(nrow(members), 2), 2, function (p)
                dst[members[p[1], 1], members[p[2], 1],
                    members[p[1], 2], members[p[2], 2]
                ]
            ))
        }
    })
    expect_equal(wd1, wd2)
})

test_that("we can also cluster models with varying K", {
    ks <- 7:10

    msk <- lapply(ks, function (K) train_model(
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
    ))

    dstk <- model_distances(msk, V)
    expect_equal(dim(dstk$d[[1]][[length(ks) - 1]]), ks[c(1, length(ks))])
    cl <- align_topics(dstk)
    expect_equal(sapply(cl$clusters, length), ks)
})

test_that("different topic-word matrix types is okay", {
    msmat <- ms
    # The gremlin comes and converts one Matrix into a matrix
    gremlin <- sample(M, 1)
    msmat[[gremlin]]$topic_words <- as.matrix(topic_words(ms[[gremlin]]))
    expect_true(!is(topic_words(msmat[[gremlin]]), "Matrix"))
    expect_equal(model_distances(msmat, V)$d, dst$d)
})

message("random seed started at: ", s)

