context("Model inspection with topic info functions")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE)

# construct a dummy trial model. We're not going for quality here,
# since this is just to check that all the formatters and extractors
# behave right

data_dir <- file.path(path.package("dfrtopics"),
                      "test-data", "pmla-modphil1905-1915")
fs <- list.files(file.path(data_dir, "wordcounts"),
                 full.names=T)[1:60]
stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

insts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(10000) %>%
    wordcounts_texts() %>%
    make_instances(stoplist_file)

n_topics <- 8
m <- train_model(insts,
                 n_topics=n_topics,
                 n_iters=200,
                 threads=1,
                 alpha_sum=5,
                 beta=0.01,
                 n_hyper_iters=20,
                 n_burn_in=20,
                 seed=42,
                 n_max_iters=10)
metadata(m) <- read_dfr_metadata(file.path(data_dir, "citations.tsv"))

test_that("Metadata extraction gives a frame of the right nrow",
    expect_equal(nrow(metadata(m)), length(fs))
)

test_that("Printing model objects gives appropriate output", {
    expect_output(print(m), "MALLET")
    expect_output(print(summary(m)), "model object.*yes")
})

test_that("Top-words extraction is as expected", {
    keys <- top_words(m, 10)
    expect_equal(keys$word[11:13],
                c("english", "story", "first"))
})

test_that("Asking for > V top words gives V top words", {
    V <- length(vocabulary(m))
    expect_equal(top_words(m, V), top_words(m, 2 * V))
})

test_that("Topic labels appear as expected", {
    tlabs <- topic_labels(m, n=3)
    expect_equal(tlabs[5],
                "5 author text jusserand")
})

test_that("Topic docs are returned correctly", {
    tdocs <- top_docs(m, n=3, identity)

    # topic 5
    expect_equal(doc_ids(m)[tdocs$doc[13:15]],
                 c("10.2307/432464", "10.2307/432470", "10.2307/3693731"))

    # check identity weighting
    expect_equal(tdocs$weight[13:15],
                c(8560, 8150, 3267))
})

test_that("Top topics for docs are returned correctly", {

    ttopics <- docs_top_topics(m, n=2, weighting=identity)
    d <- match("10.2307/432389", doc_ids(m))
    tt_check <- ttopics[ttopics$doc == d, ]
    expect_equal(tt_check$topic, c(1, 5))
    expect_equal(tt_check$weight, c(1583, 625))
})

test_that("Top topics for words are returned correctly", {
    wtopics <- words_top_topics(m, n=3, identity)
    expect_equal(wtopics$topic[wtopics$word == "latin"],
                 c(4, 3, 1), info="identity weighting")
    wtopics <- words_top_topics(m, n=3)
    expect_equal(wtopics$topic[wtopics$word == "latin"],
                 c(3, 4, 1), info="normalized weighting")
})
