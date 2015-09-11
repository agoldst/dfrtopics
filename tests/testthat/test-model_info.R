test_that("Topic info functions work as expected", {

    # construct a dummy trial model. We're not going for quality here,
    # since this is just to check that all the formatters and extractors
    # behave right

    data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")
    fs <- list.files(file.path(data_dir, "wordcounts"),
                     full.names=T)[1:60]
    stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                              "stoplist.txt"))

    insts <- read_wordcounts(fs) %>%
        dfr_docs_frame() %>%
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
                     n_max_iters=10))
    metadata(m) <- read_metadata(file.path(data_dir, "citations.tsv"))

    expect_equal(nrow(metadata(m)), length(fs))


    # check labeling functions on result
    # comes as a named vector, so strip names
    keys <- top_words(m, 10)
    expect_equal(keys$word[21:23],
                c("poetry", "latin", "literature"))

    name_m <- topic_top_words(m$wkf, topic=3:4, n=3)

    tlabs <- topic_labels(n=3)
    expect_equal(tlabs[5],
                "5 revenge nr italian")

    # check top docs

    tdocs <- top_docs(m, n=3)

    # topic 5
    expect_equal(doc_ids(m)[tdocs$doc[16:18]]
                c("10.2307/432389", "10.2307/3693732", "10.2307/432452"))

    # check identity weighting
    tdocs_raw <- top_docs(m, n=3, identity)
    expect_equal(tdocs_raw$weight[16:18],
                c(2654, 1581, 1565))

    # check top topics

    ttopics <- docs_top_topics(m, n=2, weighting=identity)
    tt_check <- ttopics[doc_ids(m) == "10.2307/432389", ]
    expect_equal(tt_check$topic, c(5, 8))
    expect_equal(tt_check$weight, c(2654, 1102))

})
