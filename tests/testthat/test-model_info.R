test_that("Topic info functions work as expected", {

    # construct a dummy trial model. We're not going for quality here,
    # since this is just to check that all the formatters and extractors
    # behave right

    data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

    # check for presence of sample data

    expect_that(file.exists(data_dir), is_true())
    expect_that(file.exists(file.path(data_dir, "citations.tsv")),
                is_true())

    fs <- list.files(file.path(data_dir, "wordcounts"),
                     full.names=T)[1:60]
    m <- list()
    m$metadata <- read_metadata(file.path(data_dir, "citations.tsv"))
    insts <-make_instances(read_dfr_wordcounts(files=fs),
                           stoplist_file=file.path(path.package("dfrtopics"),
                                                   "stoplist", "stoplist.txt"))
    m$trainer <- train_model(insts,
                             n_topics=8,
                             n_iters=200,
                             seed=42,
                             threads=1, 
                             alpha_sum=5, beta=0.01, n_hyper_iters=20,
                             n_burn_in=20,
                             n_max_iters=10)
    m$wkf <- weighted_keys_frame(m$trainer)
    m$doc_topics <- doc_topics_frame(m$trainer)

    # check labeling functions on result
    # comes as a named vector, so strip names
    expect_that(as.character(topic_top_words(m$wkf, topic=2, n=3)),
                equals(c("poetry", "latin", "literature")))

    name_m <- topic_top_words(m$wkf, topic=3:4, n=3)

    # matrix unrolled by column
    expect_that(as.character(name_m),
                equals(c("mas", "iou", "dixo", "uous", "latin", "quest")))
                         
    expect_that(rownames(name_m),
                equals(c("3", "4")))

    expect_that(topic_name(m$wkf, topic=5, n=3),
                equals("005 revenge nr italian"))

    expect_that(topic_name(m$wkf, topic=c(5, 7), n=3),
                equals(c("005 revenge nr italian",
                         "007 vv marriage players")))

    expect_that(topic_name(m$wkf, topic=c(5, 7), fmt="%d: %s", n=3),
                equals(c("5: revenge nr italian",
                         "7: vv marriage players")))

    label <- topic_labeller(m$wkf, fmt="T%d %s", n=3)
    expect_that(label(2), equals("T2 poetry latin literature"))

    # check top docs

    docs5 <- top_documents(topic=5,
                           doc_topics=doc_topics_matrix(m$doc_topics),
                           id_map=m$doc_topics$id,
                           n=3)

    expect_that(as.character(docs5$id),
                equals(c("10.2307/432389", "10.2307/3693732", "10.2307/432452")))
    expect_that(docs5$weight,
                equals(c(2654, 1581, 1565)))

    # check top topics

    toptops <- top_topics("10.2307/432389",
                           doc_topics=doc_topics_matrix(m$doc_topics),
                           id_map=m$doc_topics$id, n=2)
    expect_that(toptops$topic, equals(c(5, 8)))
    expect_that(toptops$weight, equals(c(2654, 1102)))

})
