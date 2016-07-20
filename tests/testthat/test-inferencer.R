context("Topic inference support")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE)

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

model_fs <- list.files(file.path(data_dir, "wordcounts"),
                       full.names=T)[61:120]

heldout_fs <- list.files(file.path(data_dir, "wordcounts"),
                       full.names=T)[1:40]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

model_il <- read_wordcounts(model_fs) %>%
    wordcounts_remove_rare(10000) %>%
    wordcounts_texts() %>%
    make_instances(stoplist_file)

heldout_il <- read_wordcounts(heldout_fs) %>%
    wordcounts_texts() %>%
    compatible_instances(model_il)

m <- train_model(model_il,
                 n_topics=8)

m_heldout <- infer_topics(m, heldout_il, seed=42)

test_that("read/write inferencer produces a file", {
    inf <- inferencer(m)
    f <- tempfile()
    write_inferencer(inf, f)
    expect_true(file.exists(f))
    expect_gt(file.size(f), 0)

    inf <- read_inferencer(f)
    expect_is(inf, "jobjRef")
})

test_that("compatible instances can be made", {

    expect_is(heldout_il, "jobjRef")
    expect_equal(instances_ids(heldout_il), dfr_filename_id(heldout_fs))
    expect_equal(instances_vocabulary(model_il),
                 instances_vocabulary(heldout_il))

})

test_that("new topics can be inferred", {

    expect_is(m_heldout, "mallet_model_inferred")
    expect_is(m_heldout, "mallet_model")

    expect_equal(modeling_parameters(m_heldout)$seed, 42)
    expect_output(print(m_heldout), "inferred")
    expect_output(print(summary(m_heldout)), "inferred")

    dtm <- doc_topics(m_heldout)

    expect_equal(dim(dtm), c(length(heldout_fs), n_topics(m)))
    expect_equal(rowSums(dtm), instances_lengths(heldout_il))

    dt_top <- docs_top_topics(m_heldout, n=3)
    expect_equal(nrow(dt_top), 3 * length(heldout_fs))

    topd <- top_docs(m_heldout, n=3)
    expect_equal(nrow(topd), 3 * n_topics(m))
})

test_that("merging the results is possible", {
    m_joint <- merge(m, m_heldout)
    expect_is(m_joint, "mallet_model")
    expect_is(m_joint, "mallet_model_merged")

    expect_equal(n_topics(m_joint), n_topics(m))
    dtm <- doc_topics(m_joint)
    expect_equal(dim(dtm), c(length(model_fs) + length(heldout_fs),
                             n_topics(m)))
    expect_equal(doc_ids(m_joint), dfr_filename_id(c(model_fs, heldout_fs)))

    merge(m, m_heldout,
          weighting_dtx=dt_smooth_normalize(m),
          weighting_dty=dt_normalize(m_heldout)) %>%
        doc_topics() %>%
        rowSums() %>% 
        expect_equal(rep(1, length(heldout_fs) + length(model_fs)))
})
