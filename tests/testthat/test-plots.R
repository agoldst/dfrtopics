context("Plot utilities")    

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"),
                 full.names=T)[61:120]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")
# run a scrap model: we're just doing manipulation here, not worrying
# about quality

n_topics <- 8
insts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(10000) %>%
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
    metadata=read_dfr_metadata(file.path(data_dir, "citations.tsv"))
)

test_that("plotting functions generate plots", {

    top_words(m, n=10) %>%
        plot_top_words(topic=2) %>%
        expect_is("ggplot")

    topic_series(m) %>%
        plot_series(labels=topic_labels(m, 4)) %>%
        expect_is("ggplot")

    topic_scaled_2d(m) %>%
        plot_topic_scaled() %>%
        expect_is("ggplot")

    m <- load_sampling_state(m)
    plot_word_topic_series(m, "first") %>%
        expect_is("ggplot")
})

test_that("topic_report generates files", {
    out_dir <- file.path(tempdir(), "report")
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive=T)
    }
    topic_report(m, out_dir)

    plot_fs <- paste0("00", 1:n_topics, ".png")
    expect_equal(list.files(out_dir), plot_fs)
    expect_true(all(file.size(list.files(out_dir, full.names=T)) > 0))

    for (f in list.files(out_dir)) {
        unlink(file.path(out_dir, f))
    }
    unlink(out_dir)
})

