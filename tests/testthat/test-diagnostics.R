context("MALLET diagnostics")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE,
        dfrtopics.verbose=F)

# run a scrap model: we're just doing manipulation here, not worrying
# about quality

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"),
                 full.names=T)[61:120]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

n_topics <- 8

test_that("diagnostic read/write works", {
    
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

    f <- tempfile()
    n_top <- 25
    write_diagnostics(m, f, n_top)
    expect_true(file.exists(f))

    d <- read_diagnostics(f)

    expect_equal(names(d), c("topics", "words"))
    expect_equal(d$topics$topic, 1:n_topics)
    expect_equal(nrow(d$words), n_topics * n_top)
    unlink(f)

})

