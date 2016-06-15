context("time series utilities")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE)

library("dplyr") # for testing

test_that("word_series_matrix works as advertised", {
    tdm <- matrix(c(1, 3, 4,  5, 2,
                    2, 2, 12, 0, 3), nrow=2, byrow=T)
    dates <- as.Date(c(rep("1925-01-01", 2), "1927-01-01",
                       rep("1928-01-01", 2)))

    expect_equal(as.matrix(word_series_matrix(tdm, dates)),
        matrix(c(0.5, 0, 0.25, 0.7,
                 0.5, 0, 0.75, 0.3),
            nrow=2, byrow=T, dimnames=list(NULL,
            c("1925-01-01", "1926-01-01", "1927-01-01", "1928-01-01"))
        )
    )
})

test_that("topic_series yields the expected result", {

    data_dir <- file.path(path.package("dfrtopics"), "test-data",
                          "pmla-modphil1905-1915")

    # run model

    n_topics <- 8 # topics
    m <- model_dfr_documents(
        citations_files=file.path(data_dir, "citations.tsv"),
        wordcounts_dirs=file.path(data_dir, "wordcounts"),
        stoplist_file=file.path(path.package("dfrtopics"),
                                "stoplist", "stoplist.txt"),
        n_topics=n_topics,
        n_iters=140,
        seed=42,
        threads=1L, 
        alpha_sum=5,beta=0.01,
        n_hyper_iters=20,
        n_burn_in=20,
        n_max_iters=10)

    s <- topic_series(m)
    pubyears <- strftime(metadata(m)$pubdate, "%Y")
    expect_equal(colnames(s), c("topic", "pubdate", "weight"))
    expect_equal(nrow(s), n_topics * n_distinct(pubyears))
    s %>% group_by(pubdate) %>%
        summarize(check=sum(weight)) %>%
        select(check) %>% unlist() %>% unname() %>%
        expect_equal(rep(1, 11),
                    info="years are indeed normalized to 1")

    # manually noted the ranking of topics in 1905 from the doc topics matrix
    # which should correspond to what we get out of the data frame
    s %>% filter(pubdate == "1905-01-01") %>%
        arrange(weight) %>%
        select(topic) %>%
        as.data.frame() %>%
        expect_equal(data.frame(topic=c(8, 7, 4, 6, 3, 2, 1, 5)))

})
