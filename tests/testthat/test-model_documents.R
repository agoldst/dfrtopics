context("Modeling")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE,
        dfrtopics.verbose=F)

data_dir <- file.path(path.package("dfrtopics"),
                      "test-data", "pmla-modphil1905-1915")

# we're just cross-checking functions here, so use an arbitrary
# subset of the sample data

fs <- sample(list.files(file.path(data_dir, "wordcounts"), full.names=T),
             60)

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                           "stoplist.txt")

n_topics <- 12 # topics
n_top_words <- 10 # key words

test_that("Modeling on some sample data works", {
    insts <- read_wordcounts(fs) %>%
        wordcounts_remove_rare(10000) %>%
        wordcounts_texts() %>%
        make_instances(stoplist_file)

    m <- train_model(insts,
                     n_topics=n_topics,
                     n_iters=200,
                     threads=1, 
                     alpha_sum=5,
                     beta=0.01,
                     n_hyper_iters=20,
                     n_burn_in=20,
                     n_max_iters=10)

    expect_is(m, "mallet_model")
    expect_equal(class(RTopicModel(m)), "jobjRef", check.attributes=F)
    expect_equal(
        rJava::.jstrVal(
            rJava::.jcall(RTopicModel(m), "Ljava/lang/Class;", "getClass")
        ),
        "class cc.mallet.topics.RTopicModel")

    # right number of rows in doc-topics?
    expect_that(nrow(doc_topics(m)), equals(length(fs)))

    # right number of cols?
    expect_that(ncol(doc_topics(m)), equals(n_topics))

    keys <- top_words(m, n=n_top_words)
    # right number of key words in keys frame?
    expect_that(nrow(keys), equals(n_topics * n_top_words))

    tw <- topic_words(m)
    expect_that(nrow(tw), equals(n_topics))
    expect_that(ncol(tw), equals(length(vocabulary(m))))

    expect_equal(vocabulary(m)[which.max(tw[1, ])], keys$word[1])

}) 

test_that("Convenience function model_dfr_documents completes", {
    # the modeling run: we'll also check for valid messaging
    options(dfrtopics.verbose=T)
    expect_message(
        (
            m <- model_dfr_documents(
                file.path(data_dir, "citations.tsv"),
                file.path(data_dir, "wordcounts"),
                stoplist_file,
                n_topics=n_topics,
                seed=42
            )
        ),
        "MALLET random number seed set to 42"
    )
    options(dfrtopics.verbose=F)

    # remembered the seed?
    expect_that(modeling_parameters(m)$seed, equals(42))

    # right number of rows in doc-topics?
    expect_that(nrow(doc_topics(m)),
                equals(length(list.files(file.path(data_dir, "wordcounts")))))

    # right number of cols?
    expect_that(ncol(doc_topics(m)), equals(n_topics))
})
