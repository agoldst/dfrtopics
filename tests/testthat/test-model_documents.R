context("Modeling")

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
    
    m <- list()
    m$metadata <- read_metadata(file.path(data_dir, "citations.tsv"))
    insts <- make_instances(read_dfr_wordcounts(files=fs),
                            stoplist_file=stoplist_file)

    m$trainer <- train_model(insts,
                             n_topics=n_topics,
                             n_iters=200,
                             threads=1, 
                             alpha_sum=5,
                             beta=0.01,
                             n_hyper_iters=20,
                             n_burn_in=20,
                             n_max_iters=10)

    expect_equal(class(m$trainer), "jobjRef", check.attributes=F)
    expect_equal(.jstrVal(m$trainer$getClass()),
                 "class cc.mallet.topics.RTopicModel")

    m$wkf <- weighted_keys_frame(m$trainer, n_top_words)
    m$doc_topics <- doc_topics_frame(m$trainer)

    # right number of rows in doc-topics?
    expect_that(nrow(m$doc_topics), equals(length(fs)))

    # right number of cols?
    expect_that(ncol(m$doc_topics), equals(n_topics + 1))

    # right number of key words in keys frame?
    expect_that(nrow(m$wkf), equals(n_topics * n_top_words))


    # check that file output creates all the expected files
    out_dir <- tempdir()
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    out_files <- file.path(out_dir,c(
        "doc_topics.csv",
        "topic_words.csv",
        "vocab.txt",
        "params.csv",
        "keys.csv",
        "mallet_state.gz",
        "diagnostics.xml",
        "id_map.txt",
        "instances.mallet",
        "topic_scaled.csv"))

    clear_files <- function (fs) {
        for(f in fs) {
            if (file.exists(f)) {
                unlink(f)
            }
        }
    }

    clear_files(out_files)

    output_model(m, out_dir, save_instances=T, save_scaled=T)

    expect_that(all(sapply(out_files,file.exists)), is_true())

    clear_files(out_files)

    # check that output_model fills in missing keys and doc_topics

    m$wkf <- NULL
    m$doc_topics <- NULL

    output_model(m, out_dir, save_instances=T, save_scaled=T)

    expect_that(all(sapply(out_files, file.exists)), is_true())

    clear_files(out_files)

}) 

test_that("Convenience function model_documents completes", {
    # the modeling run: we'll also check for valid messaging
    expect_message(
        (
            m <- model_documents(file.path(data_dir, "citations.tsv"),
                                 file.path(data_dir, "wordcounts"),
                                 stoplist_file,
                                 n_topics=n_topics,
                                 seed=42,
                                 n_top_words=n_top_words)
        ),
        "MALLET random number seed set to 42"
    )


    # remembered the seed?
    expect_that(m$seed, equals(42))

    # right number of rows in doc-topics?
    expect_that(nrow(m$doc_topics),
                equals(length(list.files(file.path(data_dir, "wordcounts")))))

    # right number of cols?
    expect_that(ncol(m$doc_topics), equals(n_topics + 1))

    # right number of key words in keys frame?
    expect_that(nrow(m$wkf), equals(n_topics * n_top_words))
})
