context("writing/loading model output files")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE,
        dfrtopics.verbose=F)

out_dir <- file.path(tempdir(), "test-loading")
if (!file.exists(out_dir)) {
    dir.create(out_dir, recursive=TRUE)
}

out_files <- file.path(out_dir,c(
    "doc_topics.csv",
    "doc_ids.txt",
    "vocabulary.txt",
    "top_words.csv",
    "topic_words.csv",
    "params.txt",
    "mallet_state.gz",
    "state.csv",
    "diagnostics.xml",
    "instances.mallet",
    "topic_scaled.csv"))

clear_files <- function (fs) {
    for (f in fs) {
        if (file.exists(f)) unlink(f)
    }
}
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


test_that("saving a model generates appropriate files", {

    clear_files(out_files)

    write_mallet_model(m, out_dir, save_instances=T, save_scaled=T)

    for (f in out_files) {
        expect_true(file.exists(f),
                    info=paste("Check creation of ", f))
    }

    clear_files(out_files)
})

test_that("loading saved model files reproduces the model", {
    write_mallet_model(m, out_dir)

    m2 <- load_mallet_model(
        doc_topics_file=out_files[1],
        doc_ids_file=out_files[2],
        vocab_file=out_files[3],
        top_words_file=out_files[4],
        topic_words_file=out_files[5],
        params_file=out_files[6],
        metadata_file=file.path(data_dir, "citations.tsv")
    )

    expect_is(m2, "mallet_model")

    expect_equal(doc_topics(m), doc_topics(m2))
    expect_equal(doc_ids(m), doc_ids(m2))
    expect_equal(vocabulary(m), vocabulary(m2))
    expect_equal(as.data.frame(top_words(m, 50)),
                 as.data.frame(top_words(m2)), check.attributes=F)
    expect_equal(topic_words(m)[4, 95], topic_words(m)[4, 95])
    expect_equal(hyperparameters(m), hyperparameters(m2))
    expect_equal(modeling_parameters(m), modeling_parameters(m2))
    expect_equal(as.list(metadata(m)), as.list(metadata(m2)))

    clear_files(out_files)
})

test_that("loading from a directory reproduces the model", {
    write_mallet_model(m, out_dir)

    m2 <- load_mallet_model_directory(out_dir,
        metadata_file=file.path(data_dir, "citations.tsv"))

    expect_is(m2, "mallet_model")

    expect_equal(doc_topics(m), doc_topics(m2))
    expect_equal(doc_ids(m), doc_ids(m2))
    expect_equal(vocabulary(m), vocabulary(m2))
    expect_equal(as.data.frame(top_words(m, 50)),
                 as.data.frame(top_words(m2)), check.attributes=F)
    expect_equal(topic_words(m)[5, 93], topic_words(m)[5, 93])
    expect_equal(hyperparameters(m), hyperparameters(m2))
    expect_equal(modeling_parameters(m), modeling_parameters(m2))
    expect_equal(as.list(metadata(m)), as.list(metadata(m2)))

    clear_files(out_files)
})

test_that("loading from a sampling state reproduces the model", {
    il_file <- tempfile()
    write_instances(insts, il_file)
    ms_file <- file.path(out_dir, "mallet_state.gz")
    write_mallet_state(m, ms_file)
    m2 <- load_from_mallet_state(ms_file,
                                 instances_file=il_file)

    clear_files(il_file)
    expect_is(m2, "mallet_model")
    expect_equal(doc_topics(m), doc_topics(m2))
    expect_equal(doc_ids(m), doc_ids(m2))
    expect_equal(vocabulary(m), vocabulary(m2))
    expect_equal(as.data.frame(top_words(m, 50)),
                 as.data.frame(top_words(m2, 50)), check.attributes=F)

    expect_equal(topic_words(m)[5, 93], topic_words(m2)[5, 93])
    expect_equal(hyperparameters(m), hyperparameters(m2))

    clear_files(out_files)
})

test_that("loading sampling state w/o bigmemory reproduces the model", {
    il_file <- tempfile()
    write_instances(insts, il_file)
    ms_file <- file.path(out_dir, "mallet_state.gz")
    write_mallet_state(m, ms_file)
    m2 <- load_from_mallet_state(ms_file,
                                 instances_file=il_file, bigmemory=FALSE)

    clear_files(il_file)
    expect_is(m2, "mallet_model")
    expect_equal(doc_topics(m), doc_topics(m2))
    expect_equal(doc_ids(m), doc_ids(m2))
    expect_equal(vocabulary(m), vocabulary(m2))
    expect_equal(as.data.frame(top_words(m, 50)),
                 as.data.frame(top_words(m2, 50)), check.attributes=F)

    expect_equal(topic_words(m)[5, 93], topic_words(m2)[5, 93])
    expect_equal(hyperparameters(m), hyperparameters(m2))

    clear_files(out_files)
})

