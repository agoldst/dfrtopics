context("Sampling state")
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE,
        dfrtopics.verbose=F)

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
    wordcounts_remove_rare(200) %>%
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
    seed=42,
    n_max_iters=10,
    metadata=read_dfr_metadata(file.path(data_dir, "citations.tsv"))
)


out_dir <- tempdir()
if (!file.exists(out_dir)) {
    dir.create(out_dir)
}

state_file <- file.path(out_dir, "state.gz")

ss_file <- file.path(out_dir, "state_simple.csv")
ss2 <- file.path(out_dir, "state_simple.csv")
state_unzip <- file.path(out_dir, "state")
fs <- c(state_file, ss_file, ss2, state_unzip)

for (f in fs) {
    if (file.exists(f)) unlink(f)
}

test_that("simplify_state generates the right file", {

    write_mallet_state(m, state_file) 
    expect_true(file.exists(state_file),
                info="write_mallet_state() saves a file")

    simplify_state(state_file, ss_file)

    expect_true(file.exists(ss_file),
                info="simplify_state() saves a file")

    system2("gunzip", c("-k", state_file))
    state_plain <- file(state_unzip)
    simplify_state(state_plain, ss2)

    ss_frame <- read.csv(ss_file, as.is=T)
    expect_equal(ss_frame, read.csv(ss2, as.is=T),
                 info="simplify_state on connection also works")

    # test for inconvenient edge cases in simplify_state
    simplify_state(state_file, ss2, chunk_size=1L)
    expect_equal(ss_frame, read.csv(ss2, as.is=T),
                 info="simplify_state one line at a time works")

    expect_equal(sum(ss_frame$count),
                 sum(doc_topics(m)),
                 info="total token counts same in doc topics and ss")

    # zero-based ss indices, 1-based doc_topics indices
    expect_equal(sum(ss_frame$count[ss_frame$topic == 1]),
                 sum(doc_topics(m)[ , 2]),
                 info="topic 1 sum in ss_frame = topic 2 sum in doc tops")
})

test_that("sampling state manipulation make sense", {
    skip_if_not_installed("bigtabulate")
    library(bigtabulate)

    m <- load_sampling_state(m, ss_file)
    ss_frame <- read.csv(ss_file, as.is=T)
    ss <- sampling_state(m)
    expect_equal(dim(ss), dim(ss_frame),
                 info="big.matrix ss same dims as ss_frame")

    expect_equal(sum(ss_frame$count[ss_frame$topic == 1]),
                 sum(ss[mwhich(ss, "topic", 2, "eq"), "count"]),
                 info="topic 1 sum in ss_frame = topic 2 sum in ss")

    tdms <- vector("list", n_topics)
    for (topic in seq(n_topics)) {
        mat <- tdm_topic(m, topic)
        expect_equal(dim(mat),
                     c(length(vocabulary(m)), n_docs(m)),
                     info=paste("Dimensions of tdm for topic", topic))

        expect_equal(Matrix::colSums(mat), doc_topics(m)[ , topic],
                     info=paste(
"col sums for topic", topic, "match column of doc-topic matrix"
                     )
        )
        tdms[[topic]] <- mat
    }

    tdm <- Reduce(`+`, tdms)

    # check that topic tdm matrices total to the full term-doc matrix
    expect_equal(tdm, instances_Matrix(instances(m)))
})

for (f in fs) {
    if (file.exists(f)) unlink(f)
}
