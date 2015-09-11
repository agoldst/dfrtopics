context("DfR document loading")

data_dir <- file.path(path.package("dfrtopics"),
                      "test-data", "pmla-modphil1905-1915")

# two short files for testing
fs <- file.path(data_dir, "wordcounts", c(
         "wordcounts_10.2307_432823.CSV",
         "wordcounts_10.2307_433185.CSV"
     ))

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                           "stoplist.txt")

fake_counts <- data_frame(
    id=c(rep("10.2307/123456", 3),
         rep("10.2307/654321", 3)),
    feature=c("the", "woolf", "hull",
              "the", "of", "hull"),
    weight=c(3:1, 5, 5, 1))

dummy <- tempfile()

test_that("Loading wordcounts files works as expected", {
    counts <- read_wordcounts(fs)

    n_feats <- sum(sapply(fs, function (f) length(readLines(f)) - 1))
    expect_equal(colnames(counts), c("id", "feature", "weight"))
    expect_equal(nrow(counts), n_feats)
    expect_equal(n_distinct(counts$id), length(fs))
                 
    expect_equal(counts$feature[1], "the")

    # now add in a header-only file
    writeLines("WORDCOUNTS,WEIGHT", dummy)
    counts <- read_wordcounts(c(fs, dummy))
    expect_equal(nrow(counts), n_feats)
    expect_equal(n_distinct(counts$id), length(fs))
})

test_that("Document-frame generation works as expected", {
    docs <- dfr_docs_frame(fake_counts)
    expect_equal(docs, data_frame(
        id=c("10.2307/123456", "10.2307/654321"),
        text=c("the the the woolf woolf hull",
               "the the the the the of of of of of hull")))
})

test_that("Doc lengths are correctly calculated", {
    lengths <- dfr_doc_lengths(fake_counts)
    expect_equal(lengths$length, c(6, 11))
})

test_that("Feature totals are correctly calculated", {
    lengths <- dfr_feature_totals(fake_counts)
    lengths %>%
        arrange(feature) %>%
        expect_equal(data_frame(
            feature=c("hull", "of", "the", "woolf"),
            weight=c(2, 5, 8, 2)
        ))
})

test_that("Stopword removal works correctly", {
    stopped <- dfr_remove_stopwords(fake_counts, c("the", "of"))
    expect_equal(stopped, data_frame(
        id=c(rep("10.2307/123456", 2), "10.2307/654321"),
        feature=c("woolf", "hull", "hull"),
        weight=c(2, 1, 1)))
})

test_that("Frequency filtering works correctly", {
    shorter <- dfr_remove_rare(fake_counts, 2)
    expect_equal(shorter, data_frame(
        id=c("10.2307/123456", rep("10.2307/654321", 2)),
        feature=c("the", "the", "of"),
        weight=c(3, 5, 5)))
})

test_that("Loading a big folder of files completes", {
    ff <- list.files(file.path(data_dir, "wordcounts"), full.names=T)
    counts <- read_wordcounts(ff)
    expect_equal(n_distinct(counts$id), length(ff))
})

if (file.exists(dummy)) {
    unlink(dummy)
}
