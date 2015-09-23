context("DfR document loading")

library(dplyr)
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
    word=c("the", "woolf", "hull",
              "the", "of", "hull"),
    weight=as.integer(c(3:1, 5, 5, 1)))

test_that("Loading dummy wordcounts files works as expected", {
    dummy_dir <- file.path(tempdir(), "wordcounts")
    if (!dir.exists(dummy_dir)) dir.create(dummy_dir)

    writeLines(
"WORDCOUNTS,WEIGHT
the,3
woolf,2
hull,1",
        file.path(dummy_dir, "wordcounts_10.2307_123456.CSV")
    )
    writeLines(
"WORDCOUNTS,WEIGHT
the,5
of,5
hull,1",
        file.path(dummy_dir, "wordcounts_10.2307_654321.CSV")
    )
    dummy_ws <- file.path(dummy_dir, c("wordcounts_10.2307_123456.CSV",
                                       "wordcounts_10.2307_654321.CSV"))
    expect_equal(read_wordcounts(dummy_ws), fake_counts)

    # now add in a header-only file
    writeLines("WORDCOUNTS,WEIGHT",
               file.path(dummy_dir, "wordcounts_10.2307_666.CSV"))

    # and check both file-reading methods
    expect_equal(
        read_wordcounts(dummy_ws, reader=dfrtopics:::read_wordcounts_base),
        fake_counts)

    expect_equal(
        read_wordcounts(dummy_ws, reader=dfrtopics:::read_wordcounts_readr),
        fake_counts)

    unlink(dummy_ws)
    unlink(dummy_dir)
})

test_that("A custom reading method works", {
    dummy_dir <- file.path(tempdir(), "wordcounts")
    if (!dir.exists(dummy_dir)) dir.create(dummy_dir)
    writeLines(
"zorf\tnargle
the\t3
woolf\t2
hull\t1",
        file.path(dummy_dir, "wordcounts_10.2307_123456.tsv")
    )
    writeLines(
"zorf\tnargle
the\t5
of\t5
hull\t1",
        file.path(dummy_dir, "wordcounts_10.2307_654321.tsv")
    )

    dummy_ws <- file.path(dummy_dir, c("wordcounts_10.2307_123456.tsv",
                                       "wordcounts_10.2307_654321.tsv"))
    ids <- basename(dummy_ws) %>%
        stringr::str_replace_all("(wordcounts_|\\.tsv)", "") %>%
        stringr::str_replace_all("_", "/")

    expect_equal(
        read_wordcounts(dummy_ws, ids, readr::read_tsv),
        fake_counts
    )
    unlink(dummy_ws)
    unlink(dummy_dir)
})

test_that("Loading real wordcounts files works as expected", {
    counts <- read_wordcounts(fs)

    n_feats <- sum(sapply(fs, function (f) length(readLines(f)) - 1))
    expect_equal(colnames(counts), c("id", "word", "weight"))
    expect_equal(nrow(counts), n_feats)
    expect_equal(n_distinct(counts$id), length(fs))

    expect_equal(counts$word[1], "the")
})

test_that("Document-frame generation works as expected", {
    docs <- wordcounts_texts(fake_counts)
    expect_equal(docs, data_frame(
        id=c("10.2307/123456", "10.2307/654321"),
        text=c("the the the woolf woolf hull",
               "the the the the the of of of of of hull")))
})

test_that("Doc lengths are correctly calculated", {
    lengths <- wordcounts_doc_lengths(fake_counts)
    expect_equal(lengths$length, c(6, 11))
})

test_that("word totals are correctly calculated", {
    lengths <- wordcounts_word_totals(fake_counts)
    lengths %>%
        arrange(word) %>%
        expect_equal(data_frame(
            word=c("hull", "of", "the", "woolf"),
            weight=as.integer(c(2, 5, 8, 2))
        ))
})

test_that("Stopword removal works correctly", {
    stopped <- wordcounts_remove_stopwords(fake_counts, c("the", "of"))
    expect_equal(stopped, data_frame(
        id=c(rep("10.2307/123456", 2), "10.2307/654321"),
        word=c("woolf", "hull", "hull"),
        weight=c(2L, 1L, 1L)))
})

test_that("Frequency filtering works correctly", {
    shorter <- wordcounts_remove_rare(fake_counts, 2)
    expect_equal(shorter, data_frame(
        id=c("10.2307/123456", rep("10.2307/654321", 2)),
        word=c("the", "the", "of"),
        weight=c(3L, 5L, 5L)))
})

test_that("Loading a big folder of files completes", {
    ff <- list.files(file.path(data_dir, "wordcounts"), full.names=T)
    counts <- read_wordcounts(ff)
    expect_equal(n_distinct(counts$id), length(ff))
})
