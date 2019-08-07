context("MALLET instance lists")

library(dplyr) # for testing

fake_docs <- data_frame(
    id=c("10.2307/123456", "10.2307/654321"),
    text=c("the the the the a a a shall i compare thee to a summers day",
           "music to hear why playst thou music softly"))

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist", "stoplist.txt")

test_that("Instances are successfully constructed", {
    il <- make_instances(fake_docs)
    expect_equal(il$size(), 2)
    expect_equal(instances_ids(il), fake_docs$id)
    expect_equal(instance_text(il$get(0L)), fake_docs$text[1])
})

test_that("Instance stopwording works as expected", {
    il <- make_instances(fake_docs, stoplist_file)
    expect_equal(instance_text(il$get(0L)), "compare summers day")
})

test_that("Instance vocabulary extraction works", {
    il <- make_instances(fake_docs)
    v1 <- instances_vocabulary(il)
    v2 <- unique(unlist(strsplit(fake_docs$text, " ")))
    expect_equal(sort(v1), sort(v2))
})

test_that("Instance length calculations are correct", {
    il <- make_instances(fake_docs)
    expect_equal(instances_lengths(il), c(15, 8))
})

test_that("Instance TDM generation works as expected", {
    il <- make_instances(fake_docs)
    m <- instances_Matrix(il)
    expect_equal(dim(m), c(15, 2))
    word <- match("music", instances_vocabulary(il))
    expect_equal(m[word, 2], 2)
    expect_equal(Matrix::colSums(m), instances_lengths(il))
})


test_that("Instances are made from DfR data successfully", {
    
    data_dir <- file.path(path.package("dfrtopics"), "test-data",
                          "pmla-modphil1905-1915")

    # check for presence of sample data

    expect_true(file.exists(data_dir))
    expect_true(file.exists(file.path(data_dir, "citations.tsv")))

    metadata <- read_dfr_metadata(file.path(data_dir, "citations.tsv"))

    expect_that(length(list.files(file.path(data_dir, "wordcounts"))),
                equals(nrow(metadata)))

    counts <- read_wordcounts(Sys.glob(file.path(data_dir,
                                                 "wordcounts", "*.CSV")))
    texts <- wordcounts_texts(counts)

    stop_f <- file.path(path.package("dfrtopics"), "stoplist", "stoplist.txt")
    instances <- make_instances(texts, stoplist_file=stop_f)

    # check instance ids against metadata ids (not in same order)
    expect_true(all(instances_ids(instances) %in% metadata$id))

    expect_that(instances$size(), equals(nrow(metadata)))
    vocab <- instances_vocabulary(instances)

    # cross-check vocabulary
    stopwords <- readLines(stop_f)
    vocab2 <- setdiff(unique(counts$word), stopwords)

    expect_that(sort(vocab), equals(sort(vocab2)))
})

test_that("wordcounts_instances behaves correctly", {
    fake_counts <- data_frame(
        id=rep(as.character(1:2), each=3),
        word=c("a", "a,b", "q's", "A", "b", "fnord"),
        weight=rep(3:1, 2)
    )

    il <- wordcounts_instances(fake_counts)

    expect_equal(il$size(), n_distinct(fake_counts$id))
    expect_equal(sort(fake_counts$word), sort(instances_vocabulary(il)))
    expect_equal(instance_text(il$get(0L)),
                 "a a a a,b a,b q's")
    expect_equal(instance_text(il$get(1L)),
                 "A A A b b fnord")
})


