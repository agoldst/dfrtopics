test_that("Instances are made as we expect", {
    
    data_dir <- file.path(path.package("dfrtopics"), "test-data",
                          "pmla-modphil1905-1915")

    # check for presence of sample data

    expect_that(file.exists(data_dir), is_true())
    expect_that(file.exists(file.path(data_dir, "citations.tsv")),
                is_true())

    metadata <- read_metadata(file.path(data_dir, "citations.tsv"))

    expect_that(length(list.files(file.path(data_dir, "wordcounts"))),
                equals(nrow(metadata)))

    counts <- read_dfr(dirs=file.path(data_dir, "wordcounts"))
    texts <- docs_frame(counts)

    stop_f <- file.path(path.package("dfrtopics"), "stoplist", "stoplist.txt")
    instances <- make_instances(texts, stoplist_file=stop_f)

    # check instance ids against metadata ids (not in same order)
    expect_that(all(instances_ids(instances) %in% metadata$id),
                is_true())

    expect_that(instances$size(), equals(nrow(metadata)))
    vocab <- instances_vocabulary(instances)

    # cross-check vocabulary
    stopwords <- readLines(stop_f)
    vocab2 <- setdiff(unique(counts$WORDCOUNTS), stopwords)

    expect_that(sort(vocab), equals(sort(vocab2)))

})
