context("Using models from other packages")

library(dplyr)

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"), full.names=T)[1:60]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

counts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(200) %>%
    wordcounts_remove_stopwords(readLines(stoplist_file))

insts <- wordcounts_instances(counts)

V <- length(instances_vocabulary(insts))

meta <- read_dfr_metadata(file.path(data_dir, "citations.tsv"))

test_that("Conversion to stm corpus is valid", {
    corp <- wordcounts_stm_inputs(counts, meta)
    expect_equal(names(corp), c("documents", "vocab", "data"))
    expect_equal(length(corp$documents), length(fs))
    expect_equal(sort(corp$vocab), sort(instances_vocabulary(insts)))

    for (d in seq_along(fs)) {
        id <- unique(counts$id)[d]
        doc <- counts$weight[counts$id == id]
        names(doc) <- counts$word[counts$id == id]
        expect_equal(corp$documents[[d]][2, ],
            doc[corp$vocab[corp$documents[[d]][1, ]]],
            check.attributes=F,
            info=paste("Document", d, "wordcounts match", collapse=" "))
        expect_equal(corp$data[d, ], meta[meta$id == id, ],
            check.attributes=F,
            info=paste("Document", d, "metadata matches", collapse=" "))
    }
})

test_that("Conversion to DocumentTermMatrix is valid", {
    skip_if_not_installed("tm")
    dtm <- wordcounts_DocumentTermMatrix(counts)
    expect_is(dtm, "DocumentTermMatrix")
    expect_equal(dim(dtm), c(length(fs), V))
    expect_equal(setdiff(rownames(dtm), counts$id), character(0))
    expect_equal(setdiff(colnames(dtm), counts$word), character(0))
    d <- sample(counts$id, 1)
    expect_equal(as.matrix(dtm[d, counts$word[counts$id == d]]),
                 matrix(counts$weight[counts$id == d], nrow=1),
                 check.attributes=F)
})

