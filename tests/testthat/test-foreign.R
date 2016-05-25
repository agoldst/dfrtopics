context("Using models from other packages")

clear_files <- function (fs, ...) {
    for (f in fs) {
        if (file.exists(f)) {
            unlink(f, ...)
        }
    }
}

library(dplyr)

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- list.files(file.path(data_dir, "wordcounts"), full.names=T)[361:420]

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

counts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(200) %>%
    wordcounts_remove_stopwords(readLines(stoplist_file))

K <- 8
V <- n_distinct(counts$word)

meta <- read_dfr_metadata(file.path(data_dir, "citations.tsv"))

test_that("Conversion to stm corpus is valid", {
    corp <- wordcounts_stm_inputs(counts, meta)
    expect_equal(names(corp), c("documents", "vocab", "data"))
    expect_equal(length(corp$documents), length(fs))
    expect_equal(sort(corp$vocab), sort(unique(counts$word)))

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

test_that("Conversion from TopicModel works okay", {
    skip_if_not_installed("topicmodels")
    library(topicmodels)
    dtm <- wordcounts_DocumentTermMatrix(counts)
    lda <- LDA(dtm, k=K, control=list(alpha=0.1))
    ldag <- foreign_model(lda, meta)
    expect_is(ldag, "TopicModel_glue")

    expect_output(print(ldag), "A dfrtopics wrapper", all=F)
    expect_output(print(ldag), "A LDA_VEM topic model with", all=F)

    expect_equal(n_topics(ldag), K)
    expect_equal(terms(lda, 4),
                 matrix(top_words(ldag, 4)$word, ncol=K),
                 check.attributes=F)
    expect_equal(topics(lda, 4),
                 matrix(docs_top_topics(ldag, 4)$topic, ncol=length(fs)),
                 check.attributes=F)
    expect_equal(rowSums(doc_topics(ldag)), rep(1, length(fs)),
                 check.attributes=F)
    expect_equal(rowSums(topic_words(ldag)), rep(1, K),
                 check.attributes=F)
    k <- sample(K, 1)
    expect_equal(topic_labels(ldag, 4)[k],
                 paste(k, paste(terms(lda, 4)[ , k], collapse=" ")))
    srs <- topic_series(ldag)
    date_range <- range(meta$pubdate[meta$id %in% doc_ids(ldag)])
    expect_equal(sort(unique(srs$pubdate)),
        seq(date_range[1], date_range[2], by="years")
    )

    brow <- getOption("browser")
    options(browser=function (x) message("browser: ", x))

    bout <- tempfile("ldag-dfb")
    expect_message(dfr_browser(ldag, out_dir=bout),
        paste0("browser: file://",
               normalizePath(file.path(bout, "index.html"))))

    clear_files(bout, recursive=T) 
    options(browser=brow)
})

test_that("Conversion from stm works okay", {
    skip_if_not_installed("stm")
    library(stm)
    corp <- wordcounts_stm_inputs(counts, meta)
    corp$data$journaltitle <- factor(corp$data$journaltitle)
    st <- stm(corp$documents, corp$vocab, K=K,
              prevalence = ~ journaltitle,
              data=corp$data,
              max.em.its=40,
              verbose=F)
    stg <- foreign_model(st, corp$data)
    expect_is(stg, "TopicModel_glue")
    expect_is(stg, "stm_glue")

    expect_output(print(stg), "A dfrtopics wrapper", all=F)
    expect_output(print(stg), "A topic model with", all=F)
    expect_output(print(summary(stg)), "A dfrtopics wrapper", all=F)
    expect_output(print(summary(stg)), "Source type: STM", all=F)
    expect_output(print(summary(stg)), "Top Words", all=F)

    expect_equal(n_topics(stg), K)
    expect_equal(labelTopics(st, n=4)$prob,
                 matrix(top_words(stg, 4)$word, nrow=K, byrow=T),
                 check.attributes=F)
    expect_equal(unlist(findThoughts(st, n=4)$index),
                 top_docs(stg, n=4)$doc,
                 check.attributes=F)
    expect_equal(rowSums(doc_topics(stg)), rep(1, length(fs)))
    expect_equal(rowSums(topic_words(stg)), rep(1, K))
    srs <- topic_series(stg)
    date_range <- range(meta$pubdate[meta$id %in% doc_ids(stg)])
    expect_equal(sort(unique(srs$pubdate)),
        seq(date_range[1], date_range[2], by="years")
    )

    brow <- getOption("browser")
    options(browser=function (x) message("browser: ", x))

    bout <- tempfile("stg-dfb")
    expect_message(dfr_browser(stg, out_dir=bout),
        paste0("browser: file://",
               normalizePath(file.path(bout, "index.html"))))

    clear_files(bout, recursive=T) 

    options(browser=brow)
})

test_that("We can align models from a buncha sources", {
    skip_if_not_installed("topicmodels")
    skip_if_not_installed("stm")
    library(topicmodels)
    library(stm)
    ldag <- wordcounts_DocumentTermMatrix(counts) %>% 
        LDA(k=K, control=list(alpha=0.1)) %>%
        foreign_model(meta)
    corp <- wordcounts_stm_inputs(counts, meta)
    corp$data$journaltitle <- factor(corp$data$journaltitle)
    stg <- stm(corp$documents, corp$vocab, K=K,
              prevalence = ~ journaltitle,
              data=corp$data,
              max.em.its=40,
              verbose=F) %>%
        foreign_model(corp$data)

    mm <- wordcounts_instances(counts) %>%
        train_model(n_topics=K)

    dst <- model_distances(list(mm, stg, ldag), V)
    expect_true(all(dfrtopics:::unnest_model_distances(dst) > 0))

    cl <- align_topics(dst)

    expect_less_than(max(unlist(cl$clusters)), 3 * K / 2 + 1 / 2,
                     info="Three models yields at worst all-pair clusters")
})



