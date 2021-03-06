context("dfr-browser export")

library(dplyr)
library(Matrix)
options(java.parameters="-Xmx2g",
        dfrtopics.mallet_logging="none",
        dplyr.show_progress=FALSE,
        dfrtopics.verbose=F)


clear_files <- function (fs, ...) {
    for (f in fs) {
        if (file.exists(f)) {
            unlink(f, ...)
        }
    }
}

expect_files <- function (fs, desc="") {
    for (f in fs) {
        expect_true(file.exists(f),
                    info=paste(desc, "file:", f))
    }
}

# run a scrap model: we're just doing manipulation here, not worrying
# about quality

data_dir <- file.path(path.package("dfrtopics"),
                          "test-data", "pmla-modphil1905-1915")

fs <- sample(list.files(file.path(data_dir, "wordcounts"), full.names=T),
             60)

stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                          "stoplist.txt")

n_topics <- 8
insts <- read_wordcounts(fs) %>%
    wordcounts_remove_rare(10000) %>%
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
    n_max_iters=10,
    metadata=read_dfr_metadata(file.path(data_dir, "citations.tsv"))
)

# a second model for multimodel tests

m2 <- train_model(
    insts,
    n_topics=n_topics + 2,
    n_iters=200,
    threads=1, 
    alpha_sum=5,
    beta=0.01,
    n_hyper_iters=20,
    n_burn_in=20,
    n_max_iters=10,
    metadata=read_dfr_metadata(file.path(data_dir, "citations.tsv"))
)

out_dir <- file.path(tempdir(), "browser_export")
if (!file.exists(out_dir)) {
    dir.create(out_dir, recursive=T)
}

dfb_data_files <- c(
    "dt.json.zip",
    "info.json",
    "meta.csv.zip",
    "topic_scaled.csv",
    "tw.json")
out_files <- file.path(out_dir, dfb_data_files)
out_files_non_zip <- gsub("\\.zip$", "", out_files)

test_that("dfr-browser export produces the right files", {

    # check that export works with data objects
    clear_files(out_files) 
    export_browser_data(m, out_dir=out_dir, zipped=T,
                        n_scaled_words=100)
    expect_files(out_files, "Export with data objects:") 

    # Check that we fail to overwrite files by default
    expect_error(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            n_scaled_words=100)
    )

    # Check that we succeed in overwriting when needed

    export_browser_data(m, out_dir=out_dir, zipped=T,
                        overwrite=T,
                        n_scaled_words=100)

    expect_files(out_files, "Overwrite with data objects:") 

    clear_files(out_files)
})

test_that("metadata checking acts as expected", {

    md <- metadata(m)
    metadata(m) <- NULL
    expect_warning(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            n_scaled_words=100),
        "Metadata frame unavailable"
    )

    expect_files(setdiff(out_files, file.path(out_dir, "meta.csv.zip")),
                 "NULL metadata export")

    clear_files(out_files)

    metadata(m) <- md
})

test_that("metadata munging acts as expected", {
    export_browser_data(m, out_dir=out_dir, zipped=T,
                        n_scaled_words=100)
    md <- read.csv(unz(file.path(out_dir, "meta.csv.zip"), "meta.csv"),
                   header=F, as.is=T)
    expect_equal(ncol(md), ncol(metadata(m)))
    expect_equal(as.data.frame(metadata(m)),
                 md[ , c(1, 9, 2:8, 10:13)] %>%
                        mutate(V7=as.Date(V7),
                               V11=factor(V11)),
                 check.attributes=F)

    clear_files(out_files)
    md <- metadata(m)
    metadata(m)$journaltitle <- NULL

    expect_message(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            n_scaled_words=100),
        "Not all expected DfR metadata columns"
    )
    md_out <- read.csv(unz(file.path(out_dir, "meta.csv.zip"), "meta.csv"),
                       header=F, as.is=T)
    expect_equal(as.data.frame(metadata(m)),
                 md_out %>%
                    mutate(V7=as.Date(V7),
                           V10=factor(V10)),
                 check.attributes=F)

    clear_files(out_files)

    metadata(m) <- md
})

test_that("non-zipped export produces files", {

    clear_files(out_files_non_zip)
    export_browser_data(m, out_dir=out_dir, zipped=F,
                        n_scaled_words=100)

    expect_files(out_files_non_zip, "Non-zipped export:") 
    clear_files(out_files_non_zip)
})

test_that("info parameter gets properly stored", {
    info_j <- list(
        title="Hoo boy",
        meta_info="<h2>YO</h2>",
        VIS=list(overview_words=15, resize_refresh_delay=50)
    )
    export_browser_data(m, out_dir=out_dir, zipped=T,
                        supporting_files=F,
                        n_scaled_words=100,
                        info=info_j)
    expect_files(out_files, "export with specified info")
    expect_equal(jsonlite::fromJSON(file.path(out_dir, "info.json")),
                 info_j)
    clear_files(out_files)
})

test_that("dfb copy works too", {

    options(dfrtopics.verbose=T)
    expect_message(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            supporting_files=T,
                            n_scaled_words=100),
        "Copying"
    )

    options(dfrtopics.verbose=F)
    dfb_files <- file.path(out_dir,
                           c("bin", "css", "fonts", "img",
                             "js", "lib", "index.html", "LICENSE",
                             "data"))
    expect_files(dfb_files, "dfb support: ")
    expect_files(file.path(out_dir, "data", c(
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json")))

    # check that dfb overwrite prevention works

    expect_error(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            supporting_files=T,
                            n_scaled_words=100)
    )

    # check that overwrite succeeds here too when some files are present

    clear_files(file.path(out_dir, "data", "tw.json"))
    clear_files(file.path(out_dir, "index.html"))
    export_browser_data(m, out_dir=out_dir, zipped=T,
                        supporting_files=T, overwrite=T,
                        n_scaled_words=100)

    expect_files(dfb_files, "dfb support overwrite: ")
    expect_files(file.path(out_dir, "data", c(
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json")))


    # clean up

    clear_files(dfb_files, recursive=T)
    clear_files(file.path(out_dir), recursive=T)

    expect_equal(length(list.files(out_dir)), 0,
                 info="check that no files are left over")

    clear_files(out_dir)
})

test_that("dfr_browser does multifile export", {
    dfr_browser(m, out_dir=out_dir, internalize=F, browse=F,
                n_scaled_words=100)
    dfb_files <- file.path(out_dir,
                           c("bin", "css", "fonts", "img",
                             "js", "lib", "index.html", "LICENSE",
                             "data"))
    expect_files(dfb_files, "dfb support: ")
    expect_files(file.path(out_dir, "data", c(
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json")))
    clear_files(out_dir)
})

test_that("dfr_browser does internalized export", {
    dfr_browser(m, out_dir=out_dir, n_scaled_words=100, internalize=T,
                browse=F)
    expect_true(all(!file.exists(out_files)))
    idx <- file.path(out_dir, "index.html")
    expect_files(idx)
    idx_html <- readLines(idx)
    for (d in c("dt", "info", "meta", "topic_scaled", "tw")) {
        expect_match(idx_html, paste0('id="m__DATA__', d), all=F)
    }
    clear_files(out_dir, recursive=T)
})

test_that("proper doc-topics actually are", { 
    clear_files(out_files_non_zip)

    dt_prop <- dt_smooth_normalize(m)(doc_topics(m))
    dg <- abs(round(log10(min(dt_prop)))) - 1
    expect_true(all(
            apply(dt_prop, 1, function (row) any(round(row, dg) > 0))
        ),
        info="check that rounding doesn't leave a degenerate matrix"
    )
    expect_true(any(round(dt_prop, dg) == 0),
        info="check that rounding zaps some entry"
    )

    export_browser_data(m, out_dir=out_dir, zipped=F, n_scaled_words=100,
        proper=TRUE, digits=dg
    )

    dtj <- jsonlite::fromJSON(file.path(out_dir, "dt.json"))
    expect_true(all(dtj$x > 0), info="check that output has no zero entries")
    expect_true((length(dtj$x) < nrow(dt_prop) * ncol(dt_prop)),
        info="check that some entries were dropped")
    dtm <- sparseMatrix(i=dtj$i, p=dtj$p, x=dtj$x, index1=FALSE)
    expect_equal(Matrix(round(dt_prop, dg), sparse=TRUE), dtm,
        info="check that input and output matrices match"
    )
    expect_equal(rowSums(dtm), rep(1, nrow(dtm)), tolerance=10^-(dg - 1),
        info="check that matrix is actually approximately proper"
    )

    clear_files(out_files_non_zip)
})

test_that("permuted export behaves correctly", {
    # pick a non-self-inverse permutation
    n <- n_topics
    if (n_topics %% 2 == 1) n <- n - 1

    p <- c(n:(n / 2 + 1), 1:(n / 2))
    if (n_topics %% 2 == 1) p <- c(n_topics, p)

    export_browser_data(m, out_dir=out_dir, zipped=F, n_top_words=10,
                        n_scaled_words=100, permute=p, digits=4)
    dtj <- jsonlite::fromJSON(file.path(out_dir, "dt.json"))
    dtm <- sparseMatrix(i=dtj$i, p=dtj$p, x=dtj$x, index1=FALSE)
    expect_equal(doc_topics(m)[ , 4 ], dtm[, match(4, p)])
    expect_equal(doc_topics(m), as.matrix(dtm)[, order(p)], check.attributes=F)
    twj <- jsonlite::fromJSON(file.path(out_dir, "tw.json"),
                              simplifyDataFrame=F)
    expect_equal(round(hyperparameters(m)$alpha, 4)[p], twj$alpha)
    tw2 <- top_words(m, 10) %>% filter(topic == p[2])
    expect_equal(twj$tw[[2]]$weights, tw2$weight)
    expect_equal(twj$tw[[2]]$words, tw2$word)
    clear_files(out_files_non_zip)
})

test_that(
    "multimodel browser export generates appropriate files for a list of models", {
    dfr_browser(list(m, m2), browse=F, out_dir=out_dir)
    expect_files(file.path(out_dir, "data", "info.json"))
    expect_files(file.path(out_dir, "data", "model1", dfb_data_files))
    expect_files(file.path(out_dir, "data", "model2", dfb_data_files))
    clear_files(out_dir, recursive=T)
})

test_that(
    "multimodel browser export generates appropriate files for a list of conditions", {
    dfr_browser(m, condition=c("pubdate", "journaltitle"),
                browse=F, out_dir=out_dir)
    expect_files(file.path(out_dir, "data", dfb_data_files))
    expect_files(file.path(out_dir, "data", "info-pubdate.json"))
    expect_files(file.path(out_dir, "data", "info-journaltitle.json"))
    clear_files(out_dir, recursive=T)
})

test_that(
    "multimodel browser export generates appropriate files for a list of models and multiple conditions", {
    dfr_browser(list(m, m2), condition=c("pubdate", "journaltitle"),
                browse=F, out_dir=out_dir)
    expect_files(file.path(out_dir, "data", "info.json"))
    ff <- c(setdiff(dfb_data_files, "info.json"),
            "info-pubdate.json", "info-journaltitle.json")
    expect_files(file.path(out_dir, "data", "model1", ff))
    expect_files(file.path(out_dir, "data", "model2", ff))
    clear_files(out_dir, recursive=T)
})

test_that(
    "aligned models browser export generates appropriate files", {
    dst <- model_distances(list(m, m2), n=length(vocabulary(m)))
    cl <- align_topics(dst)
    dfr_browser(cl, browse=F, out_dir=out_dir)
    expect_files(file.path(out_dir, "data", "info.json"))
    expect_files(file.path(out_dir, "data",
                           paste0("m1k", n_topics(m)), dfb_data_files))
    expect_files(file.path(out_dir, "data",
                           paste0("m2k", n_topics(m2)), dfb_data_files))

    expect_equal(jsonlite::fromJSON(file.path(out_dir, "data", 
                           paste0("m2k", n_topics(m2)), "info.json"))$topic_ids,
                 cl$clusters[[2]])

    clear_files(out_dir, recursive=T)
})
