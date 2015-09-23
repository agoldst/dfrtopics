context("dfr-browser export")

library(dplyr)

clear_files <- function (fs, ...) {
    for (f in fs) {
        if (file.exists(f)) {
            unlink(f, ...)
        }
    }
}

expect_files <- function (fs, desc="") {
    for (f in fs) {
        expect_that(file.exists(f), is_true(),
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


out_dir <- file.path(tempdir(), "browser_export")
if (!file.exists(out_dir)) {
    dir.create(out_dir, recursive=T)
}

out_files <- file.path(out_dir, c(
    "dt.json.zip",
    "info.json",
    "meta.csv.zip",
    "topic_scaled.csv",
    "tw.json"))

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
    expect_equal(metadata(m),
                 md[ , c(1, 9, 2:8, 10:13)] %>%
                        mutate(V7=as.Date(V7),
                               V11=factor(V11)),
                 check.attributes=F)

    clear_files(out_files)
    md <- metadata(m)
    metadata(m)$journaltitle <- NULL

    expect_warning(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            n_scaled_words=100),
        "Not all expected metadata columns"
    )
    md_out <- read.csv(unz(file.path(out_dir, "meta.csv.zip"), "meta.csv"),
                       header=F, as.is=T)
    expect_equal(metadata(m),
                 md_out %>%
                    mutate(V7=as.Date(V7),
                           V10=factor(V10)),
                 check.attributes=F)

    clear_files(out_files)

    metadata(m) <- md
})

test_that("non-zipped export produces files", {

    out_files_non_zip <- gsub("\\.zip$", "", out_files)
    clear_files(out_files_non_zip)
    export_browser_data(m, out_dir=out_dir, zipped=F,
                        n_scaled_words=100)

    expect_files(out_files_non_zip, "Non-zipped export:") 
    clear_files(out_files_non_zip)
})

test_that("dfb download works too", {

    expect_message(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            download_dfb=T,
                            n_scaled_words=100),
        "Downloading"
    )
    dfb_files <- file.path(out_dir,
                           c("bin", "css", "fonts",
                             "js", "lib", "index.html", "LICENSE",
                             "data"))
    expect_files(dfb_files, "dfb-download: ")
    expect_files(file.path(out_dir, "data", c(
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json")))

    # check that dfb overwrite prevention works

    expect_error(
        export_browser_data(m, out_dir=out_dir, zipped=T,
                            download_dfb=T,
                            n_scaled_words=100)
    )

    # check that overwrite succeeds here too when some files are present

    clear_files(file.path(out_dir, "data", "tw.json"))
    clear_files(file.path(out_dir, "index.html"))
    export_browser_data(m, out_dir=out_dir, zipped=T,
                        download_dfb=T, overwrite=T,
                        n_scaled_words=100)

    expect_files(dfb_files, "dfb-download overwrite: ")
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
