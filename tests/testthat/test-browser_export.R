context("dfr-browser export")

clear_files <- function (fs) {
    for (f in fs) {
        if (file.exists(f)) {
            unlink(f)
        }
    }
}

expect_files <- function (fs, desc="") {
    for (f in fs) {
        expect_that(file.exists(f), is_true(),
                    info=paste(desc, "file:", f))
    }
}

test_that("dfr-browser export produces the right files", {

    data_dir <- file.path(path.package("dfrtopics"), "test-data",
                          "pmla-modphil1905-1915")

    # run model

    n <- 8 # topics
    m <- model_dfr_documents(
        citations_files=file.path(data_dir, "citations.tsv"),
        wordcounts_dirs=file.path(data_dir, "wordcounts"),
        stoplist_file=file.path(path.package("dfrtopics"),
                                "stoplist", "stoplist.txt"),
        n_topics=n,
        n_iters=140,
        seed=42,
        threads=1L, 
        alpha_sum=5,beta=0.01,
        n_hyper_iters=20,
        n_burn_in=20,
        n_max_iters=10)

    # check that export works with data objects

    out_dir <- file.path(tempdir(), "browser")
    if (!file.exists(out_dir)) {
        dir.create(out_dir, recursive=T)
    }

    out_files <- file.path(out_dir, c(
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json"))


    clear_files(out_files)

    export_browser_data(m, out_dir=out_dir, zipped=T)

    expect_files(out_files, "Export with data objects:") 
    clear_files(out_files)

    # check that non-zipped export produces files

    out_files_non_zip <- gsub("\\.zip$", "", out_files)
    export_browser_data(m, out_dir=out_dir, zipped=F)

    expect_files(out_files_non_zip, "Non-zipped export:") 
    clear_files(out_files_non_zip)

    # clean up

    clear_files(out_files)

    expect_that(length(list.files(out_dir)), equals(0),
                info="check that no files are left over")

    clear_files(out_dir)
}) 
