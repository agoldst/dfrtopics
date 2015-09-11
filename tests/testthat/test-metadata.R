context("Document metadata")

test_that("metadata functions work as expected on old-style CSV", {

    meta_f <- file.path(path.package("dfrtopics"), "test-data",
                        "old-style", "citations.CSV")
    expect_that(file.exists(meta_f), is_true())
    meta <- read_dfr_metadata(meta_f)
    meta_lines <- readLines(meta_f)
    meta_lines <- meta_lines[meta_lines != ""]

    # correct number of rows?
    expect_that(nrow(meta), equals(length(meta_lines) - 1))

    # correct columns?
    expect_that(colnames(meta), equals(c(
        "id", "doi", "title", "author", "journaltitle", "volume", "issue",
        "pubdate", "pagerange", "publisher", "type", "reviewed.work")
    ))
})

test_that("metadata functions work as expected on new-style TSV", {
    meta_new_f <- file.path(path.package("dfrtopics"),
                            "test-data", "pmla-modphil1905-1915",
                            "citations.tsv")
    expect_that(file.exists(meta_new_f), is_true())
    meta_new <- read_dfr_metadata(meta_new_f)
    meta_lines_new <- readLines(meta_new_f)
    meta_lines_new <- meta_lines_new[meta_lines_new != ""]

    # correct number of rows?
    expect_that(nrow(meta_new), equals(length(meta_lines_new) - 1))

    # correct columns?
    expect_that(colnames(meta_new), equals(c(
        "id", "doi", "title", "author", "journaltitle", "volume", "issue",
        "pubdate", "pagerange", "publisher", "type", "reviewed.work", 
        "abstract")
    ))

    # citation generation
    expect_that(cite_articles(meta_new[1, ]), equals(
        "C. R. Baskervill, \"Sidney's \"Arcadia\" and \"The Tryall of Chevalry\",\" *Modern Philology* 10, no. 2 (October 1912): 197-201.")
    )

})


test_that("metadata utility functions work correctly", {
    # id-filename conversion
    expect_that(id_dfr_filename("10.2307/3175328"),
                equals("wordcounts_10.2307_3175328.CSV"))
    expect_that(dfr_filename_id("path/to/wordcounts_10.2307_3175328.CSV"),
                equals("10.2307/3175328"))

    # pubdate-Date conversion
    expect_that(pubdate_Date("1981-12-01T00:00:00Z\t"),
                equals(as.Date("1981-12-01")))

    # dx.doi url
    expect_that(dfr_id_url("10.2307/3175327"),
                equals("http://www.jstor.org/stable/10.2307/3175327"))
})
