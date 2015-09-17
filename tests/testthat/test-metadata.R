context("Document metadata")

test_that("metadata functions work as expected on old-style CSV", {

    # mock up old-style file (excerpt from a real one)

    meta_f <- tempfile(fileext=".CSV")

    writeLines(
"id,doi,title,author,journaltitle,volume,issue,pubdate,pagerange,publisher,type,reviewed-work
10.2307/2872880,10.2307/2872880	,Sterne's Novels: Gathering Up the Fragments	,Elizabeth W. Harries	,ELH	,49	,1	,1982-04-01T00:00:00Z	,pp. 35-49	,The Johns Hopkins University Press	,fla	,	,
10.2307/461754,10.2307/461754	,Volume Information	,	,PMLA	,95	,5	,1980-10-01T00:00:00Z	,	,Modern Language Association	,mis	,	,
10.2307/2872960,10.2307/2872960	,The Courtship of the Family: Clarissa and the Harlowes Once More	,John Allen Stevenson	,ELH	,48	,4	,1981-12-01T00:00:00Z	,pp. 757-777	,The Johns Hopkins University Press	,fla	,	,
10.2307/461755,10.2307/461755	,Front Matter	,	,PMLA	,95	,5	,1980-10-01T00:00:00Z	,pp. 781-915	,Modern Language Association	,mis	,	,
10.2307/462201,10.2307/462201	,The Tolstoy Connection in Bakhtin	,Caryl Emerson	,PMLA	,100	,1	,1985-01-01T00:00:00Z	,pp. 68-80	,Modern Language Association	,fla	,	,
",
        meta_f
    )

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
