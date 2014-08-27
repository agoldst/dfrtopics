test_that("metadata functions work as expected", {
    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")
    
    meta_f <- file.path(data_dir,"citations.CSV")
    expect_that(file.exists(meta_f),is_true())
    meta <- read_metadata(meta_f)
    meta_lines <- readLines(meta_f)
    meta_lines <- meta_lines[meta_lines != ""]

    # correct number of rows?
    expect_that(nrow(meta),equals(length(meta_lines) - 1))

    # correct columns?
    expect_that(colnames(meta),equals(c("id","doi","title","author","journaltitle","volume","issue","pubdate","pagerange","publisher","type","reviewed.work")))

    # id-filename conversion
    expect_that(id_filename("10.2307/3175328"),
                equals("wordcounts_10.2307_3175328.CSV"))
    expect_that(filename_id("path/to/wordcounts_10.2307_3175328.CSV"),
                equals("10.2307/3175328"))

    # pubdate-Date conversion
    expect_that(pubdate_Date("1981-12-01T00:00:00Z\t"),
                equals(as.Date("1981-12-01")))

    # dx.doi url
    expect_that(dfr_id_url("10.2307/3175327"),
                equals("http://www.jstor.org/stable/10.2307/3175327"))

    # citation generation
    expect_that(cite_articles(meta,"10.2307/2872914"),
                equals('Sharon Cameron, "Ahab and Pip: Those Are Pearls That Were His Eyes," *ELH* 48, no. 3 (October 1981): 573-593.'))

})

