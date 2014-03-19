test_that("Instances are made as we expect", {
    
    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())
    expect_that(length(list.files(file.path(data_dir,"wordcounts"))),
                equals(60))

    counts <- read_dfr(dirs=file.path(data_dir,"wordcounts"))
    texts <- docs_frame(counts)

    stop_f <- file.path(path.package("dfrtopics"), "stoplist","stoplist.txt")
    instances <- make_instances(texts,stoplist_file=stop_f)

    metadata <- read_metadata(file.path(data_dir,"citations.CSV"))

    # check instance ids against metadata ids (not in same order)
    expect_that(all(instances_ids(instances) %in% metadata$id),is_true())

    expect_that(instances$size(),equals(60))
    vocab <- instances_vocabulary(instances)

    # spot-check vocabulary
    expect_that(vocab[1:3],
                equals(c("macbeth","king","words")))

    # cross-check vocabulary
    stopwords <- readLines(stop_f)
    vocab2 <- setdiff(unique(counts$WORDCOUNTS),stopwords)

    expect_that(sort(vocab),equals(sort(vocab2)))

    expect_that(instance_text(instances$get(29L),vocab),
                matches("society incisive$"))

}) 
