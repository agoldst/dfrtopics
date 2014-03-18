test_that("Instances are made as we expect", {
    
    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())
    expect_that(length(list.files(file.path(data_dir,"wordcounts"))),
                equals(665))

    texts <- read_dfr_wordcounts(dirs=file.path(data_dir,"wordcounts"))
    instances <- make_instances(texts,
        stoplist_file=file.path(path.package("dfrtopics"),
                                "stoplist","stoplist.txt"))

    metadata <- read_metadata(file.path(data_dir,"citations.CSV"))

    # check instance ids against metadata ids (not in same order)
    expect_that(sort(instances_ids(instances)),
                equals(sort(metadata$id)))

    # check instances against cmd-line instances
    ci_insts <- read_instances(file.path(path.package("dfrtopics"),
                                         "mallet_check","docs.mallet"))

    expect_that(instances$size(),equals(ci_insts$size()))
    vocab <- instances_vocabulary(instances)
    ci_vocab <- instances_vocabulary(ci_insts)

    # check vocab
    expect_that(vocab, equals(ci_vocab))

    # spot-check instance equality
    expect_that(instance_text(instances$get(29L),vocab),
                equals(instance_text(ci_insts$get(29L),ci_vocab)))
}) 
