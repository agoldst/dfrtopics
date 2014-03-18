test_that("Topic naming functions work as expected", {

    data_dir <- file.path(path.package("dfrtopics"),"dfr_small")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())

    # run model

    m <- model_documents(citations_files=file.path(data_dir,"citations.CSV"),
                         dirs=file.path(data_dir,"wordcounts"),
                         stoplist_file=file.path(path.package("dfrtopics"),
                                                 "stoplist","stoplist.txt"),
                         n_topics=10,
                         n_iters=140,
                         seed=42,
                         threads=1L, 
                         alpha_sum=5,beta=0.01,
                         n_hyper_iters=20,
                         n_burn_in=20,
                         n_max_iters=0) # not available in cmdline mallet

    # check labeling functions on result
    # comes as a named vector, so strip names
    expect_that(as.character(topic_top_words(m$wkf,topic=1,n=3)),
                equals(c("war","grant","civil")))

    name_m <- topic_top_words(m$wkf,topic=1:2,n=3)

    # matrix unrolled by column
    expect_that(as.character(name_m),
                equals(c("war","baudelaire","grant","france",
                         "civil","tambour")))
    expect_that(rownames(name_m),
                equals(c("1","2")))

    expect_that(topic_name(m$wkf,topic=1,n=3),
                equals("001 war grant civil"))

    expect_that(topic_name(m$wkf,topic=1:2,n=3),
                equals(c("001 war grant civil",
                         "002 baudelaire france tambour")))

    expect_that(topic_name(m$wkf,topic=1:2,fmt="%d: %s",n=3),
                equals(c("1: war grant civil",
                         "2: baudelaire france tambour")))

    label <- topic_labeller(m$wkf,fmt="T%d %s",n=3)
    expect_that(label(2),equals("T2 baudelaire france tambour"))
})
