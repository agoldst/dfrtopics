test_that("Sampling state manipulation works as expected", {
    # construct a small trial model

    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())

    # run model

    m <- model_documents(citations_files=file.path(data_dir,"citations.CSV"),
                         dirs=file.path(data_dir,"wordcounts"),
                         stoplist_file=file.path(path.package("dfrtopics"),
                                                 "stoplist","stoplist.txt"),
                         n_topics=8,
                         n_iters=140,
                         seed=42,
                         threads=1L, 
                         alpha_sum=5,beta=0.01,
                         n_hyper_iters=20,
                         n_burn_in=20,
                         n_max_iters=0) # not available in cmdline mallet

    out_dir <- tempdir()
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    state_file <- file.path(out_dir,"state.gz")
    write_mallet_state(m$trainer,state_file)

    expect_that(file.exists(state_file),is_true(),
                info="write_mallet_state() saves a file")

    ss_file <- file.path(out_dir,"state_simple.csv")
    simplify_state(state_file,ss_file)

    expect_that(file.exists(ss_file),is_true(),
                info="simplify_state() saves a file")

    ss_frame <- read.csv(ss_file)

    expect_that(sum(ss_frame$count),
                equals(sum(doc_topics_matrix(m$doc_topics))),
                info="total token counts same in doc topics and ss")

    # zero-based ss indices, 1-based doc_topics indices
    expect_that(sum(ss_frame$count[ss_frame$topic==1]),
                equals(sum(m$doc_topics[,2])),
                info="topic 1 sum in ss_frame = topic 2 sum in doc tops")

    if(require(bigmemory)) {
        ss <- read_simplified_state(ss_file)
        expect_that(dim(ss),equals(dim(ss_frame)),
                    info="big.matrix ss same dims as ss_frame")


        expect_that(sum(ss_frame$count[ss_frame$topic==1]),
                    equals(sum(ss[mwhich(ss,"topic",2,"eq"),"count"])),
                    info="topic 1 sum in ss_frame = topic 2 sum in ss")

    }

    for (f in c(state_file,ss_file)) {
        if(file.exists(f)) {
            unlink(f)
        }
    }
})


