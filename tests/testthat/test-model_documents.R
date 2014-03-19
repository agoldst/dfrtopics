test_that("Modeling on some sample data works", {
    
    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())
    expect_that(length(list.files(file.path(data_dir,"wordcounts"))),
                equals(60))

    # run model

    n <- 8 # topics
    n_top_words <- 10 # key words
    m <- model_documents(citations_files=file.path(data_dir,"citations.CSV"),
                         dirs=file.path(data_dir,"wordcounts"),
                         stoplist_file=file.path(path.package("dfrtopics"),
                                                 "stoplist","stoplist.txt"),
                         n_topics=n,
                         n_iters=140,
                         seed=42,
                         threads=1L, 
                         alpha_sum=5,beta=0.01,
                         n_hyper_iters=20,
                         n_burn_in=20,
                         n_max_iters=0,
                         n_top_words=n_top_words)

    # remembered the seed?
    expect_that(m$seed,equals(42))

    # right number of rows in doc-topics?
    expect_that(nrow(m$doc_topics),
                equals(length(list.files(file.path(data_dir,"wordcounts")))))

    # right number of cols?
    expect_that(ncol(m$doc_topics),equals(n + 1))

    # right number of key words in keys frame?
    expect_that(nrow(m$wkf),equals(n * n_top_words))


    # check that file output creates all the expected files
    out_dir <- tempdir()
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    out_files <- file.path(out_dir,c(
        "topic_words.csv",
        "vocab.txt",
        "params.csv",
        "keys.csv",
        "mallet_state.gz",
        "diagnostics.xml",
        "id_map.txt",
        "instances.mallet",
        "topic_scaled.csv"))

    clear_files <- function (fs) {
        for(f in fs) {
            if (file.exists(f)) {
                unlink(f)
            }
        }
    }

    clear_files(out_files)

    output_model(m,out_dir,save_instances=T,save_scaled=T)

    expect_that(all(sapply(out_files,file.exists)),is_true())

    clear_files(out_files)

    # check that output_model fills in missing keys and doc_topics

    m$wkf <- NULL
    m$doc_topics <- NULL

    output_model(m,out_dir,save_instances=T,save_scaled=T)

    expect_that(all(sapply(out_files,file.exists)),is_true())

    clear_files(out_files)
}) 
