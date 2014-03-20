test_that("dfr-browser export produces the right files", {

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

    # check that export works with data objects

    out_dir <- tempdir()
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    out_files <- file.path(out_dir,c(
        "doc_len.json.zip",
        "dt.json.zip",
        "info.json",
        "meta.csv.zip",
        "topic_scaled.csv",
        "tw.json"))

    clear_files <- function (fs) {
        for(f in fs) {
            if (file.exists(f)) {
                unlink(f)
            }
        }
    }

    expect_files <- function (fs,desc="") {
        for(f in fs) {
            expect_that(file.exists(f),is_true(),
                        info=paste(desc,"file:",f))
        }
    }


    clear_files(out_files)

    export_browser_data(
        out_dir=out_dir,
        metadata=m$metadata,
        keys=m$wkf,
        doc_topics=m$doc_topics,
        topic_scaled=topic_scaled_2d(m$trainer),
        zipped=T)

    expect_files(out_files,"Export with data objects:")

    clear_files(out_files)

    # check that non-zipped export produces files

    out_files_non_zip <- gsub("\\.zip$","",out_files)
    export_browser_data(
        out_dir=out_dir,
        metadata=m$metadata,
        keys=m$wkf,
        doc_topics=m$doc_topics,
        topic_scaled=topic_scaled_2d(m$trainer),
        zipped=F)

    expect_files(out_files_non_zip,"Non-zipped export:")

    clear_files(out_files_non_zip)

    # check that export works with data files

    output_model(m,out_dir,save_instances=F,save_scaled=T)

    export_browser_data(out_dir=out_dir,
                        metadata=file.path(data_dir,"citations.CSV"),
                        keys=file.path(out_dir,"keys.csv"),
                        doc_topics=file.path(out_dir,"doc_topics.csv"),
                        topic_scaled=file.path(out_dir,"topic_scaled.csv"),
                        zipped=T)

    expect_files(out_files,"Export with data files:")

    # clean up

    clear_files(out_files)
    clear_files(file.path(out_dir,c(
        "doc_topics.csv",
        "topic_words.csv",
        "vocab.txt",
        "params.csv",
        "keys.csv",
        "mallet_state.gz",
        "diagnostics.xml",
        "id_map.txt",
        "topic_scaled.csv")))

    expect_that(length(list.files(out_dir)),equals(0),
                info="check that no files are left over")
}) 
