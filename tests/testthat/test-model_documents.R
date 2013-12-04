test_that("model_documents yields expected output on sample docs", {
    
    demo_files <- file.path(path.package("dfrtopics"),"demo","data")

    # check for presence of sample data

    expect_that(file.exists(demo_files),is_true())
    expect_that(file.exists(file.path(demo_files,"citations.CSV")),is_true())
    expect_that(length(list.files(file.path(demo_files,"wordcounts"))),
                equals(665))

    m <- model_documents(citations_files=file.path(demo_files,"citations.CSV"),
                         dirs=file.path(demo_files,"wordcounts"),
                         stoplist_file=file.path(demo_files,"stoplist.txt"),
                         num_topics=16,
                         n_iters=200,
                         seed=42,
                         threads=2L, 
                         alpha_sum=5,beta=0.01,
                         n_hyper_iters=20,
                         n_burn_in=20,
                         n_max_iters=0) # not available in cmdline mallet

    expect_that(m$seed,equals(42))

    expect_that(paste(test$wkf$word[1:19],collapse=" "),
                equals("university city chicago state york texas north international november community hill south berkeley executive iowa bloomington new chapel december "))
    #expect_that(paste(test$wkf$weight[51:70],collapse=" "),
                #equals(""))

    #expect_that(paste(test$doc_topics$id,collapse=" "),
                #equals(""))

    #expect_that(paste(unlist(test$doc_topics[3:5,1:10]),collapse=" "),
                #equals(""))


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

    #expect_that(output_model(m,out_dir,save_instances=T,save_scaled=T),
    #            shows_message(paste("Wrote",out_files)))

    expect_that(all(sapply(out_files,file.exists)),is_true())

    clear_files(out_files)

}) 
