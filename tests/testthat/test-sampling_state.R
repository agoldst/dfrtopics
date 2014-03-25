test_that("Sampling state manipulation works as expected", {
    # construct a small trial model

    data_dir <- file.path(path.package("dfrtopics"),"dfr_data")

    # check for presence of sample data

    expect_that(file.exists(data_dir),is_true())
    expect_that(file.exists(file.path(data_dir,"citations.CSV")),is_true())

    # run model

    n <- 8
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
                         n_max_iters=0) # not available in cmdline mallet

    out_dir <- tempdir()
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    state_file <- file.path(out_dir,"state.gz")

    if(file.exists(state_file)) {
        unlink(state_file)
    }

    write_mallet_state(m$trainer,state_file)

    expect_that(file.exists(state_file),is_true(),
                info="write_mallet_state() saves a file")

    ss_file <- file.path(out_dir,"state_simple.csv")

    if(file.exists(ss_file)) {
        unlink(ss_file)
    }

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


        id_map <- m$doc_topics$id
        vocab <- m$trainer$getVocabulary()

        tytm <- vector("list",n)
        for(i in 1:n) {
            tytm[[i]] <- term_year_topic_matrix(i,ss,id_map,vocab,m$metadata)
        }

        yseq <- tytm[[1]]$yseq
        expect_that(all(sapply(tytm,`[[`,"yseq")==yseq),
                    is_true(),
                    info="all tytm have same year sequences")
        years <- sort(unique(strftime(pubdate_Date(m$metadata$pubdate),
                                      "%Y-01-01")))
        expect_that(yseq,equals(years),
                    info="tytm yseq matches metadata year sequence")

        expect_that(sum(m$doc_topics[,2]),
                    equals(sum(tytm[[2]]$tym)),
                    info="sum across all of topic 2 matches tytm[[2]] sum")

        expect_that(sum(ss_frame$count[ss_frame$type==18]),
                    equals(sum(sapply(tytm,function(x) { sum(x$tym[19,]) }))),
                    info="sum for term 0 matches in ss_frame and tytm's")

        tdtm <- vector("list",n)
        for(i in 1:n) {
            tdtm[[i]] <- term_document_topic_matrix(i,ss,id_map,vocab)
        }

        expect_that(sum(m$doc_topics[,2]),
                    equals(sum(tdtm[[2]])),
                    info="sum across all of topic 2 matches tdtm[[2]] sum")

        dtm <- doc_topics_matrix(m$doc_topics)

        # kind of screwy: column sum of tdtm[[i]] gives a vector of document
        # weights for topic i; this is a plain vector, which sapply
        # understands as column when it glues together its results into a matrix

        expect_that(all(dtm==sapply(tdtm,colSums)),
                    is_true(),
                    info="column sums of tdtm's = rows of doc topic matrix")

        yearly_top_words1 <- topic_yearly_top_words(tytm[[1]]$tym,
                                                    tytm[[1]]$yseq,
                                                    vocab,n_words=5)
        expect_that(length(yearly_top_words1),equals(length(yseq)),
                    info="top words series has an entry for each year")
        expect_that(yearly_top_words1[1],
                    matches("^new university pages english available$"))

        new_series <- topic_term_time_series("new",tytm[[1]]$tym,vocab)
        new_index <- match("new",vocab)
        expect_that("new",equals(vocab[new_index]))
        expect_that(sum(new_series),
                    equals(sum(ss_frame$count[ss_frame$type == new_index - 1
                               & ss_frame$topic == 0])),
                    info="sum over time series for 'new' = sum in ss_frame")
    }

    for (f in c(state_file,ss_file)) {
        if(file.exists(f)) {
            unlink(f)
        }
    }
})


