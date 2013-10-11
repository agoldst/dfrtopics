
make_model <- function(
        output_dir,
        num.topics,
        instances=file.path(output_dir,"journals.mallet"),
        alpha.sum=5,
        beta=0.01 ,
        n.iters=500,
        n.max.iters=10,     # "iterated conditional modes" 
        optimize_hyperparameters=T,     # if F, overrides next 3
        n.hyper.iters=20,   # how often to do hyperparameter optimization
        n.burn.in=50,       # how many iterations before hyperparam. opt. starts
        symmetric_alpha=F,  # all alpha_k equal (but still optimize value)
        threads=16L,
        smoothed=F,         # applies to doc-topic and topic-word
        normalized=F,       # ditto
        doctopics_file=file.path(output_dir,"doc_topics.csv"),
        state_file=file.path(output_dir,"mallet_state.gz"),
        num.top.words=50,   # number of top "key" words for each topic
        wk_file=file.path(output_dir,"keys.csv"),
        topic_words_file=file.path(output_dir,"topic_words.csv"),
        vocab_file=file.path(output_dir,"vocab.txt"),
        diagnostics_file=file.path(output_dir,"diagnostics.xml"),
        params_file=file.path(output_dir,"params.csv"),
        dfr_analysis_root="~/Developer/dfr-analysis",
        dfr_analysis_source=file.path(dfr_analysis_root,"source_all.R"),
        java_heap="2g") {

    make_model_wd <- getwd()
    setwd(dfr_analysis_root)
    source(dfr_analysis_source)
    topics_rmallet_setup(java_heap)
    setwd(make_model_wd)

    message("Beginning mallet train-topics run...")

    trainer <- train_model( 
            instances=instances,
            num.topics=num.topics,
            alpha.sum=alpha.sum,
            beta=beta,
            n.iters=n.iters,
            n.max.iters=n.max.iters,
            optimize_hyperparameters=optimize_hyperparameters,
            n.hyper.iters=n.hyper.iters,
            n.burn.in=n.burn.in,
            threads=threads,
            symmetric_alpha=symmetric_alpha)

    message("mallet run complete.")
    message("Saving document topics to ",doctopics_file)
    doctopics <- doc_topics_frame(trainer,
                                  smoothed=smoothed,normalized=normalized)
    write.table(doctopics,
              doctopics_file,
              quote=F,sep=",",
              row.names=F,
              col.names=T)

    message("Saving state to ",state_file)
    write_mallet_state(trainer,outfile=state_file)

    message("Saving weighted keys to ",wk_file)
    wkf <- weighted_keys_frame(trainer,num.top.words=num.top.words,
                               smoothed=smoothed,
                               normalized=normalized)
    write.table(wkf,
              wk_file,
              quote=F,sep=",",
              row.names=F,
              col.names=T)


    # this function does its own messaging
    write_topic_words(trainer,
                      topic_words_file=topic_words_file,
                      vocab_file=vocab_file,
                      smoothed=smoothed,
                      normalized=normalized)

    # TODO more refined handling of diagnostics
    message("Saving mallet diagnostics to ",diagnostics_file)

    diagnostics <- get_diagnostics(trainer,as.integer(num.top.words))
    write_diagnostics(trainer,output_file=diagnostics_file,
                      diagnostics=diagnostics)

    message("Recording misc. model parameters to ",params_file)
    params <- model_params(trainer)
    write.table(params,params_file,quote=F,sep=",",row.names=F,col.names=T)

    # return the trainer object for further exploration
    trainer
}

# execution: should allow the trainer object to persist
# as in: trainer <- make_model()

