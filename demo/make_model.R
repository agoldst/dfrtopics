
make_model <- function(
        output_dir="model",
        num.topics,
        instances="instances/docs.mallet",
        save_scaled=T,      # write MD-scaled topic JS-divergences?
        alpha.sum=5,
        beta=0.01,
        n.iters=500,
        n.max.iters=10,     # "iterated conditional modes" 
        optimize_hyperparameters=T,     # if F, overrides next 3
        n.hyper.iters=20,   # how often to do hyperparameter optimization
        n.burn.in=50,       # how many iterations before hyperparam. opt. starts
        symmetric_alpha=F,  # all alpha_k equal (but still optimize value)
        threads=5L,
        smoothed=F,         # applies to doc-topic and topic-word
        normalized=F,       # ditto
        num.top.words=50,   # number of top "key" words for each topic
        dfr_analysis_root="~/Developer/dfr-analysis",
        java_heap="2g",
        seed=42L) {

    make_model_wd <- getwd()
    setwd(dfr_analysis_root)
    source("topics_rmallet.R")
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
            symmetric_alpha=symmetric_alpha,
            seed=seed)

    message("mallet run complete.")

    if(!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    output_model(list(trainer=trainer,
                      seed=seed,
                      wkf=weighted_keys_frame(trainer,
                                              num.top.words=num.top.words,
                                              smoothed=smoothed,
                                              normalized=normalized),
                      doc_topics=doc_topics_frame(trainer,
                                                   smoothed=smoothed,
                                                   normalized=normalized)),
                 output_dir=output_dir,
                 save_instances=F,
                 save_scaled=save_scaled)

    # return the trainer object for further exploration
    trainer
}

# execution: should allow the trainer object to persist
# as in: trainer <- make_model()

