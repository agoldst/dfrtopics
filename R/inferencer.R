# functions for using MALLET's topic-inference functionality: given an
# existing topic model, estimate topic proportions for new documents
#
# Workflow
# --------
#
# 1. Create instances-list object of base corpus
#    (or load from disk with litdata::read_mallet_instances)
# 2. Create topic model of base corpus
# 3. Get inferencer object for the model with inferencer()
#    (or, having done this earlier and saved it with write_inferencer(),
#    load it from disk)
# 4. Use compatible_instances() to create instances-list object of new corpus
#    (or, having done this, load it from disk)
# 5. Use infer_topics() to infer topics for new corpus
#
# Step (4) can be done any time after step (1).
#
# Last update: 2015-05-19 by AG

# -----
# given a trained model object, return its topic inferencer object
#
# model: model object from MalletLDA(). Run model$train() first.
#
# returns a reference to a topic inferencer object

inferencer <- function (model) {
    model$model$getInferencer()
}

# -----
# save an inferencer object to a file
#
# inf: a reference to a topic inferencer, from inferencer()
#
# out_file: the name of a file to save to (will overwrite an existing file)

write_inferencer <- function (inf, out_file) {
    fos <- .jnew("java/io/FileOutputStream", out_file)
    oos <- .jnew("java/io/ObjectOutputStream",
                 .jcast(fos, "java/io/OutputStream"))
    oos$writeObject(inf)
    oos$close()
}

# -----
# retrieve an inferencer object from a file
#
# returns a reference to a topic inferencer object

read_inferencer <- function (in_file) {
    J("cc.mallet.topics.TopicInferencer")$read(
        new(J("java.io.File"), in_file)
    )
}

# -----
# infer document topics. This is like the Gibbs sampling process for making a 
# topic model, but the topic-word proportions are not updated.
#
# inferencer: a topic inferencer object
#
# instances: an instances list object from compatible_instances()---or
# any instances that are compatible with the inferencer, i.e. their
# vocabulary has to correspond to that of the instances used to create
# the model that yielded the inference 
#
# n_iterations: number of Gibbs sampling iterations
#
# sampling_interval: thinning interval
#
# burn_in: number of burn-in iterations
#
# random_seed: integer random seed; set for reproducibility
#
# returns a matrix of estimated document-topic proportions m, where m[i, j] 
# gives the proportion (between 0 and 1) of topic j in document i. The
# inferencer sampling state is not accessible.

infer_topics <- function (inferencer, instances,
        n_iterations=100,
        sampling_interval=10, # aka "thinning"
        burn_in=10,
        random_seed=NULL) {

    iter <- instances$iterator()
    n_iterations <- as.integer(n_iterations)
    sampling_interval <- as.integer(sampling_interval)
    burn_in <- as.integer(burn_in)
    if (!is.null(random_seed)) {
        inferencer$setRandomSeed(as.integer(random.seed))
    }

    doc_topics <- vector("list", instances$size())
    for (j in 1:instances$size()) {
        inst <- .jcall(iter, "Ljava/lang/Object;", "next")
        doc_topics[[j]] <- inferencer$getSampledDistribution(inst,
            n_iterations, sampling_interval, burn_in)
    }

    do.call(rbind, doc_topics)
}

