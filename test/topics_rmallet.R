if(basename(getwd()) != "dfr-analysis") {
    stop("Please run tests from the dfr-analysis root directory.")
}

source("topics_rmallet.R")

# TODO set mallet's random seed

test <- model_documents(citations.file="test_data/pmla_sample/citations.CSV",
                        dirs="test_data/pmla_sample/wordcounts",
                        stoplist.file="stoplist/mallet-2.0.7-en.txt",
                        num.topics=5)

# TODO verify results
