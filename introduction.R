## ----setup, cache=F, include=F-------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=4.5)
options(dplyr.print_min=10)

## ----cache=F, message=F--------------------------------------------------
options(java.parameters="-Xmx2g")   # optional, but more memory for Java helps
library("dfrtopics")
library("dplyr")
library("ggplot2")
library("lubridate")
library("stringr")

## ------------------------------------------------------------------------
data_dir <- file.path(path.package("dfrtopics"), "test-data",
                      "pmla-modphil1905-1915")

## ------------------------------------------------------------------------
metadata_file <- file.path(data_dir, "citations.tsv")
meta <- read_dfr_metadata(metadata_file)

## ------------------------------------------------------------------------
counts <- read_wordcounts(list.files(file.path(data_dir, "wordcounts"),
                                     full.names=T))

## ------------------------------------------------------------------------
counts <- semi_join(counts,
        meta %>%
            select(id, pubdate) %>%
            filter(year(pubdate) != 1905),
    by="id")

## ------------------------------------------------------------------------
counts <- counts %>%
    group_by(id) %>%
    filter(sum(weight) > 300)

## ------------------------------------------------------------------------
stoplist_file <- file.path(path.package("dfrtopics"), "stoplist",
                           "stoplist.txt")
stoplist <- readLines(stoplist_file)
counts %>%
    group_by(id) %>%
    summarize(total=sum(weight),
              stopped=sum(weight[word %in% stoplist]))

## ------------------------------------------------------------------------
counts <- counts %>% wordcounts_remove_stopwords(stoplist)

## ------------------------------------------------------------------------
counts <- counts %>%
    wordcounts_remove_rare(20000)

## ------------------------------------------------------------------------
counts <- counts %>%
    group_by(word) %>%
    filter(sum(weight) > 3)

## ----eval=F--------------------------------------------------------------
#  # not run for this vignette's example
#  library("SnowballC")
#  counts <- counts %>%
#      mutate(word=wordStem(word)) # English stemmer

## ------------------------------------------------------------------------
docs <- wordcounts_texts(counts)

## ------------------------------------------------------------------------
ilist <- make_instances(docs)

## ----message=F-----------------------------------------------------------
m <- train_model(ilist, n_topics=40,
                 n_iters=300,
                 seed=1066,       # "reproducibility"
                 metadata=meta    # optional but handy later
                 # many more parameters...
                 )

## ----message=F-----------------------------------------------------------
write_mallet_model(m, "modeling_results")

## ----eval=F--------------------------------------------------------------
#  m <- load_mallet_model_directory("modeling_results",
#      metadata_file=metadata_file)

## ------------------------------------------------------------------------
m <- load_mallet_model_directory("modeling_results",
    load_topic_words=T,
    metadata_file=metadata_file)

## ------------------------------------------------------------------------
summary(m)

## ------------------------------------------------------------------------
top_words(m, n=10) # n is the number of words to return for each topic

## ------------------------------------------------------------------------
topic_labels(m, n=8)

## ------------------------------------------------------------------------
dd <- top_docs(m, n=3)
head(dd)

## ------------------------------------------------------------------------
ids <- doc_ids(m)[dd$doc[dd$topic == 35]]
metadata(m) %>%
    filter(id %in% ids) %>%
    cite_articles()

## ------------------------------------------------------------------------
srs <- topic_series(m, breaks="years")
head(srs)

## ------------------------------------------------------------------------
journal <- factor(metadata(m)$journal)
doc_topics(m) %>%
    sum_row_groups(journal) %>%
    normalize_cols()

## ------------------------------------------------------------------------
top_words(m, n=10) %>%
    plot_top_words(topic=3)

## ----fig.height=14, fig.width=14, out.width="600px", out.height="600px"----
topic_scaled_2d(m, n_words=2000) %>%
    plot_topic_scaled(labels=topic_labels(m, n=3))

## ----fig.height=8--------------------------------------------------------
theme_update(strip.text=element_text(size=7),  # optional graphics tweaking
             axis.text=element_text(size=7))
topic_series(m) %>%
    plot_series(labels=topic_labels(m, 2))

## ------------------------------------------------------------------------
d <- read_diagnostics(file.path("modeling_results", "diagnostics.xml"))
which.min(d$topics$corpus_dist)
# in terms of standard deviations from the mean distance:
sort(scale(d$topics$corpus_dist))[1:3]

## ----eval=F--------------------------------------------------------------
#  topic_report(m, "plots")

## ----eval=F--------------------------------------------------------------
#  export_browser_data(m, "browser", download_dfb=T)

## ------------------------------------------------------------------------
w <- "poem"

## ------------------------------------------------------------------------
m <- load_sampling_state(m,
    simplified_state_file=file.path("modeling_results", "state.csv"))

## ------------------------------------------------------------------------
sampling_state(m)
dim(sampling_state(m))

## ------------------------------------------------------------------------
topic_docs <- topic_docs_word(m, w)

## ------------------------------------------------------------------------
doc_years <- metadata(m)$pubdate %>%
    cut.Date(breaks="years")

## ------------------------------------------------------------------------
series <- sum_col_groups(topic_docs, doc_years)

## ------------------------------------------------------------------------
total_series <- t(doc_topics(m)) %>%
    sum_col_groups(doc_years) %>%
    colSums()

## ------------------------------------------------------------------------
series <- series %>%
    rescale_cols(1 / total_series)

## ------------------------------------------------------------------------
series_frame <- series %>%
    gather_matrix(col_names=c("topic", "year", "weight"))

## ------------------------------------------------------------------------
series_frame <- semi_join(series_frame,
    words_top_topics(m, 4) %>%
        filter(word == w),
    by="topic")

## ------------------------------------------------------------------------
series_frame %>%
    mutate(topic=factor(topic_labels(m, 3)[topic])) %>% 
    mutate(year=as.Date(year)) %>%  # restore data type (sigh)
    ggplot(aes(year, weight, group=topic, fill=topic)) +
        geom_area() +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('allocation of "', w, '" among topics'))

