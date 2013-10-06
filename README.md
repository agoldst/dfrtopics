# dfr-analysis

Bits and pieces to help analyze the data available from JSTOR's Data for Research (DfR) service. Skews to my needs as a literary scholar and digital-humanist-in-progress. Skews to my amateurishness as a programmer. Very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk. 

## metadata.R

Functions for manipulating metadata from DfR with R. The core function is `read_metadata()` for reading and combining `citations.CSV` files. I make particularly frequent use of `pubdate_Date()`, which converts the "pubdate" field to `Date` objects. For browsing, `cite_articles()` turns rows of metadata into a human-readable citation.


## topics_rmallet.R

Thanks to the advent of [the mallet interface for R](http://www.cs.princeton.edu/~mimno/R/), it's possible to avoid having to do the kinds of reprocessing of the output from the mallet command-line tool that I used to need to do (using the perl scripts listed below). In fact it's possible to run mallet itself from within R. This script provides basic functions for using mallet to topic-model wordcount data from DfR, and then to read and write the results of that output, as well as to tally it up by year. The functions in this file are explained in comments.

I also provide some functions for reading mallet's "instances" from within R, so that you can recover the documents you fed to the topic modeler (in post-processed form, i.e. with stopwords removed, etc.).

I have not been parsimonious about using other R packages, so there are a lot of dependencies here.

### Basic example usage within R

```R
# let's imagine you have this repository in ~/projects/dfr-analysis
# Download and unzip DFR data to, say ~/dfr/test/
# you get metadata in ~/dfr/test/citations.CSV
# and wordcounts per document in ~/dfr/test/wordcounts/*.CSV

setwd("~/projects/dfr-analysis")
source("topics_rmallet.R")
topics_rmallet_setup()    # loads metadata.R, rJava, other dependencies
m <- model_documents(citations.file="~/dfr/test/citations.CSV",
    dirs="~/dfr/test/wordcounts/",stoplist.file="stoplist/long.txt",
    num.topics=12)

source("topics_vis.R")
tm_wide <- doc_topics_wide(m$doc_topics,m$metadata)
tm_yearly_line_plot(tm_wide=tm_wide,facet=T)
```

## topics_vis.R

This script provides some `ggplot2` visualizations that make use of various aspects of the mallet output, with an emphasis on following topics over time. The `topic_keyword_plot()` gives a way of looking at "top key words" for a topic that preserves information about their weights. `topic_report()` generates a folder of images (multiple plots each) giving an overview of each topic in the model.

There are also some basic visualizations of the words in the corpus, which need various slices of the term-document matrix and the term-document-topic array (computed by functions in `topics_rmallet.R`).

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

## Browsing the model

I have split off the rudimentary web-browser-based topic-model-browser I began as a subproject here into a separate repository, [dfr-browser](http://github.com/agoldst/dfr-browser).

## Legacy scripts

### topics.R

Basic R script for loading information from a topic model created by MALLET, getting basic information from it, and making time-series plots of various kinds. In progress....or it was.

*Note.* These functions were written to use in conjunction with the other "legacy scripts" below. I am moving to the R interface for mallet (see above) and so have stopped using the data-reading functions in this file. Since I gather a few people have been making use of some of this code, I have left the legacy functions in. But this repository is really for my own use, so I won't maintain backwards compatibility longer than I feel like it.

### combine_citations

A simple shell script to combine multiple `citations.CSV` metadata files
from jstor and get rid of any duplicate lines. Usage:

    combine_citations citations1.CSV citations2.CSV ... > citations_all.csv

Sanity-check results before using.

### count2txt

*Now deprecated*. This same "inflating" of word-count data into bags of words is accomplished by the read_dfr_wordcounts() function in topics_rmallet.R. If you can use that, you can avoid the embarrassment of writing lots of highly redundant files to disk.

*Old explanation follows:* I like to play around with the topic modeling tool [MALLET](http://mallet.cs.umass.edu). I have yet to figure out how to get `mallet` at the command line to accept documents in the form of wordcounts, which is how jstor DfR provides them. `mallet` wants to count words itself. So this script lets you! Fun and easy! takes `wordcount*.CSV` files from jstor and "inflates" them into bags of words. By default, will make you one ginormous file, with each "bag" on a line. This is suitable for input to the `mallet import-file` command. Usage:

    count2txt wordcounts*.CSV > wordbags.txt

I often use this on thousands of `wordcounts*.CSV` files at once, in which case shell expansion at the command line isn't so great. So actually a more typical usage is:

    cd wordcounts; ls -1 | xargs ~/dfr-analysis/count2txt > wordbags.txt

Then you can do:

    mallet import-file --input wordbags.txt --label 0 --keep-sequence --output dfr.mallet
    mallet train-topics --input dfr.mallet ....


### subset_txt

Perl script to output a subset of the bag-of-words collection produced by count2txt. 

### sort_doc_topics

Perl script to convert the result of

    mallet train-topics --output-doc-topics filename.tsv

into a table with columns ordered by topic number. This can then be fed into the ``read.sorted.doc.topics`` method in ``topics.R``.

