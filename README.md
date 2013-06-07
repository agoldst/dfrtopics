# dfr-analysis

Bits and pieces to help analyze the data available from JSTOR's Data for Research (DfR) service. Skews to my needs as a literary scholar and digital-humanist-in-progress. Skews to my amateurishness as a programmer. Very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk. 

## metadata.R

Functions for manipulating metadata from DfR with R.

## topics.R

Basic R script for loading information from a topic model created by MALLET, getting basic information from it, and making time-series plots of various kinds. In progress.

*Note.* These functions were written to use in conjunction with the "legacy scripts" below. I am moving to the R interface for mallet (see below) and so am using the data-reading functions in this file less and less. Since I gather a few people have been making use of some of this code, I have left the legacy functions in. But this is really for my own use, so I won't maintain backwards compatibility longer than I feel like it.

## topics_rmallet.R

Thanks to the advent of [the mallet interface for R](http://www.cs.princeton.edu/~mimno/R/), it's possible to avoid having to do the kinds of reprocessing of the output from the mallet command-line tool that I used to need to do (using the perl scripts listed below). In fact it's possible to run mallet itself from within R. This script provides basic functions for using mallet to topic-model wordcount data from DfR. I use it to put together a data frame with document metadata and document-topic proportions, which can then work with the same plotting functions in topics.R. But much more is possible, given access, within R, to the mallet topic model object itself.

### basic example usage within R

```R
# let's imagine you have this repository in ~/projects/dfr-analysis
# Download and unzip DFR data to, say ~/dfr/test/
# you get metadata in ~/dfr/test/citations.CSV
# and wordcounts per document in ~/dfr/test/wordcounts/*.CSV

setwd("~/projects/dfr-analysis")
source("topics_rmallet.R")
model <- model_documents(citations.file="~/dfr/test/citations.CSV",
    dirs="~/dfr/test/wordcounts/",stoplist.file="stoplist/long.txt",
    num.topics=12)

library(ggplot2)
plot.topics.yearly(1:12,docmodel$tm,docmodel$kf)
```


## Legacy scripts

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

## More to come!

