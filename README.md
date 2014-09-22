# dfrtopics

This small R package provides bits and pieces to help make and explore topic models of the data available from JSTOR's [Data for Research](http://dfr.jstor.org) (DfR) service. It uses [MALLET](http://mallet.cs.umass.edu) to run the models and [ggplot2](http://ggplot2.org/) for visualizations.

The functions skew to my needs as a literary scholar and digital-humanist-in-progress. The code skews to my amateurishness as a programmer. It is all very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk. 

Every function has online help in R. For an introduction, try `help("dfrtopics")`. I plan to write a fuller tutorial soon. The documentation will also be available on the web.

## Installation

For now, this is too messy for CRAN. The easiest way to install is to first install the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package, and then use it to install this package straight from github:

```R
library(devtools)
install_github("dfrtopics","agoldst")
```

(This should work even if you don't have git or a github account.)

I have been profligate with dependencies. Note that if you use RStudio, getting rJava to load can be a messy business. On my system (MacOS X 10.7.5) I have been forced to use the following shell alias to open RStudio:

```
alias rstudio="DYLD_FALLBACK_LIBRARY_PATH=/Library/Java/JavaVirtualMachines/jdk$(java -version 2>&1 | head -n 1 | cut -d'"' -f 2).jdk/Contents/Home/jre/lib/server/: open -a RStudio"
```

(The `$(...)` bit is to avoid hardcoding the Java version.)

## Basic example usage within R

Let's imagine you have downloaded and unzipped the results of a DfR request to 
`~/dfr/test/`: you have metadata in `~/dfr/test/citations.CSV`,
wordcounts per document in `~/dfr/test/wordcounts/*.CSV`, and a stoplist in `stoplist/long.txt`.

```R
library(dfrtopics)
# Run the model
m <- model_documents(citations_files="~/dfr/test/citations.CSV",
         dirs="~/dfr/test/wordcounts/",stoplist_file="stoplist/long.txt",
         n_topics=40)
# Get doc-topic matrix joined with metadata
dtw <- doc_topics_wide(m$doc_topics,m$metadata)
# Convert that into a data frame of topic yearly time series
series <- topic_proportions_series_frame(topic_year_matrix(dtw))
# Make a faceted plot
topic_yearly_lineplot(series,facet=T)
```


## Browsing the model interactively

Now in alpha release: another project of mine, [dfr-browser](http://agoldst.github.io/dfr-browser), which makes topic models of DfR data into a javascript-based interactive browser.

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

## Running the package tests

The tests are based on a sample set of data from DfR. I do not currently have permission to distribute that data, but you can recreate it if you wish to run the tests. My test data came from downloading the results of the following query: <http://dfr.jstor.org/fsearch/submitrequest?cs=year%3A%5B1980+TO+1985%5D+AND+%28jcode%3APMLA+OR+jcode%3Aelh%29%5E1.0&view=text&>, then randomly selecting five hits from each journal in each year. The chosen files are listed by id in `dfr_data/ids.txt`. As long as JSTOR does not change its wordcounts for those documents, if you download them and their associated metadata and put them where the tests expect to find them, you should be able to get the tests that depend on them to pass.

In 2014 JSTOR altered their metadata format. A further test uses the results of a newer query; see `dfr_data_new/MANIFEST.txt` for the query.

Eventually I will substitute fake test data that I can distribute with the package.


