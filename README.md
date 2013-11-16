# dfrtopics

This small R package provides bits and pieces to help make and explore topic models of the data available from JSTOR's [Data for Research](http://dfr.jstor.org) (DfR) service. It uses [MALLET](http://mallet.cs.umass.edu) to run the models and [ggplot2](http://ggplot2.org/) for visualizations.

The functions skew to my needs as a literary scholar and digital-humanist-in-progress. The code skews to my amateurishness as a programmer. It is all very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk. 

Every function has online help in R. For an introduction, try `help("dfrtopics")`. I plan to write a fuller tutorial soon. The documentation will also be available on the web.

## Basic example usage within R

Let's imagine you have downloaded and unzipped the results of a DfR request to 
`~/dfr/test/`: you have metadata in `~/dfr/test/citations.CSV`
and wordcounts per document in `~/dfr/test/wordcounts/*.CSV`.

```R
library(dfrtopics)
# Run the model
m <- model_documents(citations_files="~/dfr/test/citations.CSV",
         dirs="~/dfr/test/wordcounts/",stoplist_file="stoplist/long.txt",
         num_topics=40)
# Get doc-topic matrix joined with metadata
dtw <- doc_topics_wide(m$doc_topics,m$metadata)
# Convert that into a data frame of topic yearly time series
series <- topic_proportions_series_frame(topic_year_matrix(dtw))
# Make a faceted plot
topic_yearly_lineplot(series,facet=T)
```


## Browsing the model interactively

Now in alpha release: another project of mine, [dfr-browser](http://github.com/agoldst/dfr-browser), which makes topic models of DfR data into a javascript-based interactive browser.

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

