# dfrtopics

This small R package provides bits and pieces to help make and explore topic models of text, especially word-count data like that available from JSTOR's [Data for Research](http://dfr.jstor.org) (DfR) service. It uses [MALLET](http://mallet.cs.umass.edu) to run the models.

I wrote most of the bits and pieces here while working on my research (I am a literary scholar), so this is not meant to be a professional, sophisticated, multipurpose tool. Nonetheless, by now it seemed worth it to make some of what I'd done conceivably reusable by others who might also want to explore topic models even if, like me, like to use R and have only very rudimentary knowledge of machine learning. The code skews to my amateurishness as a programmer. It is all very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk. 

Every function has online help in R. There is an accompanying vignette (`vignette("introduction", "dfrtopics")`, but see below for notes on generating it. In the meantime it is viewable [here](https://raw.githubusercontent.com/agoldst/dfrtopics/master/inst/doc/introduction.html).

## Installation

This is too messy for CRAN. The easiest way to install is to first install the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package, and then use it to install this package straight from github:

```R
library(devtools)
install_github("dfrtopics", "agoldst")
```

(This should work even if you don't have git or a github account.)

I have been profligate with dependencies. Note that if you use RStudio, getting rJava and mallet to load can be a messy business. See my [blog post on rJava and RStudio on MacOS X](http://andrewgoldstone.com/blog/2015/02/03/rjava/).

## Browsing the model interactively

Now in alpha release: another project of mine, [dfr-browser](http://agoldst.github.io/dfr-browser), which makes topic models of DfR data into a javascript-based interactive browser.

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

## Running the package tests

The tests are based on a sample set of data from DfR. I do not currently have permission to distribute that data, but you can recreate it if you wish to run the tests or regenerate the package vignette. Perform [this search in DfR](http://dfr.jstor.org/fsearch/submitrequest?cs=jo%3A%28pmla+OR+%22modern+philology%22%29+AND+year%3A%5B1905+TO+1915%5D+AND+ty%3Afla%5E1.0&fs=yrm1&view=text&) and make a Dataset Request for wordcounts and metadata in CSV format. Then unzip the archive to a directory `test-data` inside the package directory for `dfrtopics`.

## Version history

v0.2
 :  New release. An almost completely rewritten API, so don't expect backwards compatibility. This version should be more flexible and easier to use.

v0.1
 :  Earliest public version(s), 2013--2015

