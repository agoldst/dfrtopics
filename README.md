# dfrtopics

This small R package provides bits and pieces to help make and explore topic models of text, especially word-count data like that available from JSTOR's [Data for Research](http://dfr.jstor.org) (DfR) service. It uses [MALLET](http://mallet.cs.umass.edu) to run the models.

I wrote most of the bits and pieces here while working on my research (I am a literary scholar), so this is not meant to be a professional, sophisticated, multipurpose tool. Nonetheless, by now it seemed worth it to make some of what I'd done conceivably reusable by others who might also want to explore topic models even if they, like me, know very little about machine learning. The code skews to my amateurishness as a programmer. It is all very much in-progress, hacked together, catch-as-catch-can, I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk.

Every function has online help in R. For a fairly detailed introduction to what you can do with this package, see the introductory vignette:  `vignette("introduction", "dfrtopics")` or [online here](http://agoldst.github.io/dfrtopics/introduction.html). I'm always happy to hear from anyone who makes use of this.

## Installation

This is too messy for CRAN. The easiest way to install is to first install the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package, and then use it to install this package straight from github:

```R
devtools::install_github("agoldst/dfrtopics")
```

(You don't need to have git or a github account for this to work.) I have been profligate with dependencies on other R packages. This package contains C++ code, so R must be able to build it from source. Mac users should install XCode; Windows users need [RTools](https://cran.r-project.org/bin/windows/Rtools/).

Note, however, that the [mallet](http://cran.r-project.org/web/packages/mallet) package is *not* a formal dependency of the package, so as to make it possible to use other parts of the passage without loading the Java VM. To make use of the topic-modeling functions, however, mallet must be installed. It is available in two versions. To install from CRAN, use

```R
install.packages("mallet")
```

To install a more recent development version of mallet, use

```R
devtools::install_github("mimno/RMallet", subdir="mallet")
```

Either command will also install rJava, which mallet depends on. If you use RStudio, getting rJava and mallet to load can be a messy business. Mac users, see my [blog post on rJava and RStudio on MacOS X](http://andrewgoldstone.com/blog/2015/02/03/rjava/).

To install this package together with its optional as well as required dependencies (CRAN mallet included), use

```R
devtools::install_github("agoldst/dfrtopics", dependencies=T)
```

## Browsing the model interactively

Now in alpha release: another project of mine, [dfr-browser](http://agoldst.github.io/dfr-browser), which makes topic models of DfR data into a javascript-based interactive browser. To browse a model created in dfrtopics, use the package function `dfr_browser` (see the function documentation for more detail).

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

## Running the package tests

The tests are based on a sample set of data from DfR. I do not currently have permission to distribute that data, but you can recreate it if you wish to run the tests or regenerate the package vignette. Perform [this search in DfR](http://dfr.jstor.org/fsearch/submitrequest?cs=jo%3A%28pmla+OR+%22modern+philology%22%29+AND+year%3A%5B1905+TO+1915%5D+AND+ty%3Afla%5E1.0&fs=yrm1&view=text&) and make a Dataset Request for wordcounts and metadata in CSV format. Then unzip the archive to a directory `test-data` inside the package directory for `dfrtopics`.

## Version history

v0.2.5
 : 8/6/19. Some support for visualization of multiple models together (sort of) using dfr-browser v0.8.1-alpha.

v0.2.4
 : 4/26/16. Compatibility with `mallet` package versions 1.0 and 1.1+. *Very experimental* topic alignment functions.

v0.2.3
 : 4/19/16. An adjusted dfr-browser export via `dfr_browser()` for one-line interactive browsing. `export_browser_data` is still available for more control. `wordcounts_instances` introduced to help express "no, MALLET, no more tokenizing!"

v0.2.2
 : 2/10/16. New (beta) feature: functions for the mutual information of words and documents within topics, and for using this in a posterior predictive check of the model fit: `imi_topic`, `mi_topic`, `imi_check`, `mi_check`. Introduces a dependency on RcppEigen.

v0.2.1
 : 9/23/15. Minor updates. `read_wordcounts` accepts a `reader` method for improved flexibility about data sources, and `export_browser_data` is more tolerant of variant metadata formats. Scaled topic coordinates now use `JS_divergence` written in C++ (introducing a direct Rcpp dependency for a questionable speed gain in a function no one uses). Various code- and documentation-cleaning tweaks.

v0.2
 :  New release, September 2015. An almost completely rewritten API, so don't expect backwards compatibility. This version should be more flexible and easier to use. At least it has more documentation.

v0.1
 :  Earliest public version(s), 2013--2015

Andrew Goldstone (<andrew.goldstone@rutgers.edu>)
