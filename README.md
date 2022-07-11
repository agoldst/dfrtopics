# dfrtopics

This small R package provides bits and pieces to help make and explore topic models of text, especially word-count data. It relies on [MALLET](http://mallet.cs.umass.edu) to create the models. I originally wrote this package to study data from JSTOR's Data for Research (DfR) service. DfR has now been replaced by a more elaborate "platform," [Constellate](https://constellate.org), which supplies similar data in a different format. This package can still be used to support topic-model exploration from R, but you are on your own as far as processing the new Constellate output format goes.

Most of my work on this was in 2013–2016, with some sporadic additions through 2019. That's ages in computer years. Some of the package dependencies have started to accumulate cobwebs, including the R interface for MALLET. Some package functions use now-deprecated "[tidyverse](https://tidyverse.org)" idioms; they currently work, but you'll see warnings about this if you use the package. I am not actively maintaining this package, so if you wish to use it, good luck to you. For general text-data-manipulation purposes, take a look at the [tidytext](https://www.tidytextmining.com) package. A more recent and more flexible variant of topic modeling itself, called Structural Topic Modeling, is implemented in the [stm](https://www.jstatsoft.org/article/view/v091i02) package, which also has a rich set of tools for exploring and validating the models it makes. For whatever's trendy in machine learning these days, I have no idea, but I'm sure someone has a large language model with a bridge in Brooklyn to sell you.

I am a literary scholar and I wrote this for my own research purposes, so this is not meant to be a professional, sophisticated, multipurpose tool. Nonetheless, it seemed worth it to make some of what I'd done conceivably reusable by others who might also want to explore topic models even if they, like me, know very little about machine learning. The code skews to my amateurishness as a programmer. It is just hacked together. I am not an expert, I am not a lawyer, etc., etc., etc. Use and share freely, at your own risk.

Every function has online help in R. For a fairly detailed introduction to what you can do with this package, see the introductory vignette:  `vignette("introduction", "dfrtopics")` or [online here](http://agoldst.github.io/dfrtopics/introduction.html). I'm always happy to hear from anyone who makes use of this.

## Installation

This is too messy for CRAN. The easiest way to install is to first install the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package, and then use it to install this package straight from github:

```R
devtools::install_github("agoldst/dfrtopics")
```

(You don't need to have git or a github account for this to work.) I have been profligate with dependencies on other R packages. This package contains C++ code, so R must be able to build it from source. Mac users should install XCode; Windows users need [RTools](https://cran.r-project.org/bin/windows/Rtools/).

Note, however, that the [mallet](http://cran.r-project.org/web/packages/mallet) package is *not* a formal dependency of the package, so as to make it possible to use other parts of the passage without dealing with Java or loading the Java VM. To make use of the topic-modeling functions, however, mallet must be installed. The package was available from CRAN but has currently (July 2022) been pulled, so it can be installed as follows:

```R
devtools::install_github("mimno/RMallet", subdir="mallet")
```

This will also _try_ to install rJava, which mallet depends on. At this point you may be forced into some yak-shaving. Remember when Java was the great solution to cross-platform programming? Talk about long times in computer years. On my current OS (macOS Monterey) I got a working java that cooperated with rJava with `brew install java`.

To install this package together with its optional as well as required dependencies (CRAN mallet included), use

```R
devtools::install_github("agoldst/dfrtopics", dependencies=T)
```

## Browsing the model interactively

Another project of mine, [dfr-browser](http://agoldst.github.io/dfr-browser), makes topic models from this package into a javascript-based interactive browser. To browse a model created in dfrtopics, use the package function `dfr_browser` (see the function documentation for more detail).

## A note on licensing

I have decided to apply the [MIT License](https://github.com/agoldst/dfr-analysis/tree/master/LICENSE) to this repository. That means you can pretty much do anything you want with it, provided you attribute stuff by me to me. And you can't hold me liable. I prefer the spirit of the GNU Public License, but I would like academics who use this code to be able to do so without being obliged to release their source, since that it is not always possible. I don't attempt to forbid commercial uses, but I don't welcome them.

## Running the package tests

The tests are based on a sample set of data from DfR, in the repository under `test-data`. Since DfR has been shut down, these tests are of dubious usefulness.

## Version history

v0.2.5a
 : 7/1/22. Maintenance.

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
