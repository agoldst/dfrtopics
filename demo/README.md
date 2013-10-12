# Demo: modeling *PMLA*

Here is an example of how one might use the functions in this repository to make topic models of DfR data somewhat systematically. The Makefile gives the top-level steps:

1. `make instances/docs.mallet`
2. `make model`

These two make targets simply source the files `make_instance.R` and `make_model.R` in turn, calling the correspondingly named functions of each. 
The result is a set of CSV files in `model` giving the estimated topic model. 

If you wished to tweak what goes on here, you could start by tweaking the parameters to those two functions. As an example, I have used variables in the Makefile to set the number of topics and the number of sampling iterations.

The output files are not identical to those output by the mallet command-line tool; they are instead in formats that I have found convenient for reading into R and analyzing from there, and they correspond, in general, to what many of the functions in `dfr-analysis/topics_rmallet.R` and `dfr-analysis/topics_vis.R` expect. They are also ready for postprocessing for my [topic browser](http://github.com/agoldst/dfr-browser).

## The data

To run these scripts as is, clone `dfr-analysis`, then place the results of a DfR download of *PMLA* articles in a subdirectory `pmla_all` of this folder (`dfr-analysis/demo`). I believe I cannot redistribute the data. Here is a link to a DfR query for the requisite wordcounts for the journal *PMLA*: <http://dfr.jstor.org/?view=text&qk0=ft&qw0=1.0&qv0=jcode%3Apmla&qf0=any>. If you download this data in smaller chunks, modify the `journal_dirs` parameter to `make_instance()` to be a list of names of the directories where the chunks live (my scripts take care of merging the metadata).

## Reproducibility

Notice that `make_model()` sets MALLET's random seed. Though the
algorithm is probabilistic, with this setting the results will be the
same every time. To do the more usual thing and set a new random seed in
each run, pass `seed=NULL` as a parameter to `make_model()`.

As a further wrinkle, the data itself presumably a moving target, as *PMLA*'s embargo window shifts. JSTOR can also, of course, regenerate its OCR at any time. Keep those MANIFEST.txt files from JSTOR! 

## Notes on authorship

The stoplist, `stoplist.txt`, and the British-to-American regularization rules, `uk_us.csv`, draw on those used in [Ted Underwood's and my joint project](http://github.com/agoldst/tmhls). The code for running mallet in `make_instance.R` and `make_model.R` is also derived from code written for that project.

