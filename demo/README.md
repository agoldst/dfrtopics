# A minimal demo

## The data

I believe I cannot redistribute the data. Here is a link to the dfr query necessary to access the requisite wordcounts for the journal *PMLA*: <http://dfr.jstor.org/?view=text&qk0=ft&qw0=1.0&qv0=jcode%3Apmla&qf0=any>. This will need to be downloaded in chunks of 1000 documents (use date limits). As a further wrinkle, this data is presumably a moving target, as PMLA's embargo window shifts. JSTOR can also, of course, regenerate its OCR at any time.

## Notes on authorship

The stoplist, `stoplist.txt`, and the British-to-American regularization rules, `uk_us.csv`, draw on those used in [Ted Underwood's and my joint project](http://github.com/agoldst/tmhls). The code for running mallet is also derived from code written for that project.
