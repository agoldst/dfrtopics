"""
Get corpus totals from DfR data. A Python alternative to
dfrtopics::overall_counts, which requires loading all count-files into
memory. Prints the results as a CSV (with headers "word,weight") to
standard output.

Usage:
    term_totals.py <dir>
    term_totals.py [-h | --help]

Arguments:
    <dir>   wordcounts directory containing CSV files

Options:
    -h, --help  show help
"""

from docopt import docopt
import os
from collections import defaultdict

def totals(d):
    result = defaultdict(int)
    for filename in os.listdir(d):
        with open(os.path.join(d,filename)) as f:
            first = True
            for line in f:
                if first:
                    first = False
                    continue
                term, weight = line.strip().split(",")
                result[term] += int(weight)

    print "word,weight"
    for term in result:
        print "{},{}".format(term,result[term])

if __name__=="__main__":
    args = docopt(__doc__)      # automatically deals with -h | --help
    totals(args["<dir>"])
