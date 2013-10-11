"""word_lengths.py

A simple script to make a table of document lengths, in words. 

USAGE: python word_lengths.py dir1 dir2 dir3 ...

Looks for .CSV files in dir1, dir2, dir3. Assumes all such files are DfR wordcount files. For each such file, prints out a single line of the form:

filename,length

by summing the WEIGHT column.
"""

from glob import glob
import os.path

def main(script,*dirs):
    files = []
    for d in dirs:
        files.extend(glob(os.path.join(d,"*.CSV")))

    # header line
    print "filename,length"

    for f in files:
        wordlen = 0
        with open(f) as countfile:
            for l in countfile:
                if l == "WORDCOUNTS,WEIGHT\n":
                    continue 
                wordcount,weight = l.strip().split(',')
                wordlen += int(weight)
        print "{},{}".format(f,wordlen)


if __name__=="__main__":
    import sys
    main(*sys.argv)
    


