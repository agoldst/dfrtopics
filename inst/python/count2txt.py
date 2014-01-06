"""count2txt.py

Turn JSTOR wordcounts*.CSV files into a file suitable for mallet's import-file command.
"""

import os.path

def main(script,*files):

    for f in files:
        with open(f) as countfile:
            out_line = os.path.basename(f) + ' X '
            for l in countfile:
                if l == "WORDCOUNTS,WEIGHT\n":
                    continue 
                wordcount,weight = l.strip().split(',')
                out_line += (wordcount + ' ') * int(weight)
            print out_line


if __name__=="__main__":
    import sys
    main(*sys.argv)
    



