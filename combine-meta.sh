#!/bin/bash

header_line="id,doi,title,author,journaltitle,volume,issue,pubdate,pagerange,publisher,type,reviewed-work"
target_file="citations-combined.csv"

# Prepare a temporary file in /tmp for working
tmp_stem=`basename $0`
tmp_file=`mktemp /tmp/${tmp_stem}.XXXXXX` || exit 1

# take all but the first line of each command line argument
# and print them all into the temporary file

for cit in "$@"; do
    tail -q -n +2 $cit >> $tmp_file
done

echo $header_line > $target_file 

sort $tmp_file | uniq >> $target_file
