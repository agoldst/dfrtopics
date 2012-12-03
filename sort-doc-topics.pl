#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: sort-doc-topics.pl
#
#        USAGE: ./sort-doc-topics.pl  
#
#  DESCRIPTION: read mallet doc-topics output from stdin, write a tab-delimited 
#  file with the topic proportion data ordered by topic number to stdout
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Andrew Goldstone (agoldst), andrew.goldstone@gmail.com
# ORGANIZATION: Rutgers University, New Brunswick
#      VERSION: 1.0
#      CREATED: 07/24/2012 17:12:29
#     REVISION: ---
#===============================================================================
use v5.14;                                  # entails strict, unicode_strings 
use autodie;
use utf8;                                   # source code itself is in utf-8
use warnings;
use warnings FATAL => "utf8";               # Unicode encode errors are fatal
use open qw( :std :utf8 );                  # utf8 layer for open, stdin/out

# If the columns of the csv are 0, 1, 2, ...
# and there are 100 topics,
# then the topic numbers are at positions 2, 4, ... 200
# and the proportions are at 3, 5, ... 201
# but topics themselves are numbered 0 .. 99

my $N_TOPICS = 100;
my @top_indices = map { $_ * 2 } (1..$N_TOPICS);

# Read and discard the header
my $header_line = <STDIN>;

while (<STDIN>) {
    my @row = split /\t/;

    # hash saying which topics are in which columns of this row

    my %indexmap = ();
    $indexmap{$row[$_]} = $_ foreach (@top_indices);

    # keep the first two columns
    
    print "$row[0]\t$row[1]";

    # now output the topic proportions in order

    for (my $i = 0;$i <= $N_TOPICS - 1;$i++) {
        print "\t" . $row[$indexmap{$i} + 1] 
    }

    print "\n";
}
