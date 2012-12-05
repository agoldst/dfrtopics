#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: write_gephi.pl
#
#        USAGE: ./write_gephi.pl  
#
#  DESCRIPTION: Convert a matrix of distances saved from R into a list of edges
#  for Gephi. Simply writes out a pair of indices whenever the that element
#  of the distance matrix is less than the THRESHOLD set at the start of the 
#  script. In order to be used in gephi, you also need an accompanying
#  node list that translates from row indices (0...n-1) of this matrix to 
#  whatever it is that you took distances of (topics, say).
#
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Andrew Goldstone (agoldst), andrew.goldstone@gmail.com
# ORGANIZATION: Rutgers University, New Brunswick
#      VERSION: 1.0
#      CREATED: 12/05/2012 15:23:47
#     REVISION: ---
#===============================================================================

my $THRESHOLD = 0.1;

my $header = <STDIN>;

# output header
print "Source,Target,Type,Weight\n";

# expects first column to be an id number indexed from 0 
while(<STDIN>) {
    chomp;
    my ($id,@fields) = split ",";

    for($i = $id + 1;$i < @fields;$i++) {
        if($fields[$i] < $THRESHOLD) {
            print "$id,$i,Undirected,$fields[$i]\n"; 
        }
    } 
}
