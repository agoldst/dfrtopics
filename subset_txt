#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: subset_txt.pl
#
#        USAGE: ./subset_txt.pl  
#
#  DESCRIPTION: 
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Andrew Goldstone (agoldst), andrew.goldstone@gmail.com
# ORGANIZATION: Rutgers University, New Brunswick
#      VERSION: 1.0
#      CREATED: 11/15/2012 16:09:43
#     REVISION: ---
#===============================================================================

use v5.14;                                  # entails strict, unicode_strings 
use autodie;
use utf8;                                   # source code itself is in utf-8
use warnings;
use warnings FATAL => "utf8";               # Unicode encode errors are fatal
use open qw( :std :utf8 );                  # default utf8 layer

my $subset_file = shift;
open SUBSET, "< :encoding(UTF-8)", $subset_file
    or die "Couldn't open $subset_file";

my @subset_filenames = <SUBSET>;
close SUBSET;

chomp @subset_filenames;
my %subset;
$subset{$_} = 1 for(@subset_filenames);

while(<STDIN>) {
    my ($countfile) = /^(\S+)/;
    die "ill-formed line: $!" unless $countfile;
    print $_ if $subset{$countfile}; 
}
