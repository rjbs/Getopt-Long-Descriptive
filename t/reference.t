#!perl
use strict;
use warnings;

use Getopt::Long::Descriptive;
use Test::More;

local @ARGV;
my ($opt, $usage) = describe_options(
  "%c %o",
  (
    [ 'constrained_list|s=s{2,4}'   => "constrained list of strings" ],
    [ 'list|l=s@'    => "list of strings" ],
    [ 'hash|h=s%'    => "hash values" ],
  ),
  {
        getopt_conf => ['no_bundling']
  }    
);

is (ref($opt->{'constrained_list'}), 'ARRAY', 'constrained list is ARRAY');
is (ref($opt->{'list'}), 'ARRAY', 'list is ARRAY');
is (ref($opt->{'hash'}), 'HASH', 'hash is HASH');

done_testing;