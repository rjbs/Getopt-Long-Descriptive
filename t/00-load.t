#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Getopt::Long::Descriptive' );
}

diag( "Testing Getopt::Long::Descriptive $Getopt::Long::Descriptive::VERSION, Perl $], $^X" );
