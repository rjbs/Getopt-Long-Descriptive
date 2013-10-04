#!perl
use strict;
use warnings;

use Test::More;
use Test::Warnings 0.005 qw[ warning ];
use Test::Fatal;

use Getopt::Long::Descriptive;

my $nreq = 6;
my @reqs = map { "--req$_" } 1 .. $nreq;

sub _args {
  "test %o",
  # load with extra required to make sure
  # sorting on shortcircuit attribute works;
  (map { [ "req$_", 'required', { required => 1 } ] } 1 .. $nreq),
  [ 'help', 'help', { shortcircuit => 1, @_ } ],
}

like(
  exception { describe_options( _args() ) },
  qr/required/,
  'no req: error',
);

like(
  warning {
    local @ARGV = @reqs;
    describe_options( _args( default => 1 ) );
  },
  qr/'default' does not make sense for shortcircuit/,
  'shortcircuit + default'
);

SKIP: {
  my $opt;

  is(
    exception {
      local @ARGV = @reqs;
      ( $opt ) = describe_options( _args() );
    },
    undef,
    'req: no error'
  ) or skip( 'no object due to failure', 1 );

  ok( defined $opt->req1 && $opt->req1 == 1, 'req: req1 specified' );
}

SKIP: {
  my $opt;

  is(
    exception {
      local @ARGV = qw[ --help ];
      ( $opt ) = describe_options( _args() );
    },
    undef,
    'help: no error'
  ) or skip( 'no object due to failure', 2 );

  is( $opt->help,          1, 'help: help flag' );
  is( scalar keys %{$opt}, 1, 'help: only help' );
}

SKIP: {
  my ( $w, $opt );

  is(
    exception {
      local @ARGV = qw[ --help ];
      ( $opt ) = describe_options( _args( @reqs ) );
    },
    undef,
    'help + req: no error'
  ) or skip( 'no object due to failure', 2 );

  is( $opt->help,          1, 'help + req: help flag' );
  is( scalar keys %{$opt}, 1, 'help + req: only help' );
}

done_testing;
