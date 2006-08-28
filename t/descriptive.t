#!perl

use strict;
use warnings;

use Test::More 'no_plan';

use_ok("Getopt::Long::Descriptive");

# test constraints:
# (look at P::V for names, too)
# required => 1
# depends => [...]
# precludes => [...]
# sugar for only_one_of and all_or_none

sub is_opt {
  my ($argv, $specs, $expect, $desc) = @_;
  local @ARGV = @$argv;
  eval { 
    my ($opt, $usage) = describe_options(
      "test %o",
      @$specs,
    );
    is_deeply(
      $opt,
      $expect,
      $desc,
    );
  }; 
  if ($@) {
    chomp($@);
    if (ref($expect) eq 'Regexp') {
      like($@, $expect, $desc);
    } else {
      # auto-fail
      is($@, "", "$desc: $@");
    }
  }
}

sub is_hidden {
  my ($specs, $cmd, $text) = @_;
  eval {
    local @ARGV;
    my ($opt, $usage) = describe_options(
      "test %o",
      @$specs,
    );
    like(
      $usage->text,
      $cmd,
      "hidden option in usage command",
    );
    unlike(
      $usage->text,
      $text,
      "hidden option description",
    );
  };
  if ($@) {
    chomp($@);
    is($@, "", "hidden: $@");
    ok(0);
  }
}

is_opt(
  [ ],
  [ [ "foo-bar=i", "foo integer", { default => 17 } ] ],
  { foo_bar => 17 },
  "default foo_bar with no short option name",
);

# test hidden

is_hidden(
  [
    [ "foo|f", "a foo option" ],
    [ "bar|b", "a bar option", { hidden => 1 } ],
  ],
  qr/test \[-f\] \[long options\.\.\.\]/i,
  qr/a bar option/,
);

### tests for one_of

my $foobar = [ 
  [ 'foo' => 'a foo option' ],
  [ 'bar' => 'a bar option' ],
];

is_opt(
  [ ],
  [ 
    [ 
      mode => $foobar, { default => 'foo' },
    ],
  ],
  { mode => 'foo' },
  "basic usage, with default",
);

is_opt(
  [ '--bar' ],
  [
    [
      mode => $foobar,
    ],
  ],
  { bar => 1, mode => 'bar' },
  "basic usage, passed-in",
);

# implicit hidden syntax
is_hidden(
  [ [ mode => [] ] ],
  qr/test\s*\n/i,
  qr/mode/,
);

is_opt(
  [ '--foo', '--bar' ],
  [ [ mode => $foobar ] ],
  #qr/\Qonly one 'mode' option (foo, bar)\E/,
  qr/it is 'foo' already/,
);

is_opt(
  [ '--no-bar', '--baz' ],
  [
    [
      mode => [
        [ foo    => 'a foo option' ],
        [ 'bar!' => 'a negatable bar option' ],
      ],
    ],
    [ 'baz!' => 'a negatable baz option' ],
  ],
  { bar => 0, mode => 'bar', baz => 1 },
  "negatable usage",
);

is_opt(
  [ ],
  [
    [ req => 'a required option' => {
      required => 1
    } ],
  ],
  qr/a required option/,
  "required option -- help text"
);

{
  local @ARGV;
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ foo => "a foo option" ],
    [],
    ['bar options:'],
    [ bar => "a bar option" ],
  );
  like(
    $usage->text,
    qr/foo option\n\s+\n\tbar options:\n\s+--bar/,
    "spacer and non-option description found",
  );
}

