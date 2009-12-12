#!perl
use strict;
use warnings;

use Test::More tests => 37;

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

    for my $key (keys %$expect) {
      is($opt->$key, $expect->{$key}, "...->$key");
    }
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
  "only one 'mode' option",
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

  local $SIG{__WARN__} = sub {}; # we know that this will warn; don't care
  like(
    $usage->(1),
    qr/foo option\n\s+\n\tbar options:\n\s+--bar/,
    "CODEISH: spacer and non-option description found",
  );
}

{
  local @ARGV;
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ 'foo'          => "foo option" ],
    [ 'bar|b'        => "bar option" ],
    [ 'string|s=s'   => "string value" ],
    [ 'ostring|S:s'  => "optional string value" ],
    [ 'list|l=s@'    => "list of strings" ],
    [ 'hash|h=s%'    => "hash values" ],
    [ 'optional|o!'  => "optional" ],
    [ 'increment|i+' => "incremental option" ],
  );
  like(
    $usage->text,
    qr/\[-bhiloSs\]/,
    "short options",
  );
}

{
  local @ARGV = qw(--foo FOO --baz BAZ);
  my ($c_opt, $usage) = describe_options(
    "%c %o",
    [ "foo=s", '' ],
    [ "bar=s", '', { default => 'BAR' } ],
    [ "baz=s", '', { default => 'BAZ' } ],
  );

  my $s_opt = $c_opt->_specified_opts;
  my $C_opt = $s_opt->_complete_opts;

  is($c_opt->foo, 'FOO', 'c_opt->foo is FOO');
  is($C_opt->foo, 'FOO', 'C_opt->foo is FOO');
  is($s_opt->foo, 'FOO', 's_opt->foo is FOO');

  is($c_opt->bar, 'BAR', 'c_opt->foo is BAR');
  is($C_opt->bar, 'BAR', 'C_opt->foo is BAR');
  is($s_opt->bar, undef, 's_opt->foo is undef');

  is($c_opt->baz, 'BAZ', 'c_opt->foo is BAZ');
  is($C_opt->baz, 'BAZ', 'C_opt->foo is BAZ');
  is($s_opt->baz, 'BAZ', 's_opt->foo is BAZ');
}

{
  local @ARGV = qw(--foo);
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ "foo", '' ],
    [ "bar", '' ],
  );
  is( $opt->{foo}, 1, "empty-but-present description is ok" );
  is( $opt->foo,   1, "empty-but-present description is ok" );

  is( $opt->{bar}, undef, "entry not given is undef (exists? no guarantee)" );
  is( $opt->bar,   undef, "entry not given is undef (as method)");
}

{
  local @ARGV = qw(--foo-bar);
  my ($opt) = describe_options(
    "%c %o",
    [ "foo:s", "foo option" ],
    [ "foo-bar", "foo-bar option", { implies => 'foo' } ],
  );
  is_deeply($opt, { foo => 1, foo_bar => 1 },
    "ok to imply option with optional argument");

  is($opt->foo_bar, 1, 'given value (checked with method)');
  is($opt->foo,     1, 'implied value (checked with method)');
}
