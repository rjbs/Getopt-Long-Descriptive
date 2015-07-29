#!perl
use strict;
use warnings;

use Test::More tests => 58;

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

is_opt(
  [ '--nora' ],
  [ [ "nora", "Invisible Nora", { hidden => 1 } ] ],
  { nora => 1 },
  "",
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
  qr/mandatory parameter/i,
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
  local @ARGV;
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ 'string|s=s'   => "string value" ],
    [ 'ostring|S:s'  => "optional string value" ],
    [ 'list|l=s@'    => "list of strings" ],
    [ 'hash|h=s%'    => "hash values" ],
    [ 'optional|o!'  => "optional" ],
    [ 'increment|i+' => "incremental option" ],
  );
  my $usage_text = $usage->text;
  like(
    $usage_text,
    qr/-s STR --string STR\s+string value/,
    "Spec =s gets an STR in usage output",
  );
  like(
    $usage_text,
    qr/-S\[=STR\] --ostring\[=STR\]\s+optional string value/,
    "Spec :s gets an STR in usage output",
  );
  like(
    $usage_text,
    qr/-l STR\Q...\E --list STR\Q...\E\s+list of strings/,
    "Spec =s@ gets an STR... in usage output",
  );
  like(
    $usage_text,
    qr/-h KEY=STR\Q...\E --hash KEY=STR\Q...\E\s+hash values/,
    "Spec =s% gets an KEY=STR... in usage output",
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

{
  local @ARGV;
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ foo => "a foo option" ],
    [ bar => "a bar option" ],
    [ baz => "a baz option with a very long description."
             . " It just goes on for a really long time."
             . " This allows us to test line wrapping and"
             . " make sure the output always looks spiffy" ],
  );

  my $expect = qr/
	--baz  a baz option with a very long description. It just goes on for
	       a really long time. This allows us to test line wrapping and
	       make sure the output always looks spiffy/;


  like($usage->text, $expect, 'long option description is wrapped cleanly');
}

{
  local @ARGV;
  my ($opt, $usage) = describe_options(
    "%c %o",
    [ foo => "x" x 80 ],
  );
  local $@;
  local $SIG{ALRM} = sub { die "ALRM\n" };
  eval {
    alarm(2);
    like($usage->text, qr/@{["x" x 80]}/, "handled unwrappable description");
    alarm(0);
  };
  is($@, '', "no error in eval");
}

{
  my $p = \&Getopt::Long::Descriptive::Usage::_parse_assignment;

  is ($p->('=s'), ' STR', 'string');
  is ($p->('=i'), ' INT', 'int (i)');
  is ($p->('=o'), ' INT', 'int (o)');
  is ($p->('=f'), ' NUM', 'float');

  is ($p->('=s@'), ' STR...', 'strings');
  is ($p->('=i@'), ' INT...', 'ints (i)');
  is ($p->('=o@'), ' INT...', 'ints (o)');
  is ($p->('=f@'), ' NUM...', 'floats');

  is ($p->('=s%'), ' KEY=STR...', 'string maps');
  is ($p->('=i%'), ' KEY=INT...', 'int maps (i)');
  is ($p->('=o%'), ' KEY=INT...', 'int maps (o)');
  is ($p->('=f%'), ' KEY=NUM...', 'float maps');
}
