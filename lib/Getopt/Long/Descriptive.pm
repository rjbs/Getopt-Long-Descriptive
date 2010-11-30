use strict;
use warnings;
package Getopt::Long::Descriptive;

use Carp qw(carp croak);
use File::Basename ();
use Getopt::Long 2.33;
use List::Util qw(first);
use Params::Validate qw(:all);
use Scalar::Util ();

use Getopt::Long::Descriptive::Opts;
use Getopt::Long::Descriptive::Usage;

=head1 NAME

Getopt::Long::Descriptive - Getopt::Long, but simpler and more powerful

=head1 VERSION

Version 0.087

=cut

our $VERSION = '0.087';

=head1 SYNOPSIS

  use Getopt::Long::Descriptive;

  my ($opt, $usage) = describe_options(
    'my-program %o <some-arg>',
    [ 'server|s=s', "the server to connect to"                  ],
    [ 'port|p=i',   "the port to connect to", { default => 79 } ],
    [],
    [ 'verbose|v',  "print extra stuff"            ],
    [ 'help',       "print usage message and exit" ],
  );

  print($usage->text), exit if $opt->help;

  Client->connect( $opt->server, $opt->port );

  print "Connected!\n" if $opt->verbose;

...and running C<my-program --help> will produce:

  my-program [-psv] [long options...] <some-arg>
    -s --server     the server to connect to
    -p --port       the port to connect to
                  
    -v --verbose    print extra stuff
    --help          print usage message and exit

=head1 DESCRIPTION

Getopt::Long::Descriptive is yet another Getopt library.  It's built atop
Getopt::Long, and gets a lot of its features, but tries to avoid making you
think about its huge array of options.

It also provides usage (help) messages, data validation, and a few other useful
features.

=head1 FUNCTIONS

Getopt::Long::Descriptive only exports one routine by default:
C<describe_options>.  All GLD's exports are exported by L<Sub::Exporter>.

=head2 describe_options

  my ($opt, $usage) = describe_options($usage_desc, @opt_spec, \%arg);

This routine inspects C<@ARGV> returns the options given and a object
for generating usage messages.

The C<$opt> object will be a dynamically-generated subclass of
L<Getopt::Long::Descriptive::Opts>.  In brief, each of the options in
C<@opt_spec> becomes an accessor method on the object, using the first-given
name, with dashes converted to underscores.  For more information, see the
documentation for the Opts class.

The C<$usage> object will be a L<Getopt::Long::Descriptive::Usage> object,
which provides a C<text> method to get the text of the usage message and C<die>
to die with it.  For more methods and options, consults the documentation for
the Usage class.

=head3 $usage_desc

The C<$usage_desc> parameter to C<describe_options> is a C<sprintf>-like string
that is used in generating the first line of the usage message.  It's a
one-line summary of how the command is to be invoked.  A typical usage
description might be:

  $usage_desc = "%c %o <source> <desc>";

C<%c> will be replaced with what Getopt::Long::Descriptive thinks is the
program name (it's computed from C<$0>, see L</prog_name>).

C<%o> will be replaced with a list of the short options, as well as the text
"[long options...]" if any have been defined.

The rest of the usage description can be used to summarize what arguments are
expected to follow the program's options, and is entirely free-form.

Literal C<%> characters will need to be written as C<%%>, just like with
C<sprintf>.

=head3 @opt_spec

The C<@opt_spec> part of the args to C<describe_options> is used to configure
option parsing and to produce the usage message.  Each entry in the list is an
arrayref describing one option, like this:

  @opt_spec = (
    [ "verbose|V" => "be noisy"       ],
    [ "logfile=s" => "file to log to" ],
  );

The first value in the arrayref is a Getopt::Long-style option specification.
In brief, they work like this:  each one is a pipe-delimited list of names,
optionally followed by a type declaration.  Type declarations are '=x' or ':x',
where C<=> means a value is required and C<:> means it is optional.  I<x> may
be 's' to indicate a string is required, 'i' for an integer, or 'f' for a
number with a fractional part.  The type spec may end in C<@> to indicate that
the option may appear multiple times.

For more information on how these work, see the L<Getopt::Long> documentation.

The first name given should be the canonical name, as it will be used as the
accessor method on the C<$opt> object.  Dashes in the name will be converted to
underscores, and all letters will be lowercased.  For this reason, all options
should generally have a long-form name.

The second value in the arrayref is a description of the option, for use in the
usage message.

=head4 Special Option Specifications

If the option specification (arrayref) is empty, it will have no effect other
than causing a blank line to appear in the usage message.

If the option specification contains only one element, it will be printed in
the usage message with no other effect.

If the option specification contains a third element, it adds extra constraints
or modifiers to the interpretation and validation of the value.  These are the
keys that may be present in that hashref, and how they behave:

=over 4

=item implies

  implies => 'bar'
  implies => [qw(foo bar)]
  implies => { foo => 1, bar => 2 }

If option I<A> has an "implies" entry, then if I<A> is given, other options
will be enabled.  The value may be a single option to set, an arrayref of
options to set, or a hashref of options to set to specific values.

=item required

  required => 1

If an option is required, failure to provide the option will result in
C<describe_options> printing the usage message and exiting.

=item hidden

  hidden => 1

This option will not show up in the usage text.

You can achieve the same behavior by using the string "hidden" for the option's
description.

=item one_of

  one_of => \@subopt_specs

This is useful for a group of options that are related.  Each option
spec is added to the list for normal parsing and validation.

Your option name will end up with a value of the name of the
option that was chosen.  For example, given the following spec:

  [ "mode" => hidden => { one_of => [
    [ "get|g"  => "get the value" ],
    [ "set|s"  => "set the value" ],
    [ "delete" => "delete it" ],
  ] } ],

No usage text for 'mode' will be displayed, but text for get, set, and delete
will be displayed.

If more than one of get, set, or delete is given, an error will be thrown.

So, given the C<@opt_spec> above, and an C<@ARGV> of C<('--get')>, the
following would be true:

  $opt->get == 1;

  $opt->mode eq 'get';

B<Note>: C<get> would not be set if C<mode> defaulted to 'get' and no arguments
were passed in.

Even though the option sub-specs for C<one_of> are meant to be 'first
class' specs, some options don't make sense with them, e.g. C<required>.

As a further shorthand, you may specify C<one_of> options using this form:

  [ mode => \@option_specs, \%constraints ]

=item Params::Validate

In addition, any constraint understood by Params::Validate may be used.

(Internally, all constraints are translated into Params::Validate options or
callbacks.)

=back

=head3 %arg

The C<%arg> to C<describe_options> is optional.  If the last parameter is a
hashref, it contains extra arguments to modify the way C<describe_options>
works.  Valid arguments are:

  getopt_conf - an arrayref of strings, passed to Getopt::Long::Configure

=head2 prog_name

This routine, exported on demand, returns the basename of C<$0>, grabbed at
compile-time.  You can override this guess by calling C<prog_name($string)>
yourself.

=head1 OTHER EXPORTS

=head2 C<-types>

Any of the Params::Validate type constants (C<SCALAR>, etc.) can be imported as
well.  You can get all of them at once by importing C<-types>.

=head2 C<-all>

This import group will import C<-type>, C<describe_options>, and C<prog_name>.

=cut

my $prog_name;
sub prog_name { @_ ? ($prog_name = shift) : $prog_name }

BEGIN {
  # grab this before someone decides to change it
  prog_name(File::Basename::basename($0));
}

use Sub::Exporter::Util ();
use Sub::Exporter 0.972 -setup => {
  exports => [
    describe_options => \'_build_describe_options',
    q(prog_name),
    @{ $Params::Validate::EXPORT_TAGS{types} }
  ],
  groups  => [
    default => [ qw(describe_options) ],
    types   => $Params::Validate::EXPORT_TAGS{types},
  ],
};

my %CONSTRAINT = (
  implies  => \&_mk_implies,
  required => { optional => 0 },
  only_one => \&_mk_only_one,
);

our $MungeOptions = 1;

sub _nohidden {
  return grep { ! $_->{constraint}->{hidden} } @_;
}

sub _expand {
  return map { {(
    spec       => $_->[0] || '',
    desc       => @$_ > 1 ? $_->[1] : 'spacer',
    constraint => $_->[2] || {},

    # if @$_ is 0 then we got [], a spacer
    name       => @$_ ? _munge((split /[:=|!+]/, $_->[0] || '')[0]) : '',
  )} } @_;
}
    
my %HIDDEN = (
  hidden => 1,
);

my $SPEC_RE = qr{(?:[:=][\d\w\+]+[%@]?({\d*,\d*})?|[!+])$};
sub _strip_assignment {
  my ($self, $str) = @_;

  (my $copy = $str) =~ s{$SPEC_RE}{};

  return $copy;
}

# This is here only to deal with people who were calling this fully-qualified
# without importing.  Sucks to them!  -- rjbs, 2009-08-21
sub describe_options {
  my $sub = __PACKAGE__->_build_describe_options(describe_options => {} => {});
  $sub->(@_);
}

sub usage_class { 'Getopt::Long::Descriptive::Usage' }

sub _build_describe_options {
  my ($class) = @_;

  sub {
    my $format = shift;
    my $arg    = (ref $_[-1] and ref $_[-1] eq 'HASH') ? pop @_ : {};
    my @opts;

    # special casing
    # wish we had real loop objects
    my %method_map;
    for my $opt (_expand(@_)) {
      $method_map{ $opt->{name} } = undef unless $opt->{desc} eq 'spacer';
 
      if (ref($opt->{desc}) eq 'ARRAY') {
        $opt->{constraint}->{one_of} = delete $opt->{desc};
        $opt->{desc} = 'hidden';
      }
      if ($HIDDEN{$opt->{desc}}) {
        $opt->{constraint}->{hidden}++;
      }
      if ($opt->{constraint}->{one_of}) {
        for my $one_opt (_expand(
          @{delete $opt->{constraint}->{one_of}}
        )) {
          $one_opt->{constraint}->{implies}
            ->{$opt->{name}} = $one_opt->{name};
          for my $wipe (qw(required default)) {
            if ($one_opt->{constraint}->{$wipe}) {
              carp "'$wipe' constraint does not make sense in sub-option";
              delete $one_opt->{constraint}->{$wipe};
            }
          }
          $one_opt->{constraint}->{one_of} = $opt->{name};
          push @opts, $one_opt;
        }
      }
      push @opts, $opt;
    }
    
    my @go_conf = @{ $arg->{getopt_conf} || $arg->{getopt} || [] };
    if ($arg->{getopt}) {
      warn "describe_options: 'getopt' is deprecated, please use 'getopt_conf' instead\n";
    }

    push @go_conf, "bundling" unless grep { /bundling/i } @go_conf;
    push @go_conf, "no_auto_help"  unless grep { /no_auto_help/i } @go_conf;

    # not entirely sure that all of this (until the Usage->new) shouldn't be
    # moved into Usage -- rjbs, 2009-08-19
    my @specs =
      map  { $_->{spec} }
      grep { $_->{desc} ne 'spacer' }
      _nohidden(@opts);

    my $short = join q{},
      sort  { lc $a cmp lc $b or $a cmp $b }
      grep  { /^.$/ }
      map   { split /\|/ }
      map   { __PACKAGE__->_strip_assignment($_) }
      @specs;
    
    my $long = grep /\b[^|]{2,}/, @specs;

    my %replace = (
      "%" => "%",
      "c" => prog_name,
      "o" => join(q{ },
        ($short ? "[-$short]" : ()),
        ($long  ? "[long options...]" : ())
      ),
    );

    (my $str = $format) =~ s/%(.)/$replace{$1}/ge;
    $str =~ s/\s{2,}/ /g;

    my $usage = $class->usage_class->new({
      options     => [ _nohidden(@opts) ],
      leader_text => $str,
    });

    Getopt::Long::Configure(@go_conf);

    my %return;
    $usage->die unless GetOptions(\%return, grep { length } @specs);
    my @given_keys = keys %return;

    for my $opt (keys %return) {
      my $newopt = _munge($opt);
      next if $newopt eq $opt;
      $return{$newopt} = delete $return{$opt};
    }

    for my $copt (grep { $_->{constraint} } @opts) {
      delete $copt->{constraint}->{hidden};
      my $name = $copt->{name};
      my $new  = _validate_with(
        name   => $name,
        params => \%return,
        spec   => $copt->{constraint},
        opts   => \@opts,
        usage  => $usage,
      );
      next unless (defined($new) || exists($return{$name}));
      $return{$name} = $new;
    }

    my $opt_obj = Getopt::Long::Descriptive::Opts->___new_opt_obj({
      values => { %method_map, %return },
      given  => { map {; $_ => 1 } @given_keys },
    });

    return($opt_obj, $usage);
  }
}

sub _munge {
  my ($opt) = @_;
  return $opt unless $MungeOptions;
  $opt = lc($opt);
  $opt =~ tr/-/_/;
  return $opt;
}

sub _validate_with {
  my (%arg) = validate(@_, {
    name   => 1,
    params => 1,
    spec   => 1,
    opts   => 1,
    usage  => 1,
  });
  my $spec = $arg{spec};
  my %pvspec;
  for my $ct (keys %{$spec}) {
    if ($CONSTRAINT{$ct} and ref $CONSTRAINT{$ct} eq 'CODE') {
      $pvspec{callbacks} ||= {};
      $pvspec{callbacks} = {
        %{$pvspec{callbacks}},
        $CONSTRAINT{$ct}->(
          $arg{name},
          $spec->{$ct},
          $arg{params},
          $arg{opts},
        ),
      };
    } else {
      %pvspec = (
        %pvspec,
        $CONSTRAINT{$ct} ? %{$CONSTRAINT{$ct}} : ($ct => $spec->{$ct}),
      );
    }
  }

  $pvspec{optional} = 1 unless exists $pvspec{optional};

  # we need to implement 'default' by ourselves sometimes
  # because otherwise the implies won't be checked/executed
  # XXX this should be more generic -- we'll probably want
  # other callbacks to always run, too
  if (!defined($arg{params}{$arg{name}})
        && $pvspec{default}
          && $spec->{implies}) {

    $arg{params}{$arg{name}} = delete $pvspec{default};
  }

  my %p = eval { 
    validate_with(
      params => [ %{$arg{params}} ],
      spec   => { $arg{name} => \%pvspec },
      allow_extra => 1,
    );
  };

  if ($@) {
    if ($@ =~ /^Mandatory parameter '([^']+)' missing/) {
      my $missing = $1;
      $arg{usage}->die({
        pre_text => "Required option missing: $1\n",
      });
    }

    die $@;
  }
      
  return $p{$arg{name}};
}

# scalar:   single option = true
# arrayref: multiple options = true
# hashref:  single/multiple options = given values
sub _norm_imply {
  my ($what) = @_;

  return { $what => 1 } unless my $ref = ref $what;

  return $what                      if $ref eq 'HASH';
  return { map { $_ => 1 } @$what } if $ref eq 'ARRAY';

  die "can't imply: $what";
}

sub _mk_implies {
  my $name = shift;
  my $what = _norm_imply(shift);
  my $param = shift;
  my $opts  = shift;

  for my $implied (keys %$what) {
    die("option specification for $name implies nonexistent option $implied\n")
      unless first { $_->{name} eq $implied } @$opts
  }

  my $whatstr = join(q{, }, map { "$_=$what->{$_}" } keys %$what);

  return "$name implies $whatstr" => sub {
    my ($pv_val) = shift;

    # negatable options will be 0 here, which is ok.
    return 1 unless defined $pv_val;

    while (my ($key, $val) = each %$what) {
      if (exists $param->{$key} and $param->{$key} ne $val) {
        die(
          "option specification for $name implies that $key should be "
          . "set to '$val', but it is '$param->{$key}' already\n"
        );
      }
      $param->{$key} = $val;
    }

    return 1;
  };
}

sub _mk_only_one {
  die "unimplemented";
}

=head1 CUSTOMIZING

Getopt::Long::Descriptive uses L<Sub::Exporter|Sub::Exporter> to build and
export the C<describe_options> routine.  By writing a new class that extends
Getopt::Long::Descriptive, the behavior of the constructed C<describe_options>
routine can be changed.

The following methods can be overridden:

=head2 usage_class

  my $class = Getopt::Long::Descriptive->usage_class;

This returns the class to be used for constructing a Usage object, and defaults
to Getopt::Long::Descriptive::Usage.

=head1 SEE ALSO

L<Getopt::Long>
L<Params::Validate>

=head1 AUTHORS

Hans Dieter Pearcey, C<< <hdp@cpan.org> >>

Ricardo Signes, C<< <rjbs@cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-getopt-long-descriptive@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Getopt-Long-Descriptive>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 COPYRIGHT & LICENSE

Copyright 2005 Hans Dieter Pearcey, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Getopt::Long::Descriptive
