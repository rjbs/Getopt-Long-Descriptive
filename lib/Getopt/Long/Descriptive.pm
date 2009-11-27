package Getopt::Long::Descriptive;

use strict;
use Getopt::Long;
use List::Util qw(first);
use Carp qw(carp croak);
use Params::Validate qw(:all);
use File::Basename ();

use Getopt::Long::Descriptive::Usage;

=head1 NAME

Getopt::Long::Descriptive - Getopt::Long with usage text

=head1 VERSION

Version 0.080

=cut

our $VERSION = '0.080';

=head1 DESCRIPTION

Convenient wrapper for Getopt::Long and program usage output

=head1 SYNOPSIS

  use Getopt::Long::Descriptive;
  my ($opts, $usage) = describe_options($format, @opts, \%arg);

=head1 FORMAT

  $format = "usage: myprog %o myarg...";

C<%o> will be replaced with a list of the short options, as well as the text
"[long options...]" if any have been defined.

C<%c> will be replaced with what Getopt::Long::Descriptive
thinks is the program name (see L</prog_name>).  You can
override this guess by calling C<< prog_name($string) >>
yourself.

Because of this, any literal C<%> symbols will need to be written as C<%%>.

=head1 OPTIONS

Option specifications are the same as in Getopt::Long.  You should pass in an
array of arrayrefs whose first elements are option specs and whose second
elements are descriptions.

  my @opts = (
    [ "verbose|V" => "be noisy"       ],
    [ "logfile=s" => "file to log to" ],
  );

Option specifications may have a third hashref argument.  If
present, this configures extra restrictions on the value or
presence of that option.

You may cause a blank line to be printed by passing an empty
arrayref.  Likewise, a plain descriptive line will be
printed if you pass an arrayref with a single element:

  @opts = (
    $option,
    [],
    [ 'other options:' ],
    $other_option,
  );

=head2 Option Constraints

=head3 implies

  implies => 'bar'

  implies => [qw(foo bar)]

  implies => { foo => 1, bar => 2 }

=head3 required

  required => 1

=head3 hidden

  hidden => 1

This option will not show up in the usage text.

You can achieve this same behavior by using the string C<<
hidden >> for the option's description.

=head3 one_of

  one_of => \@option_specs

Useful for a group of options that are related.  Each option
spec is added to the list for normal parsing and validation.

Your option name will end up with a value of the name of the
option that was chosen.  For example, given the following spec:

  [ "mode" => hidden => { one_of => [
    [ "get|g"  => "get the value" ],
    [ "set|s"  => "set the value" ],
    [ "delete" => "delete it" ],
  ] } ],

No usage text for 'mode' will be displayed, though
get/set/delete will all have descriptions.

If more than one of get/set/delete (or their short versions)
are given, an error will be thrown.

If C<@ARGV> is C<--get>, a dump of the resultant option
hashref would look like this:

  { get  => 1,
    mode => 'get' }

NOTE: C<< get >> would not be set if C<< mode >> defaulted
to 'get' and no arguments were passed in.

WARNING: Even though the option sub-specs for C<< one_of >>
are meant to be 'first class' specs, some options don't make
sense with them, e.g. C<< required >>.

As a further shorthand, you may specify C<< one_of >>
options using this form:

  [ mode => \@option_specs, \%constraints ]

=head3 Params::Validate

In addition, any constraint understood by Params::Validate may be used.

(Internally, all constraints are translated into Params::Validate options or
callbacks.)

=head1 EXTRA ARGUMENTS

If the last parameter is a hashref, it contains extra arguments to modify the
way C<describe_options> works.  Valid arguments are:

  getopt_conf - an arrayref of strings, passed to Getopt::Long::Configure

=head1 EXPORTED FUNCTIONS

=head2 C<describe_options>

See SYNOPSIS; returns a hashref of option values and an object that represents
the usage statement.  You should always import this routine, and not call it
directly.  The ability to call C<Getopt::Long::Descriptive::describe_options>
may go away in the future.

The usage object has several methods:

=over 4

=item * C<< $usage->text >> returns the usage string

=item * C<< $usage->warn >> prints usage to STDERR

=item * C<< $usage->die >> dies with the usage string

=back

For more information on the usage object, look at
L<Getopt::Long::Descriptive::Usage|Getopt::Long::Descriptive::Usage>.

=head2 prog_name

This routine returns the basename of C<< $0 >>, grabbed at compile-time.

=head2 -types

Any of the Params::Validate type constants (C<SCALAR>, etc.) can be imported as
well.  You can get all of them at once by importing C<-types>.

=head2 C<-all>

This gets you everything.

=head1 CONFIGURATION

=head2 C<$MungeOptions>

When C<$Getopt::Long::Descriptive::MungeOptions> is true, some munging is done
to make option names more hash-key friendly:

=over 4

=item * All keys are lowercased

=item * C<-> is changed to C<_>

=back

The default is a true value.

=head1 SEE ALSO

L<Getopt::Long>
L<Params::Validate>

=cut

my $prog_name;
sub prog_name { @_ ? ($prog_name = shift) : $prog_name }

BEGIN {
  # grab this before someone decides to change it
  prog_name(File::Basename::basename($0));
}

use Sub::Exporter::Util ();
use Sub::Exporter -setup => {
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
    name       => _munge((split /[:=|!+]/, $_->[0] || '')[0]),
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

    my $opt_obj = $class->_new_opt_obj({
      values => { %method_map, %return },
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

my $OPT_CLASS_COUNTER = 1;

sub _class_for_opt {
  my ($gld_class, $arg) = @_;

  my $values = $arg->{values};
  my @bad = grep { $_ !~ /^[a-z_]\w*$/ } keys %$values;
  Carp::confess "perverse option names given: @bad" if @bad;

  my $class = "$gld_class\::__OPT__::" . $OPT_CLASS_COUNTER++;

  {
    no strict 'refs';
    ${"$class\::VERSION"} = $gld_class->VERSION;
    for my $opt (keys %$values) {
      *{"$class\::$opt"} = sub { $_[0]->{ $opt } };
    }
  }

  return $class;
}

sub _new_opt_obj {
  my ($gld_class, $arg) = @_;
  
  my $class = $gld_class->_class_for_opt($arg);
  bless { %{ $arg->{values} } } => $class;
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

=head1 AUTHOR

Hans Dieter Pearcey, C<< <hdp@cpan.org> >>

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
