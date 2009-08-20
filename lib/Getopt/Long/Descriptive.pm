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

Version 0.076

=cut

our $VERSION = '0.076';

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

In addition, any constraint understood by Params::Validate
may be used.

(Internally, all constraints are translated into
Params::Validate options or callbacks.)

=head1 EXTRA ARGUMENTS

If the last parameter is a hashref, it contains extra arguments to modify the
way C<describe_options> works.  Valid arguments are:

  getopt_conf - an arrayref of strings, passed to Getopt::Long::Configure

=head1 EXPORTED FUNCTIONS

=head2 C<describe_options>

See SYNOPSIS; returns a hashref of option values and an
object that represents the usage statement.

The usage statement has several methods:

=over 4

=item * C<< $usage->text >> returns the usage string

=item * C<< $usage->warn >> prints usage to STDERR

=item * C<< $usage->die >> dies with the usage string

=back

=head2 C<< prog_name >>

A helper function that returns the basename of C<< $0 >>,
grabbed at compile-time.

=head2 C<:types>

Any of the Params::Validate type constants (C<SCALAR>, etc.)
can be imported as well.  You can get all of them at once by
importing C<:types>.

=head2 C<:all>

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

use Sub::Exporter -setup => {
  exports => [
    qw(describe_options prog_name),
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
    name       => _munge((split /[:=|!]/, $_->[0] || '')[0]),
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

sub describe_options {
  my $format = shift;
  my $arg    = (ref $_[-1] and ref $_[-1] eq 'HASH') ? pop @_ : {};
  my @opts;

  # special casing
  # wish we had real loop objects
  for my $opt (_expand(@_)) {
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
  my @specs = map { $_->{spec} } grep {
    $_->{desc} ne 'spacer'
  } _nohidden(@opts);

  my $short = join "", sort {
    lc $a cmp lc $b 
    or $a cmp $b
  } map {
    my $s = __PACKAGE__->_strip_assignment($_);
    grep /^.$/, split /\|/, $s
  } @specs;
  
  my $long = grep /\b[^|]{2,}/, @specs;

  my %replace = (
    "%" => "%",
    "o" => (join(" ",
                 ($short ? "[-$short]" : ()),
                 ($long  ? "[long options...]" : ())
               )),
    "c" => prog_name,
  );

  (my $str = $format) =~ s/%(.)/$replace{$1}/ge;
  $str =~ s/\s{2,}/ /g;

  my $usage = Getopt::Long::Descriptive::Usage->new({
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

  my $opt_obj = Getopt::Long::Descriptive::OptObjFactory->new_opt_obj({
    values => \%return,
  });

  return($opt_obj, $usage);
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

{
  # Clever line break to avoid indexing! -- rjbs, 2009-08-20
  package
    Getopt::Long::Descriptive::OptObjFactory;

  my $VERSION = '0.076';

  use Carp ();

  my $i = 1;

  sub new_opt_obj {
    my ($inv_class, $arg) = @_;
    
    my %given = %{ $arg->{values} };

    my @bad = grep { $_ !~ /^[a-z_]\w+/ } keys %given;
    Carp::confess "perverse option names given: @bad" if @bad;

    my $class = "$inv_class\::_::" . $i++;

    {
      no strict 'refs';
      ${"$class\::VERSION"} = $inv_class->VERSION;
      for my $opt (keys %given) {
        *{"$class\::$opt"} = sub { $_[0]->{ $opt } };
      }
    }

    bless \%given => $class;
  }
}

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
