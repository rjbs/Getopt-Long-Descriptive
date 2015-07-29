use strict;
use warnings;
package Getopt::Long::Descriptive::Usage;
# ABSTRACT: the usage description for GLD

use List::Util qw(max);

=head1 SYNOPSIS

  use Getopt::Long::Descriptive;
  my ($opt, $usage) = describe_options( ... );

  $usage->text; # complete usage message

  $usage->die;  # die with usage message

=head1 DESCRIPTION

This document only describes the methods of the Usage object.  For information
on how to use L<Getopt::Long::Descriptive>, consult its documentation.

=head1 METHODS

=head2 new

  my $usage = Getopt::Long::Descriptive::Usage->new(\%arg);

You B<really> don't need to call this.  GLD will do it for you.

Valid arguments are:

  options     - an arrayref of options
  leader_text - the text that leads the usage; this may go away!

=cut

sub new {
  my ($class, $arg) = @_;

  my @to_copy = qw(options leader_text show_defaults);

  my %copy;
  @copy{ @to_copy } = @$arg{ @to_copy };

  bless \%copy => $class;
}

=head2 text

This returns the full text of the usage message.

=cut

sub text {
  my ($self) = @_;

  return join qq{\n}, $self->leader_text, $self->option_text;
}

=head2 leader_text

This returns the text that comes at the beginning of the usage message.

=cut

sub leader_text { $_[0]->{leader_text} }

=head2 option_text

This returns the text describing the available options.

=cut

sub option_text {
  my ($self) = @_;

  my @options  = @{ $self->{options} || [] };
  my $string   = q{};
  my @specs = map { $_->{spec} } grep { $_->{desc} ne 'spacer' } @options;
  my $length   = (max(map { _option_length($_) } @specs) || 0);
  my $spec_fmt = "\t%-${length}s";

  while (@options) {
    my $opt  = shift @options;
    my $spec = $opt->{spec};
    my $desc = $opt->{desc};
    my $assign;
    if ($desc eq 'spacer') {
      $string .= sprintf "$spec_fmt\n", $opt->{spec};
      next;
    }

    ($spec, $assign) = Getopt::Long::Descriptive->_strip_assignment($spec);
    $assign = _parse_assignment($assign);
    $spec = join " ", reverse map { length > 1 ? "--${_}$assign" : "-${_}$assign" }
                              split /\|/, $spec;

    my @desc = $self->_split_description($length, $desc);

    # add default value if it exists
    if (exists $opt->{constraint}->{default} and $self->{show_defaults}) {
      my $dflt = $opt->{constraint}->{default};
      $dflt = ! defined $dflt ? '(undef)'
            : ! length  $dflt ? '(empty string)'
            :                   $dflt;
      push @desc, "(default value: $dflt)";
    }

    $string .= sprintf "$spec_fmt  %s\n", $spec, shift @desc;
    for my $line (@desc) {
        $string .= "\t";
        $string .= q{ } x ( $length + 2 );
        $string .= "$line\n";
    }
  }

  return $string;
}

sub _option_length {
    my ($fullspec) = @_;
    my $number_opts = 1;
    my $last_pos = 0;
    my $number_shortopts = 0;
    my ($spec, $argspec) = Getopt::Long::Descriptive->_strip_assignment($fullspec);
    my $length = length $spec;
    my $arglen = length(_parse_assignment($argspec));

    # Spacing rules:
    #
    # For short options we want 1 space (for '-'), for long options 2
    # spaces (for '--').  Then one space for separating the options,
    # but we here abuse that $spec has a '|' char for that.
    #
    # For options that take arguments, we want 2 spaces for mandatory
    # options ('=X') and 4 for optional arguments ('[=X]').  Note we
    # consider {N,M} cases as "single argument" atm.

    # Count the number of "variants" (e.g. "long|s" has two variants)
    while ($spec =~ m{\|}g) {
        $number_opts++;
        if (pos($spec) - $last_pos == 2) {
            $number_shortopts++;
        }
        $last_pos = pos($spec);
    }

    # Was the last option a "short" one?
    if ($length - $last_pos == 1) {
        $number_shortopts++;
    }

    # We got $number_opts options, each with an argument length of
    # $arglen.  Plus each option (after the first) needs 3 a char
    # spacing.  $length gives us the total length of all options and 1
    # char spacing per option (after the first).  So the result should be:

    my $number_longopts = $number_opts - $number_shortopts;
    my $total_arglen = $number_opts * $arglen;
    my $total_optsep = 2 * $number_longopts + $number_shortopts;
    my $total = $length + $total_optsep + $total_arglen;
    return $total;
}

sub _split_description {
  my ($self, $length, $desc) = @_;

  # 8 for a tab, 2 for the space between option & desc;
  my $max_length = 78 - ( $length + 8 + 2 );

  return $desc if length $desc <= $max_length;

  my @lines;
  while (length $desc > $max_length) {
    my $idx = rindex( substr( $desc, 0, $max_length ), q{ }, );
    last unless $idx >= 0;
    push @lines, substr($desc, 0, $idx);
    substr($desc, 0, $idx + 1) = q{};
  }
  push @lines, $desc;

  return @lines;
}

sub _parse_assignment {
    my ($assign_spec) = @_;
    my $argument;
    my $result = 'STR';
    my $desttype;
    if (length($assign_spec) < 2) {
        # empty, ! or +
        return '';
    }

    $argument = substr $assign_spec, 1, 2;
    if ($argument =~ m/^i/ or $argument =~ m/^o/) {
        $result = 'INT';
    } elsif ($argument =~ m/^f/) {
        $result = 'NUM';
    }
    if (length($assign_spec) > 2) {
        $desttype = substr($assign_spec, 2, 1);
        if ($desttype eq '@') {
            # Imply it can be repeated
            $result .= '...';
        } elsif ($desttype eq '%') {
            $result = "KEY=${result}...";
        }
    }
    if (substr($assign_spec, 0, 1) eq ':') {
        return "[=$result]";
    }
    # with leading space so it can just blindly be appended.
    return " $result";
}

=head2 warn

This warns with the usage message.

=cut

sub warn { warn shift->text }

=head2 die

This throws the usage message as an exception.

  $usage_obj->die(\%arg);

Some arguments can be provided 

  pre_text  - text to be prepended to the usage message
  post_text - text to be appended to the usage message

The C<pre_text> and C<post_text> arguments are concatenated with the usage
message with no line breaks, so supply this if you need them.

=cut

sub die  {
  my $self = shift;
  my $arg  = shift || {};

  die(
    join q{}, grep { defined } $arg->{pre_text}, $self->text, $arg->{post_text}
  );
}

use overload (
  q{""} => "text",

  # This is only needed because Usage used to be a blessed coderef that worked
  # this way.  Later we can toss a warning in here. -- rjbs, 2009-08-19
  '&{}' => sub {
    my ($self) = @_;
    Carp::cluck("use of __PACKAGE__ objects as a code ref is deprecated");
    return sub { return $_[0] ? $self->text : $self->warn; };
  }
);

1;
