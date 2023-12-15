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

  my @to_copy = qw(leader_text options show_defaults);

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

  my $string   = q{};

  my @options  = @{ $self->{options} || [] };
  my @specs    = map { $_->{spec} } grep { $_->{desc} ne 'spacer' } @options;
  my $length   = (max(map { _option_length($_) } @specs) || 0);
  my $spec_fmt = "    %-${length}s";

  while (@options) {
    my $opt  = shift @options;
    my $spec = $opt->{spec};
    my $desc = $opt->{desc};

    if ($desc eq 'spacer') {
      if (ref $opt->{spec}) {
        $string .= "${ $opt->{spec} }\n";
        next;
      } else {
        my @lines = $self->_split_description(0, $opt->{spec});

        $string .= length($_) ? sprintf("$spec_fmt\n", $_) : "\n" for @lines;
        next;
      }
    }

    ($spec, my $assign) = Getopt::Long::Descriptive->_strip_assignment($spec);

    my ($pre, $post) = _parse_assignment($assign);
    my @names = split /\|/, $spec;

    my $primary = shift @names;
    my $short;
    my ($i) = grep {; length $names[$_] == 1 } (0 .. $#names);
    if (defined $i) {
      $short = splice @names, $i, 1;
    }

    $spec = length $primary > 1 ? "--$pre$primary$post" : "-$primary$post";
    $spec .= " (or -$short)" if $short;

    my @desc = $self->_split_description($length, $desc);

    if (@names) {
      push @desc,
        "aka " . join q{, }, map { length > 1 ? "--$_" : "-$_" } @names;
    }

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
        $string .= "    ";
        $string .= q{ } x ( $length + 2 );
        $string .= "$line\n";
    }
  }

  return $string;
}

sub _option_length {
    my ($fullspec) = @_;

    my ($spec, $argspec) = Getopt::Long::Descriptive->_strip_assignment($fullspec);

    my ($pre, $post) = _parse_assignment($argspec);
    my @names = split /\|/, $spec;

    my $primary = shift @names;
    my $short   = (@names && length $names[0] eq 1)
                ? shift @names
                : undef;

    $spec = length $primary > 1 ? "--$pre$primary$post" : "-$primary$post";
    $spec .= " (or -$short)" if $short;

    return length $spec;
}

sub _max_line_length {
  return $Getopt::Long::Descriptive::TERM_WIDTH - 2;
}

sub _split_description {
  my ($self, $length, $desc) = @_;

  # 8 for a tab, 2 for the space between option & desc, 2 more for gutter
  my $max_length = $self->_max_line_length - ( $length + 8 + 2 );

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

    my $result = 'STR';
    my $desttype;
    if (length($assign_spec) < 2) {
        # empty, ! or +
        return ('[no-]', '') if $assign_spec eq '!';
        return ('', '');
    }

    my $optional = substr($assign_spec, 0, 1) eq ':';
    my $argument = substr $assign_spec, 1, 2;

    if ($argument =~ m/^[io]/ or $assign_spec =~ m/^:[+0-9]/) {
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

    if ($optional) {
        return ("", "[=$result]");
    }

    # with leading space so it can just blindly be appended.
    return ("", " $result");
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
