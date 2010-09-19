use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME                => 'Getopt::Long::Descriptive',
  AUTHOR              => 'Hans Dieter Pearcey <hdp@cpan.org>',
  VERSION_FROM        => 'lib/Getopt/Long/Descriptive.pm',
  ABSTRACT_FROM       => 'lib/Getopt/Long/Descriptive.pm',
  PL_FILES            => {},
  PREREQ_PM => {
    'Getopt::Long'     => 2.33, # !-options get --no-X
    'List::Util'       => 0,
    'Params::Validate' => '0.74',
    'Sub::Exporter'    => 0.972, # generator name with \'name'
    'Test::More'       => 0,
  },
  dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
  clean               => { FILES => 'Getopt-Long-Descriptive-*' },
);
