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
        'Test::More' => 0,
        'Params::Validate' => '0.74',
        'List::Util' => 0,
        'IO::Scalar' => 0,
        'Sub::Exporter' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Getopt-Long-Descriptive-*' },
);
