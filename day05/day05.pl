#! /usr/bin/env perl

use 5.024;
use warnings;
use List::Util qw( min );

chomp( $_ = <> );

say length $_;
my $re = join '|', map { $_.uc($_), uc($_).$_ } 'a'..'z';
while ( s/$re//g ) {}
say length $_;

my $base = $_;
my @lengths;
for ('a'..'z') {
  $_ = $base =~ s/$_|\u$_//gr;
  while ( s/$re//g ) {}
  push @lengths, length $_;
}
say min @lengths;
