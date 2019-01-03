#! /usr/bin/env perl

use 5.024;
use warnings;

my (%g,%i);

while ( <> ) {
  my ($id,$x,$y,$w,$h) = /#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/;
  $i{$id} = 1;
  for my $i ($x..$x+$w-1) {
    for my $j ($y..$y+$h-1) {
      my $k = "$i,$j";
      my $former = $g{$k};
      if ($former) { $g{$k} = 'X'; $i{$former} = $i{$id} = 0 }
      else         { $g{$k} = $id }
    }
  }
}

say scalar grep $_ eq 'X', values %g;
say grep $i{$_}, keys %i;
