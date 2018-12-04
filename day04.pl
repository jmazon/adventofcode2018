#! /usr/bin/env perl

use 5.024;
use warnings;

my ($id,$start,$end,$line);
my %lines;

sub update {
  defined $id and push @{ $lines{$id} }, $line;
  $line = '.' x 60;
}

for ( sort <> ) {
  my ($year,$month,$day,$hour,$minute,$msg) =
    /\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)/
    or die $_;
  if ( $msg =~ /Guard #(\d+) begins shift/ ) {
    update;
    $id = $1;
  }
  elsif ( $msg =~ /falls asleep/ ) {
    $start = $minute;
  }
  elsif ( $msg =~ /wakes up/ ) {
    $end = $minute;
    substr( $line,$_,1 ) = '#' for $start .. $end-1;
  }
  else { die $msg; }
}
update;

my ($result,$max_time) = (undef,-1);
my ($min_max,$min_id,$min_guard_id) = (-1,undef,undef);
for $id ( keys %lines ) {
  my $time = 0;
  my @minutes = (0) x 60;
  for $line ( @{ $lines{$id} } ) {
    for (0..59) {
      if ( substr( $line,$_,1 ) eq '#' ) {
        $time++;
        $minutes[$_]++;
        if ( $minutes[$_] > $min_max ) {
          $min_max = $minutes[$_];
          $min_id = $_;
          $min_guard_id = $id;
        }
      }
    }
  }
  if ($time > $max_time) {
    my ($max_min,$id_min) = (0,undef);
    for (0..59) {
      if ( $minutes[$_] > $max_min ) {
        $max_min = $minutes[$_];
        $id_min = $_;
      }
    }
    $max_time = $time;
    $result = $id * $id_min;
  }
}

say $result;
say $min_guard_id * $min_id;
