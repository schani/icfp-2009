#!/bin/perl

$ta = sprintf("%020.15f", $ARGV[0]);
$tb = sprintf("%020.15f", $ARGV[1]);

print $ta cmp $tb;

