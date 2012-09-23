#!/usr/bin/env perl

## Solve Every Sudoku Puzzle
## See http://norvig.com/sudoku.html

## Throughout this program we have:
##   $r is a row,    e.g. 'A'
##   $c is a column, e.g. '3'
##   $s is a square, e.g. 'A3'
##   $d is a digit,  e.g. '9'
##   $u is a unit,   e.g. ['A1','B1','C1','D1','E1','F1','G1','H1','I1']
##   $grid is a grid,e.g. 81 non-blank chars, e.g. starting with '.18...7...
##   $values is a dict of possible values, e.g. {'A1':'12349', 'A2':'8', ...}

use strict;
use warnings;
use v5.16;
use File::Slurp;
use Time::HiRes qw( time );
use Games::Sudoku::Lite;
use List::AllUtils qw(all first zip each_array each_arrayref reduce max shuffle sum);

sub solve {
    my $board = shift;
    my $puzzle = Games::Sudoku::Lite->new($board);
    $puzzle->solve;
}

sub from_file {
    my $filename = shift;
    my @lines = read_file( $filename, chomp => 1 );
    my @boards;
    for (@lines) {
        my @chunks = ( $_ =~ m/........./g );
        my $board = join("\n", @chunks);
        push @boards, $board ;
    }
    return \@boards;
}

################ System test ################

sub time_solve {
    my $grid   = shift;
    my $start  = time();
    my $values = solve($grid);
    my $t      = time() - $start;
    return ( $t, 1 );
}

sub solve_all {
    my ( $grids, $name ) = @_;
    my $N = scalar(@$grids);
    my ( @times, @results );
    for (@$grids) {
        my ( $t, $result ) = time_solve($_);
        push @times,   $t;
        push @results, $result;
    }
    if ( $N > 1 ) {
        printf "Solved %d of %d %s puzzles (avg %.2f secs (%d Hz), max %.2f secs).\n",
          sum(@results), $N, $name, sum(@times) / $N, $N / sum(@times),
          max(@times);
    }
}

my $grid1 = '..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..';
my $grid2 = '4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......';
my $hard1 = '.....6....59.....82....8....45........3........6..3.54...325..6..................';

solve_all( from_file("easy50.txt"),  "easy" );
solve_all( from_file("top95.txt"),   "hard" );
solve_all( from_file("hardest.txt"), "hardest" );

## References used:
## http://www.scanraid.com/BasicStrategies.htm
## http://www.sudokudragon.com/sudokustrategy.htm
## http://www.krazydad.com/blog/2005/09/29/an-index-of-sudoku-strategies/
## http://www2.warwick.ac.uk/fac/sci/moac/currentstudents/peter_cock/python/sudoku/
