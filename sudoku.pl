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

use 5.010;
use strict;
use warnings;
use List::AllUtils
  qw(all first zip each_array each_arrayref reduce max shuffle sum);
use Storable qw(dclone);
use File::Slurp;
use Time::HiRes qw( time );

my $digit_re  = qr/\d/;
my $sudoku_re = qr/[\d.]/;

sub cross {
    my ( $a, $b ) = @_;
    my @cross = ();
    for my $x (@$a) {
        for my $y (@$b) {
            push @cross, "$x$y";
        }
    }
    return \@cross;
}

my $digits  = '123456789';
my $rows    = [qw(A B C D E F G H I)];
my $cols    = [qw(1 2 3 4 5 6 7 8 9)];
my $squares = cross( $rows, $cols );

my @unitlist = ();
push @unitlist, cross( $rows, [$_] ) for @$cols;
push @unitlist, cross( [$_], $cols ) for @$rows;
for my $r ( [qw(A B C)], [qw(D E F)], [qw(G H I)] ) {
    for my $c ( [qw(1 2 3)], [qw(4 5 6)], [qw(7 8 9)] ) {
        push @unitlist, cross( $r, $c );
    }
}

my %units;
for my $s (@$squares) {
    $units{$s} = [];
    for my $unit (@unitlist) {
        for my $s2 (@$unit) {
            if ( $s eq $s2 ) {
                push @{ $units{$s} }, $unit;
                last;
            }
        }
    }
}

my %peers;
for my $s (@$squares) {
    $peers{$s} = {};
    for my $u ( @{ $units{$s} } ) {
        for my $s2 ( @{$u} ) {
            $peers{$s}{$s2} = 1 if ( $s2 ne $s );
        }
    }

    $peers{$s} = [ keys %{ $peers{$s} } ]
}

################ Unit Tests ################

sub test {
    die "There are not 81 squares" unless ( @$squares == 81 );
    die "There are not 27 units"   unless ( @unitlist == 27 );
    die "There is a faulty unit"
      unless ( all { scalar( @{ $units{$_} } ) == 3 } @$squares );
    die "There is a faulty peer list"
      unless ( all { @{ $peers{$_} } == 20 } @$squares );
    my %c2_peers = map { $_ => 1 }
      qw(A2 B2 D2 E2 F2 G2 H2 I2 C1 C3 C4 C5 C6 C7 C8 C9 A1 A3 B1 B3);

    for ( @{ $peers{'C2'} } ) {
        die "Faulty peer list" unless ( exists( $c2_peers{$_} ) );
    }
    my @c2_units = (
        [qw(A2 B2 C2 D2 E2 F2 G2 H2 I2 )],
        [qw( C1 C2 C3 C4 C5 C6 C7 C8 C9)],
        [qw( A1 A2 A3 B1 B2 B3 C1 C2 C3 )],
    );
    my $ea = each_array( @c2_units, @{ $units{'C2'} } );
    while ( my ( $a, $b ) = $ea->() ) {
        my $ear = each_arrayref( $a, $b );
        while ( my ( $x, $y ) = $ear->() ) {
            die "Faulty unit list" unless ( $x eq $y );
        }
    }
    say 'All tests pass.';
}

################ Parse a Grid ################

sub parse_grid {
    my $grid   = shift;
    my $values = {};
    $values->{$_} = $digits for ( @{$squares} );
    my @chars = grep { $_ =~ m/$sudoku_re/ } split( //, $grid );
    my %g = zip @{$squares}, @chars;
    while ( my ( $s, $d ) = each %g ) {
        if ( ( $d =~ m/$digit_re/ ) && ( assign( $values, $s, $d ) == 0 ) ) {
            return 0;
        }
    }
    return $values;
}

################ Constraint Propagation ################

sub assign {
    my ( $values, $s, $d ) = @_;
    return $values
      if (
        all { eliminate( $values, $s, $_ ) }
        grep { $_ ne $d } split( //, $values->{$s} )
      );
    return 0;
}

sub eliminate {
    my ( $values, $s, $d ) = @_;
    my $i = index( $values->{$s}, $d );
    return $values unless ( $i >= 0 );

    substr( $values->{$s}, $i, 1, '' );
    my $len = length( $values->{$s} );
    if ( $len == 0 ) {
        return 0;
    }
    elsif ( $len == 1 ) {
        return 0
          unless ( all { eliminate( $values, $_, $values->{$s} ) }
            @{ $peers{$s} } );
    }
    for my $u ( @{ $units{$s} } ) {
        my @dplaces = grep { index( $values->{$_}, $d ) >= 0 } @$u;
        $len = @dplaces;
        if ( $len == 0 ) {
            return 0;
        }
        elsif ( $len == 1 ) {
            if ( assign( $values, $dplaces[0], $d ) == 0 ) {
                return 0;
            }
        }
    }
    return $values;
}

sub solve {
    my $grid = shift;
    return search( parse_grid($grid) );
}

sub search {
    my $values = shift;
    return 0 if ( $values == 0 );
    return $values
      if ( all { length( $values->{$_} ) == 1 } @{$squares} );    # solved!

    my $s =
      reduce { length( $values->{$a} ) < length( $values->{$b} ) ? $a : $b }
    grep { length( $values->{$_} ) > 1 } @{$squares};

    my $some = 0;
    for ( split( //, $values->{$s} ) ) {
        $some = search( assign( dclone($values), $s, $_ ) );
        last unless ( $some == 0 );
    }
    return $some;
}

sub from_file {
    my $filename = shift;
    my @lines = read_file( $filename, chomp => 1 );
    return \@lines;
}

################ System test ################

sub time_solve {
    my $grid   = shift;
    my $start  = time();
    my $values = solve($grid);
    my $t      = time() - $start;
    return ( $t, solved($values) );
}

sub print_grid {
    my $values = shift;
    my @puzzle;
    for ( @{$squares} ) {
        if ( length( $values->{$_} ) == 1 ) {
            push @puzzle, $values->{$_};
        }
        else {
            push @puzzle, ".";
        }
    }
    return join( "", @puzzle );
}

sub solve_all {
    my ( $grids, $name ) = @_;
    my $N = @$grids;
    my ( @times, @results );
    for (@$grids) {
        my ( $t, $result ) = time_solve($_);
        push @times,   $t;
        push @results, $result;
    }
    if ( $N > 1 ) {
        printf
"Solved %d of %d %s puzzles (avg %.4f secs (%.2f Hz), max %.4f secs).\n",
          sum(@results), $N, $name, sum(@times) / $N, $N / sum(@times),
          max(@times);
    }
}

sub unit_solved {
    my ( $values, $unit ) = @_;
    my %set = map { $values->{$_} => 1 } @$unit;
    return all { exists( $set{$_} ) } @$cols;
}

sub solved {
    my $values = shift;
    return 0 if ( $values == 0 );
    return 1 if all { unit_solved( $values, $_ ) } @unitlist;
}

sub random_puzzle {
    my $N      = shift;
    my $values = {};
    $values->{$_} = $digits for ( @{$squares} );
    for ( shuffle @{$squares} ) {
        my $d =
          substr( $values->{$_}, int( rand( length( $values->{$_} ) ) ), 1 );
        last if ( assign( $values, $_, $d ) == 0 );
        my @ds;
        for ( grep { length( $values->{$_} ) == 1 } @{$squares} ) {
            push @ds, $values->{$_};
        }
        my %set = map { $_ => 1 } @ds;
        if ( ( @ds >= $N ) && ( scalar( keys %set ) >= 8 ) ) {
            return print_grid($values);
        }
    }
    return random_puzzle(16);

}

my $grid1 =
'..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..';
my $grid2 =
'4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......';
my $hard1 =
'.....6....59.....82....8....45........3........6..3.54...325..6..................';

test();
solve_all( from_file("easy50.txt"),  "easy" );
solve_all( from_file("top95.txt"),   "hard" );
solve_all( from_file("hardest.txt"), "hardest" );
my @random_puzzles;
push @random_puzzles, random_puzzle(17) for ( 0 .. 99 );
solve_all( \@random_puzzles, "random" );

## References used:
## http://www.scanraid.com/BasicStrategies.htm
## http://www.sudokudragon.com/sudokustrategy.htm
## http://www.krazydad.com/blog/2005/09/29/an-index-of-sudoku-strategies/
## http://www2.warwick.ac.uk/fac/sci/moac/currentstudents/peter_cock/python/sudoku/
