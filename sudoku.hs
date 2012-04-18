-- Solve Every Sudoku Puzzle

-- See http://norvig.com/sudoku.html

-- Throughout this program we have:
-- r is a row, e.g. 'A'
-- c is a column, e.g. '3'
-- s is a square, e.g. 'A3'
-- d is a digit, e.g. '9'
-- u is a unit, e.g. ['A1','B1','C1','D1','E1','F1','G1','H1','I1']
-- grid is a grid,e.g. 81 non-blank chars, e.g. starting with '.18...7...
-- values is a dict of possible values, e.g. {'A1':'12349', 'A2':'8', ...

module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception.Base
import Control.Monad
import System.IO
import System.CPUTime
import Text.Printf
import Control.DeepSeq
import System.Random
import Data.Array.IO
import Data.Maybe

lookup' e m =  fromJust . Map.lookup e $ m

-- Cross product of elements in aa and elements in bb.
cross aa bb = [[a, b] | a <- aa, b <- bb]

digits = "123456789"
rows = "ABCDEFGHI"
cols = digits
squares = cross rows cols
unitlist = [cross rows [c] | c <- cols] ++ 
					[cross [r] cols | r <- rows] ++ 
					[cross rs cs | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789"]]
units = Map.fromList [(s, [u | u <- unitlist, elem s u]) | s <- squares]
peers = Map.fromList [(s, Set.difference (Set.fromList . concat . lookup' s $ units) (Set.fromList [s])) | s <- squares]

----------------------------- Unit Tests ----------------------------

assert' p = assert p ()
-- A set of tests that must pass.
test = map assert' [ (length squares == 81), 
		 								(length unitlist == 27),
										and [(length . lookup' s $ units) == 3 | s <- squares],
										and [(Set.size . lookup' s $ peers) == 20 | s <- squares],
										lookup' "C2" units == [["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2"],
          			                 					["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"],
                		           					["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"]],
										lookup' "C2" peers == Set.fromList ["A2", "B2", "D2", "E2", "F2", "G2", "H2", "I2",
                               													"C1", "C3", "C4", "C5", "C6", "C7", "C8", "C9",
                               													"A1", "A3", "B1", "B3"] ]
			
----------------------------- Parse a Grid ----------------------------

parse_grid :: [Char] -> Maybe (Map.Map [Char] [Char])
-- Convert grid to a map of possible values, [ (square, digits) ], or
-- return False if a contradiction is detected.
parse_grid grid = 
	-- To start, every square can be any digit; then assign values from the grid.
	let
		values = Map.fromList [(s, digits) | s <- squares]
		reduce vs (s, d) = if elem d digits
													then assign vs s d
													else Just vs
	in foldM reduce values $ grid_values grid

grid_values :: [Char] -> [([Char], Char)]
-- Convert grid into a map of [ (square, char) ] with '0' or '.' for empties.
grid_values grid = 
	let
		chars = [c | c <- grid, elem c digits || elem c "0."]
	in assert' (length chars == 81) `seq` zip squares $ chars

----------------------------- Constraint Propagation ---------------------------

assign :: Map.Map [Char] [Char] -> [Char] -> Char -> Maybe (Map.Map [Char] [Char])
-- Eliminate all the other values (except d) from values[s] and propagate.
-- Return Just values, except return Nothing if a contradiction is detected.
assign vs s d = 
	let
		otd = List.delete d $ lookup' s vs
		reduce_elim vs' dd = eliminate vs' s dd
	in foldM reduce_elim vs otd 

eliminate :: Map.Map [Char] [Char] -> [Char] -> Char -> Maybe (Map.Map [Char] [Char])
-- Eliminate d from values[s]; propagate when values or places <= 2.
-- Return values, except return False if a contradiction is detected.
eliminate vs s d
	| elem d (lookup' s vs) == False = Just vs
	| otherwise = 
		let
			vs' = Map.update (\v -> Just $ List.delete d v) s vs
			-- (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
			propagate_to_peers vs''
				| length (lookup' s vs'') == 0 = Nothing -- Contradiction: removed last value

				| length (lookup' s vs'') == 1 =
					let 
						d2 = (lookup' s vs'') !! 0
						reduce_elim vs''' ss = eliminate vs''' ss d2
					in foldM reduce_elim vs'' $ Set.toList $ lookup' s peers
				| True = Just vs''
			-- (2) If a unit u is reduced to only one place for a value d, then put it there.
			propagate_to_units vs'' =
				let reduce vs''' u = 
					let
						dplaces = [ss | ss <- u, elem d $ lookup' ss vs''']
						propagate dplaces' 
							-- Contradiction: no place for this value
							| length dplaces' == 0 = Nothing
							-- d can only be in one place in unit; assign it there
							| length dplaces' == 1 = 
								assign vs''' (dplaces' !! 0) d 
							| True = Just vs'''
					in 
						propagate dplaces
				in foldM reduce vs'' $ lookup' s units
		in
			do
				vs'' <- propagate_to_peers vs'
				result <- propagate_to_units vs''
				return result

--------------------------- Display as 2-D grid ---------------------------

display ::  Maybe (Map.Map [Char] [Char]) -> [Char]
-- Display these values as a 2-D grid.
display values = 
	let
		vs = fromJust values
		width = 1 + List.maximum [length $ lookup' s vs | s <- squares]
		line' = List.replicate (3 * width) '-'
		line = line' ++ "+" ++ line' ++ "+" ++ line'
		do_cols r o c =
			let
				s' = lookup' [r, c] vs
				s = s' ++ (List.replicate (width - length s') ' ')
			in
				if elem c "36"
					then o ++ s ++ "|"
					else o ++ s
		do_rows o r = 
			let
				column = List.foldl' (do_cols r) "" cols
			in
				if elem r "CF"
					then o ++ column ++ "\n" ++ line ++ "\n"
					else o ++ column ++ "\n"
	in
		List.foldl' do_rows "" rows 

--------------------------- Search ---------------------------

solve grid = search $ parse_grid grid

-- Using depth-first search and propagation, try all possible values.
search Nothing = Nothing -- Failed earlier
search (Just vs) =
	if and [1 == (length $ lookup' s vs) | s <- squares]
		then Just vs
		else -- Chose the unfilled square s with the fewest possibilities
			let 
				(n, s) = List.minimum [(length $ lookup' s vs, s) | s <- squares, (length $ lookup' s vs) > 1]
				result = List.find (\v -> v /= Nothing) [search $ assign vs s d | d <- lookup' s vs]
			in case result of
					Just r -> r
					_ -> Nothing
	
--------------------------- Utilities --------------------------

from_file filename consumer = 
	withFile filename ReadMode (\handle -> do
			contents <- hGetContents handle
			consumer $ lines contents
			return ())

-- Refer to http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> IO [a]
-- Return a randomly shuffled copy of the input sequence.
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

-- Refer to https://speely.wordpress.com/2010/08/16/selecting-a-random-element-in-haskell/
pick :: [a] -> IO a
pick xs = do
	r <- randomRIO (0, (length xs - 1))
	return $ xs !! r

--------------------------- System test --------------------------

time_solve :: [Char] -> IO (Double, Int)
time_solve grid = do
	start <- getCPUTime
	let values = solve grid
	rnf values `seq` return ()
	end <- getCPUTime
	let t = (fromIntegral (end - start)) / (10^12)
	return (t, solved values)

solve_all :: [Char] -> [[Char]] -> IO ()
-- Attempt to solve a sequence of grids. Report results.
-- When showif is a number of seconds, display puzzles that take longer.
-- When showif is None, don't display any puzzles.
solve_all name grids = do
	r' <- mapM time_solve grids
	let r = unzip r'
	let times = fst r
	let results = snd r
	let n_int = length grids :: Int
	let n = fromIntegral $ n_int
	let n_solved = sum results :: Int
	if n > 1
		then printf "Solved %d of %d %s puzzles (avg %.2f secs (%d Hz), max %.2f secs).\n"  n_solved n_int name ((sum times)/n) (round (n/(sum times)) :: Int) (maximum times)
		else return ()
				
solved :: Maybe (Map.Map [Char] [Char]) -> Int
-- A puzzle is solved if each unit is a permutation of the digits 1 to 9.
solved Nothing = 0
solved (Just vs) =
	let unitsolved unit = (Set.fromList [(lookup' s vs) !! 0 | s <- unit]) == (Set.fromList digits)
	in if and [unitsolved unit | unit <- unitlist]
				then 1
				else 0

reduce_puzzle :: Int -> (Map.Map [Char] [Char], Int) -> [Char] -> IO (Map.Map [Char] [Char], Int) 
reduce_puzzle n (vs, done) s =  do
		if done == 0
			then do
				picked <- pick (lookup' s vs)
				let r = assign vs s picked
				case r of
					Just vs' -> do
						let ds = [lookup' s vs' | s <- squares, (length $ lookup' s vs') == 1]
						if length ds >= n && Set.size (Set.fromList ds) >= 8
							then return (vs', 1)
							else return (vs', 0)
					_ -> return (vs, -1)
			else
				return (vs, done)

random_puzzle :: Int -> IO [Char]
-- Make a random puzzle with n or more assignments. Restart on contradictions.
-- Note the resulting puzzle is not guaranteed to be solvable, but empirically
-- about 99.8% of them are solvable. Some have multiple solutions.
random_puzzle n = do
	let values = Map.fromList [(s, digits) | s <- squares]
	shuffled <- shuffle squares
	puzzle <- foldM (reduce_puzzle n) (values, 0) shuffled
	case puzzle of
		(vs, 1) -> return [if (length $ lookup' s vs) == 1 then (lookup' s vs) !! 0 else '.' | s <- squares]
		_ -> random_puzzle n -- Give up and make a new puzzle

grid1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
main = do
			from_file "easy50.txt" (solve_all "easy") 
			from_file "top95.txt" (solve_all "hard") 
			from_file "hardest.txt" (solve_all "hardest")
			puzzles <- sequence [random_puzzle 17 | _ <- [1..99]]
			solve_all "random" puzzles