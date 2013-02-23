package main

import "strings"
import "fmt"
import "io/ioutil"
import "time"
import "math/rand"

type unit []string
type unitgroup []unit
type peerlist []string
type puzzle_t map[string]string

var rows string
var cols string
var digits string
var squares []string
var unitlist []unit
var units map[string]unitgroup
var peers map[string]peerlist

const puzzle_n int = 17

func cross(x string, y string) []string {
        result := make([]string, 0)
        a := strings.Split(x, "")
        b := strings.Split(y, "")
        for _, i := range a {
                for _, j := range b {
                        s := []string{i, j}
                        result = append(result, strings.Join(s, ""))
                }
        }
        return result
}

func test() {

        if len(squares) != 81 {
                panic("wtf, the number of squares is not 81")
        }

        if len(unitlist) != 27 {
                panic("wtf, the number of units is not 27")
        }

        for _, s := range squares {
                if len(units[s]) != 3 {
                        panic("bad unit")
                }
        }

        for _, s := range squares {
                if len(peers[s]) != 20 {
                        panic("bad peer list")
                }
        }

        // fmt.Println(peers["C2"])

        /* TODO: need assertions for units["C2"] and peers["C2"] */

        fmt.Println("All tests pass.")
}

// Parse a grid

func parse_grid(grid string) (map[string]string, bool) {
        puzzle := grid_values(grid)

        // To start, every square can be any digit; then assign values from the grid.
        solution := make(map[string]string)
        for _, s := range squares {
                solution[s] = digits
        }
        for s, d := range puzzle {
                if strings.Contains(digits, d) {
                        if !assign(solution, s, d) {
                                return solution, false
                        }
                }
        }
        return solution, true
}

func grid_values(grid string) map[string]string {
        puzzle := make(map[string]string)
        i := 0
        for _, c := range strings.Split(grid, "") {
                valid := digits + "."
                if strings.Contains(valid, c) {
                        puzzle[squares[i]] = c
                        i++
                }
        }
        if len(puzzle) != 81 {
                panic("invalid puzzle")
        }
        return puzzle
}

// Constraint Propagation

func assign(puzzle map[string]string, s string, d string) bool {
        other_values := strings.Replace(puzzle[s], d, "", -1)
        for _, d2 := range strings.Split(other_values, "") {
                if !eliminate(puzzle, s, d2) {
                        return false
                }
        }
        return true
}

func eliminate(puzzle map[string]string, s string, d string) bool {
        if !strings.Contains(puzzle[s], d) {
                return true // Already eliminated
        }
        puzzle[s] = strings.Replace(puzzle[s], d, "", -1)
        // (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
        if len(puzzle[s]) == 0 {
                return false // Contradiction, removed last value
        } else if len(puzzle[s]) == 1 {
                d2 := puzzle[s]
                for _, s2 := range peers[s] {
                        if !eliminate(puzzle, s2, d2) {
                                return false
                        }
                }
        }
        // (2) If a unit u is reduced to only one place for a value d, then put it there.
        for _, u := range units[s] {
                dplaces := make([]string, 0)
                for _, sq := range u {
                        if strings.Contains(puzzle[sq], d) {
                                dplaces = append(dplaces, sq)
                        }
                }
                num_spots := len(dplaces)
                if num_spots == 0 {
                        return false // Contradiction: no place for this value
                } else if num_spots == 1 {
                        if !assign(puzzle, dplaces[0], d) {
                                return false
                        }
                }
        }
        return true
}

// Search

func solve(grid string) (map[string]string, bool) {
        puzzle, ok := parse_grid(grid)
        if ok {
                return search(puzzle)
        }
        return puzzle, ok
}

func search(puzzle map[string]string) (map[string]string, bool) {
        for _, s := range squares {
                if len(puzzle[s]) != 1 {
                        min_square := "A1"
                        min_size := 9
                        for _, s := range squares {
                                size := len(puzzle[s])
                                if size > 1 && size <= min_size {
                                        min_square = s
                                        min_size = size
                                }
                        }
                        for _, d := range strings.Split(puzzle[min_square], "") {
                                puzzle_copy := make(map[string]string)
                                for k, v := range puzzle {
                                        puzzle_copy[k] = v
                                }

                                if assign(puzzle_copy, min_square, d) {
                                        result, ok := search(puzzle_copy)
                                        if ok {
                                                return result, true
                                        }
                                }
                        }
                        return puzzle, false
                }
        }
        return puzzle, true
}

func max(values []int64) int64 {
        var max int64
        max = 0
        for _, v := range values {
                if v > max {
                        max = v
                }
        }
        return max
}

func sum(items []int64) int64 {
        var accum int64
        accum = 0
        for _, b := range items {
                accum += b
        }
        return accum
}

func bool2int(booleans []bool) []int64 {
        ints := make([]int64, 0)
        for _, b := range booleans {
                if b {
                        ints = append(ints, 1)
                } else {
                        ints = append(ints, 0)
                }
        }
        return ints
}

func time_solve(grid string) (int64, bool) {
        start := time.Now()
        nanos_start := start.UnixNano()
        puzzle, _ := solve(grid)
        end := time.Now()
        nanos_end := end.UnixNano()
        duration := nanos_end - nanos_start
        /*
           fmt.Println(grid)
           var solved_ []string
           for _,sq := range squares {
               solved_ = append(solved_,puzzle[sq])
           }
           fmt.Println(strings.Join(solved_,""))
        */
        return duration, solved(puzzle)
}

func from_file(filename string) []string {
        dat, _ := ioutil.ReadFile(filename)
        grids := strings.Split(string(dat), "\n")
        return grids[:len(grids)-1]
}

func nanoconv(nanos int64) float64 {
        return float64(nanos) / 1000000000.0
}

func solve_all(grids []string, name string) {
        times := make([]int64, 0)
        results := make([]bool, 0)

        for _, grid := range grids {
                t, result := time_solve(grid)
                times = append(times, t)
                results = append(results, result)
        }

        n := len(grids)
        if n > 1 {
                fmt.Printf("Solved %d of %d %s puzzles (avg %.4f secs (%.2f Hz), max %.4f secs).\n",
                        sum(bool2int(results)), n, name, float64(nanoconv(sum(times)))/float64(n), float64(n)/float64(nanoconv(sum(times))), nanoconv(max(times)))
        }
}

func unit_solved(puzzle map[string]string, unit_ unit) bool {
        set := make(map[string]int)
        for _, s := range unit_ {
                set[puzzle[s]] = 1
        }
        for _, d := range strings.Split(digits, "") {
                if _, ok := set[d]; !ok {
                        return false
                }
        }
        return true
}

func solved(puzzle map[string]string) bool {
        for _, unit_ := range unitlist {
                if !unit_solved(puzzle, unit_) {
                        return false
                }
        }
        return true
}

func random_puzzle() string {
        puzzle := make(map[string]string)
        for _, s := range squares {
                puzzle[s] = digits
        }
        shuffled := make([]string, len(squares))
        perm := rand.Perm(len(squares))
        for i, v := range perm {
                shuffled[v] = squares[i]
        }
        for _, s := range shuffled {
                elements := strings.Split(puzzle[s], "")
                if !assign(puzzle, s, elements[rand.Intn(len(elements))]) {
                        break
                }
                ds := make([]string, 0)
                for _, sq := range squares {
                        if len(puzzle[sq]) == 1 {
                                ds = append(ds, puzzle[sq])
                        }
                }
                set := make(map[string]int)
                for _, sq := range ds {
                        set[sq] = 1
                }
                if len(ds) >= puzzle_n && len(set) >= 8 {
                        out := make([]string, 0)
                        for _, sq := range squares {
                                if len(puzzle[sq]) == 1 {
                                        out = append(out, puzzle[sq])
                                } else {
                                        out = append(out, ".")
                                }
                        }
                        puzzle := strings.Join(out, "")
                        return puzzle
                }
        }
        return random_puzzle()
}

func main() {
        rows = "ABCDEFGHI"
        digits = "123456789"
        cols = digits
        squares = cross(rows, cols)

        unitlist = make([]unit, 0)

        for _, c := range cols {
                unitlist = append(unitlist, cross(rows, string(c)))
        }
        for _, r := range rows {
                unitlist = append(unitlist, cross(string(r), cols))
        }
        rs := []string{"ABC", "DEF", "GHI"}
        cs := []string{"123", "456", "789"}

        for _, r := range rs {
                for _, c := range cs {
                        unitlist = append(unitlist, cross(r, c))
                }
        }

        units = make(map[string]unitgroup)
        for _, s := range squares {
                group := make(unitgroup, 0)
                for _, unit := range unitlist {
                        for _, square := range unit {
                                if square == s {
                                        group = append(group, unit)
                                        break
                                }
                        }
                }
                units[s] = group
        }

        peers = make(map[string]peerlist)

        for _, s := range squares {
                peer_set := make(map[string]int)
                for _, unit := range units[s] {
                        for _, square := range unit {
                                if square != s {
                                        peer_set[square] = 1
                                }
                        }
                }
                peer_list := make(peerlist, len(peer_set))
                i := 0
                for k, _ := range peer_set {
                        peer_list[i] = k
                        i++
                }
                peers[s] = peer_list
        }

        /*
           grid1 := "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
           grid2 := "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
           hard1 := ".....6....59.....82....8....45........3........6..3.54...325..6.................."            
        */
        test()
        solve_all(from_file("easy50.txt"), "easy")
        solve_all(from_file("top95.txt"), "hard")
        solve_all(from_file("hardest.txt"), "hardest")
        random_puzzles := make([]string, 100)
        for j := 0; j < 100; j++ {
                random_puzzles[j] = random_puzzle()
        }
        solve_all(random_puzzles, "random")
}
