package main

import "strings"
import "fmt"

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

func cross(x string, y string) []string {
    result := make([]string,0)
    a := strings.Split(x,"")
    b := strings.Split(y,"")
    for _,i := range a {
        for _,j := range b {
            s := []string{i,j}
            result = append(result,strings.Join(s,""))
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

    for _,s := range squares {
        if len(units[s]) != 3 {
            panic("bad unit")
        }
    }

    for _,s := range squares {
        if len(peers[s]) != 20 {
            panic("bad peer list")
        }
    }
    
    fmt.Println(peers["C2"])

    /* TODO: need assertions for units["C2"] and peers["C2"] */

}

// Parse a grid

func parse_grid(grid string) map[string]string {
    solution := make(map[string]string)
    // To start, every square can be any digit; then assign values from the grid.
    for _,s := range squares {
        solution[s] = digits
    }
    puzzle := grid_values(grid)
    for s,d := range puzzle {
        if strings.Contains("123456789",d) {
            solution,ok= assign(solution,s,d)
            if !ok {
                return solution, false
            }
        }
    }
    return solution,true
}

func grid_values(grid string) map[string]string {
    puzzle := make(map[string]string)
    i := 0
    for c := range strings.Split(grid,"") {
        if strings.Contains(digits, c) || c == "." {
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
    other_values := Replace(puzzle[s],s,"",-1)
    for d2 := range strings.Split(other_values) {
        if !eliminate(puzzle,s,d2) {
            return false
        }
    }
    return true
}

func eliminate(puzzle map[string]string, s string, d string) bool {
    if !strings.Contains(puzzle[s],d) {
        return true // Already eliminated
    }
    puzzle[s] = Replace(puzzle[s],d,"",-1)
    // (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
    if len(puzzle[s]) == 0 {
        return false // Contradiction, removed last value
    } else if len(puzzle[s]) == 1 {
        d2 := puzzle[s]
        for _,s2 := range peers[s] {
            if !eliminate(puzzle,s2,d2) {
                return false
            }
        }
    }
    // (2) If a unit u is reduced to only one place for a value d, then put it there.
    for _,u := range units {
        dplaces := []string
        for _,sq := range u {
            if strings.Contains(puzzle[s],d) {
                dplaces = append(dplaces,sq)
            }
        }
        num_spots = len(dplaces)
        if num_spots == 0 {
            return false // Contradiction: no place for this value
        } else if num_spots == 1 {
            if !assign(puzzle,dplaces[0],d) {
                return false
            }
        }        
    }
    return true
}

// Search

func solve(grid string) (map[string]string, bool) {
    return search(parse_grid(grid))
}

func search(puzzle map[string]string) (map[string]string, bool) {
    for _,s := range squares {
        if len(puzzle[s]) != 1 {
            min_square := "A1"
            min_size := 9
            for _,s := range squares {
                size = len(puzzle[s])
                if size > 1 && size <= min_size {
                    min_square = s
                    min_size = size
                }
            }
            for d := range strings.Split(puzzle[min_square]) {
                puzzle_copy := make(map[string]string)
                for k,v := range puzzle {
                    puzzle_copy[k] = v
                }
                
                if assign(puzzle_copy,min_square,d) {
                    result,ok := search(puzzle_copy)
                    if ok {
                        return result,true
                    }
                }
            }
        }
    }
    return puzzle,true
}

func main() {
    rows = "ABCDEFGHI"
    digits = "123456789"
    cols = digits
    squares = cross(rows,cols)

    unitlist = make([]unit,0)

    for _,c := range cols {
        unitlist = append(unitlist,cross(rows,string(c)))
    }
    for _,r := range rows {
        unitlist = append(unitlist,cross(string(r),cols))
    }
    rs := []string{"ABC","DEF","GHI"}
    cs := []string{"123","456","789"}

    for _,r := range rs {
        for _,c := range cs {
            unitlist = append(unitlist,cross(r,c))
        }
    }

    units = make(map[string]unitgroup)
    for _,s := range squares {
        group := make(unitgroup,0)
        for _,unit := range unitlist {
            for _,square := range unit {
                if square == s {
                    group = append(group,unit)
                    break
                }
            }
        }
        units[s] = group
    }
    
    peers = make(map[string]peerlist)

    for _,s := range squares {
        peer_set := make(map[string]int)
        for _,unit := range units[s] {
            for _,square := range unit {
                if square != s {
                    peer_set[square] = 1
                }
            }
        }
        peer_list := make(peerlist,len(peer_set))
        i := 0
        for k,_ := range peer_set {
            peer_list[i] = k            
            i++
        }
        peers[s] = peer_list
    }

    test()


}
