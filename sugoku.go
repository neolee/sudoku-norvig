package main

import "fmt"
import "strings"

type unit []string
type unitgroup []unit

var rows string
var cols string
var digits string
var squares []string
var unitlist []unit
var units map[string]unitgroup

func test() {

    if len(squares) != 81 {
        panic("wtf, the number of squares is not 81")
    }

    if len(unitlist) != 27 {
        fmt.Println("The size of unitlist is",len(unitlist))
        panic("wtf, the number of units is not 27")
    }

}

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
            for _,s2 := range unit {
                if s2 == s {
                    group = append(group,unit)
                    break
                }
            }
        }
        units[s] = group
    }
    
    fmt.Println(units)
    test()
}
