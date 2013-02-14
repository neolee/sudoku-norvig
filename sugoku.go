package main

import "fmt"
import "strings"

/* All this really could be much nicer, because I really don't know what I'm doing with golang yet. */

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
    rows := "ABCDEFGHI"
    digits := "123456789"
    cols := digits
    squares := cross(rows,cols)
    for _,i := range squares {
        fmt.Println(i)
    }
}
