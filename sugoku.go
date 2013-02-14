package main

import "fmt"
import "strings"

func cross(a []string, b []string) []string {
    result := make([]string,0)
    for _,i := range a {
        for _,j := range b {
            s := []string{i,j}
            result = append(result,strings.Join(s,""))
        }
    }
    return result
}

func main() {
    rows := []string{"A","B","C","D","E","F","G", "H", "I"}
    cols := []string{"1","2","3","4","5","6","7","8","9"}
    squares := cross(rows,cols)
}
