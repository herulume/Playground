package iteraction


func Repeat(toRepeat string, n int) string {
    var repeated string

    for i := 0; i < n; i++ {
        repeated += toRepeat
    }

    return repeated
}
