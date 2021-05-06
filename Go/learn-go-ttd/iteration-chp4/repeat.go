package iteration

func Repeat(c string, times int) string {
	if times <= 0 {
		return ""
	}

	var repeated string
	for i := 0; i < times; i++ {
		repeated += c
	}
	return repeated
}
