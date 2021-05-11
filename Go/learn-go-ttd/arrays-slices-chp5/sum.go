package arrays_slices

func Sum(numbers []int) int {
	sum := 0
	for _, number := range numbers {
		sum += number
	}
	return sum
}

/* func SumAll(numbers ...[]int) []int {
	numberSlices := len(numbers)
	sums := make([]int, numberSlices)

	for i, numbers := range numbers {
		sums[i] = Sum(numbers)
	}

	return sums
}
*/
func SumAll(numbers ...[]int) []int {
	var sums []int

	for _, numbers := range numbers {
		sums = append(sums, Sum(numbers))
	}

	return sums
}

func SumAllTails(numbers ...[]int) []int {
	var sums []int

	for _, numbers := range numbers {
		if len(numbers) == 0 {
			sums = append(sums, 0)
		} else {
			tail := numbers[1:]
			sums = append(sums, Sum(tail))
		}
	}

	return sums
}
