package iteration

import "testing"
import "fmt"

func TestRepeat(t *testing.T) {

	assertCorrectString := func(t testing.TB, repeated, expected string) {
		t.Helper()
		if repeated != expected {
			t.Errorf("Expected %q, but got %q", expected, repeated)
		}
	}

	t.Run ("Repeat char 5 times", func (t *testing.T) {
		repeated := Repeat("a", 5)
		expected := "aaaaa"
		assertCorrectString(t, repeated, expected)

	})

	t.Run ("Return empty string if 0 is supplied times", func (t *testing.T) {
		repeated := Repeat("a", 0)
		expected := ""
		assertCorrectString(t, repeated, expected)
	})

	t.Run ("Return empty string if a negative argument is supplied times", func (t *testing.T) {
		repeated := Repeat("a", (-1))
		expected := ""
		assertCorrectString(t, repeated, expected)
	})
}

func BenchmarkRepeat(b *testing.B) {
	for i := 0; i < b.N; i++ {
		Repeat("a", 5)
	}
}

func ExampleRepeat() {
	repeated := Repeat("a", 5)
	fmt.Println(repeated)
	// Output: aaaaa
}
