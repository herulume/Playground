package urlshort

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestMapHandler(t *testing.T) {

	t.Run("it uses the fallback for unknown routes", func(t *testing.T) {
		noPath := map[string]string{}
		mapHandler := createMapHandlerWithDefault(noPath)

		request, _ := http.NewRequest(http.MethodGet, "/unknown", nil)
		response := httptest.NewRecorder()

		mapHandler(response, request)

		got := response.Body.String()

		if got != fallbackResponse {
			t.Errorf("Expected fallback response to be %s, got %v",
				fallbackResponse, got)
		}
	})

	t.Run("it routes known routes", func(t *testing.T) {
		path := "/test"
		site := "https://test.com"
		knownPath := map[string]string{
			path: site,
		}
		mapHandler := createMapHandlerWithDefault(knownPath)

		request, _ := http.NewRequest(http.MethodGet, path, nil)
		response := httptest.NewRecorder()

		mapHandler(response, request)
		result := response.Result()

		assertStatus(t, result, http.StatusFound)
		assertURL(t, result, site)
	})
}

func createMapHandlerWithDefault(paths map[string]string) http.HandlerFunc {
	fallbackHandler := http.HandlerFunc(fallback)
	return MapHandler(paths, fallbackHandler)
}

func assertURL(t *testing.T, resp *http.Response, want string) {
	t.Helper()
	url, err := resp.Location()

	if err != nil {
		t.Fatal("Could not read location", err)
	}

	if url.String() != want {
		t.Errorf("Expected url to be %s, got %s", url, want)
	}
}

func assertStatus(t *testing.T, resp *http.Response, want int) {
	t.Helper()
	if resp.StatusCode != want {
		t.Errorf("Expected status to be %d, got %d",
			want, resp.StatusCode)
	}
}
