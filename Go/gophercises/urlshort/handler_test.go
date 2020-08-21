package urlshort

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

const (
	path = "/test"
	site = "https://test.com"
)

func TestMapHandler(t *testing.T) {

	t.Run("it uses the fallback for unknown routes", func(t *testing.T) {
		noPath := map[string]string{}
		mapHandler := createMapHandlerWithDefault(noPath)

		request, _ := http.NewRequest(http.MethodGet, "/unknown", nil)
		response := httptest.NewRecorder()

		mapHandler(response, request)

		assertBody(t, response.Result(), fallbackResponse)

	})

	t.Run("it routes known routes", func(t *testing.T) {
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

func TestYAMLHandler(t *testing.T) {
	yaml := fmt.Sprintf(`
- path: %s
  url: %s
`, path, site)
	t.Run("it uses the fallback for unknown routes", func(t *testing.T) {
		request, _ := http.NewRequest(http.MethodGet, "/unknown", nil)
		response := httptest.NewRecorder()

		yamlHandler := createYAMLHandler(t, yaml)
		yamlHandler(response, request)

		assertBody(t, response.Result(), fallbackResponse)
	})

	t.Run("it redirects for found url", func(t *testing.T) {
		fmt.Println(yaml)
		request, _ := http.NewRequest(http.MethodGet, path, nil)
		response := httptest.NewRecorder()

		yamlHandler := createYAMLHandler(t, yaml)
		yamlHandler(response, request)
		result := response.Result()

		assertStatus(t, result, http.StatusFound)
		assertURL(t, result, site)
	})
}

func createYAMLHandler(t *testing.T, yaml string) http.HandlerFunc {
	t.Helper()

	fallbackHandler := http.HandlerFunc(fallback)
	handler, err := YAMLHandler([]byte(yaml), fallbackHandler)
	if err != nil {
		t.Fatalf("could not create a YAML handler %v", err)
	}

	return handler
}

func createMapHandlerWithDefault(paths map[string]string) http.HandlerFunc {
	fallbackHandler := http.HandlerFunc(fallback)
	return MapHandler(paths, fallbackHandler)
}

func assertBody(t *testing.T, resp *http.Response, want string) {
	t.Helper()
	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		t.Fatal("Could not ready response body", err)
	}

	got := string(body)
	if want != got {
		t.Errorf("Expected response body to be %s, got %s",
			want, got)
	}
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
