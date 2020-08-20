package urlshort

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestMapHandler(t *testing.T) {

	t.Run("it uses the fallback for unknown routes", func(t *testing.T) {
		mapHandler := createEmptyMapHandler()
		request, _ := http.NewRequest(http.MethodGet, "/unknown", nil)
		response := httptest.NewRecorder()

		mapHandler(response, request)

		got := response.Body.String()

		if got != fallbackResponse {
			t.Errorf("Expected fallback response to be %s, got %v",
				fallbackResponse, got)
		}
	})
}

func createEmptyMapHandler() http.HandlerFunc {
	pathToUrls := map[string]string{}
	fallbackHandler := http.HandlerFunc(fallback)
	return MapHandler(pathToUrls, fallbackHandler)
}
