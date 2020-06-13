package lib

import (
	"github.com/google/go-cmp/cmp"
    "net/http"
    "net/http/httptest"
	"testing"
)

func TestHealthCheckHandler(t *testing.T) {
    request, err := http.NewRequest("GET", "/health-check", nil)
    if err != nil {
        t.Fatal(err)
    }

    recorder := httptest.NewRecorder()
    handler := http.HandlerFunc(HealthCheckHandler)

    handler.ServeHTTP(recorder, request)

    expectedStatus := http.StatusOK
    if diff := cmp.Diff(recorder.Code, expectedStatus); diff != "" {
        t.Errorf("HealthCheckHandler() status code mismatch (-want +got):\n%s", diff)
    }

    expectedBody := `{"alive": false}`
    if diff := cmp.Diff(recorder.Body.String(), expectedBody); diff != "" {
        t.Errorf("HealthCheckHandler() response body mismatch (-want +got):\n%s", diff)
    }
}
