package main

import (
	"github.com/ghuntley/depot/patterns/nix/golang/lib"
    "net/http"
    "log"
)

func main() {
    http.HandleFunc("/health", lib.HealthCheckHandler)

    port := ":8080"
    log.Printf("Serving demo at localhost%s/health", port)
    log.Fatal(http.ListenAndServe(port, nil))
}
