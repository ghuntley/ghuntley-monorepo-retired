package main

import (
	"fmt"
	"os"
	"strings"

	"io/ioutil"
	"net/http"
)

func main() {

	// Set display texts to defaults.
	var fullText string = "unknown"
	var shortText string = "unknown"

	// Request whats-my-ip service to return this
	// machine's public IP address via TLS.
	resp, err := http.Get("https://ip.wirelab.org/")
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to get response from public IP service: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Read-in body part of response containing
	// the raw IP address.
	ipRaw, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Could not read body part of IP service response: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}
	resp.Body.Close()

	// Remove surrounding space.
	ip := strings.TrimSpace(string(ipRaw))

	fullText = ip
	shortText = ip

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
