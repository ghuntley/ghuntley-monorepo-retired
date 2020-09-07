package main

import (
	"fmt"
	"os"
	"strings"

	"io/ioutil"
)

func main() {

	// Set display texts to defaults.
	var fullText string = "error"
	var shortText string = "error"

	// Read current load average information from kernel
	// pseudo-file-system mounted at /proc.
	loadRaw, err := ioutil.ReadFile("/proc/loadavg")
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to read load average file: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Remove surrounding space and split at inner spaces.
	loadStrings := strings.Split(strings.TrimSpace(string(loadRaw)), " ")

	// Depending on length of display text, construct
	// final output string.
	fullText = fmt.Sprintf("%s<span foreground=\"#999999\">-</span>%s<span foreground=\"#999999\">-</span>%s", loadStrings[0], loadStrings[1], loadStrings[2])
	shortText = fmt.Sprintf("%s", loadStrings[0])

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
