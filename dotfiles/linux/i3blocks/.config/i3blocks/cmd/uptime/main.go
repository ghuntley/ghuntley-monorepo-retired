package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"

	"io/ioutil"
)

func main() {

	// Expect a boolean indicator whether seconds
	// should be calculated as well.
	secondsFlag := flag.Bool("showSeconds", false, "When this flag is set, seconds will be included in uptime presentation.")
	flag.Parse()

	// Set display texts to defaults.
	var output string
	var fullText string = "error"
	var shortText string = "error"

	// Read uptime information from kernel
	// pseudo-file-system mounted at /proc.
	uptimeRaw, err := ioutil.ReadFile("/proc/uptime")
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to read uptime file: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Remove surrounding space and split at inner spaces.
	uptimeStrings := strings.Split(strings.TrimSpace(string(uptimeRaw)), " ")

	// Extract uptime string as whole seconds.
	uptimeSecondsRaw := strings.Split(uptimeStrings[0], ".")[0]
	uptimeSecondsInt, err := strconv.Atoi(uptimeSecondsRaw)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Could not convert uptime value: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Calculate corresponding hours, minutes, and seconds values.
	uptimeHours := uptimeSecondsInt / 3600
	uptimeMinutes := (uptimeSecondsInt / 60) - (uptimeHours * 60)
	uptimeSeconds := (uptimeSecondsInt - (uptimeHours * 3600)) - (uptimeMinutes * 60)

	// Depending on flag value, include representation of
	// seconds in uptime output string.
	if *secondsFlag == true {
		output = fmt.Sprintf("%02d:%02d:%02d", uptimeHours, uptimeMinutes, uptimeSeconds)
	} else {
		output = fmt.Sprintf("%02d:%02d", uptimeHours, uptimeMinutes)
	}

	fullText = output
	shortText = output

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
