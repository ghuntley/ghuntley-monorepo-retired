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

	// Allow to specify high and critical thresholds.
	highFlag := flag.Int("highTemp", 72, "Specify which temperature threshold in Celsius is considered high.")
	criticalFlag := flag.Int("criticalTemp", 80, "Specify which temperature threshold in Celsius is considered critical.")
	flag.Parse()

	// Gather temperature thresholds.
	criticalTemp := *criticalFlag
	highTemp := *highFlag

	// Set display texts to defaults.
	var output string
	var fullText string = "error"
	var shortText string = "error"

	// Read CPU temperature information from kernel
	// pseudo-file-system mounted at /sys.
	tempRaw, err := ioutil.ReadFile("/sys/class/hwmon/hwmon0/temp1_input")
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to read CPU temperature file: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Trim spaces.
	tempString := strings.TrimSpace(string(tempRaw))

	// Convert temperature string to integer.
	temp, err := strconv.Atoi(tempString)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Could not convert temperature value: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Normalize temperature value.
	temp = temp / 1000

	// Build final output string.
	if (temp > highTemp) && (temp <= criticalTemp) {
		output = fmt.Sprintf("<span foreground=\"#ffae00\">%d°C</span>", temp)
	} else if temp > criticalTemp {
		output = fmt.Sprintf("<span foreground=\"#ff0000\">%d°C</span>", temp)
	} else {
		output = fmt.Sprintf("%d°C", temp)
	}

	fullText = output
	shortText = output

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
