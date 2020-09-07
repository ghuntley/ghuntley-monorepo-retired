package main

import (
	"flag"
	"fmt"
	"os"
	"time"
)

func main() {

	// Allow a flag to add seconds to representation.
	secondsFlag := flag.Bool("showSeconds", false, "Specify to include seconds in date and time representation.")
	flag.Parse()

	// Set display texts to defaults.
	var fullText string
	var shortText string

	// Retrieve current time.
	nowTime := time.Now()

	// Build final output strings depending
	// on string lengths and seconds flag.
	if *secondsFlag == true {
		fullText = nowTime.Format("2006-01-02 15:04:05")
		shortText = nowTime.Format("06/01/02 15:04:05")
	} else {
		fullText = nowTime.Format("2006-01-02 15:04")
		shortText = nowTime.Format("06/01/02 15:04")
	}

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
