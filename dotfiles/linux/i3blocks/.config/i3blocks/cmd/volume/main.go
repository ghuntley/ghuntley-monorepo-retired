package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"

	"os/exec"
)

// volume queries the amixer command
// and attempts to extract volume level
// as well as muted information from the
// commands output.
func volume() (int, error) {

	// Define aregular expression matching for
	// currently set volume level.
	var volRegex = regexp.MustCompile(`\[(\d{1,3})\%\]`)

	// Return value for volume level. Setting it
	// to -1 indicates that speakers are muted.
	vol := 0

	// Execute 'amixer sget Master' and store
	// output of this command written to either
	// STDOUT or STDERR.
	cmd := exec.Command("amixer", "sget", "Master")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return 0, err
	}

	scanner := bufio.NewScanner(bytes.NewBuffer(out))

	for scanner.Scan() {

		text := scanner.Text()

		// Walk over received command output and check
		// for speaker sub-string.
		if strings.Contains(text, "Front Left: Playback") {

			// If speakers are muted, return -1.
			if strings.Contains(text, "[off]") {
				return -1, nil
			}

			// Otherwise, attempt to match above regex
			// on speaker string.
			matches := volRegex.FindStringSubmatch(text)
			if len(matches) != 2 {
				return 0, fmt.Errorf("expected two matches but found %d", len(matches))
			}

			// Convert extracted volume string to integer.
			vol, err = strconv.Atoi(matches[1])
			if err != nil {
				return 0, err
			}

			break
		}
	}

	return vol, nil
}

func main() {

	// Set display texts to defaults.
	var output string
	var fullText string = "unknown"
	var shortText string = "unknown"

	// Retrieve current volume status from amixer.
	vol, err := volume()
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to get volume: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	// Check if speakers are muted. Set final
	// output string accordingly.
	if vol == -1 {
		output = "<span foreground=\"#777777\"></span>  off"
	} else if vol == 0 {
		output = fmt.Sprintf(" %3d%%", vol)
	} else if (vol > 0) && (vol <= 49) {
		output = fmt.Sprintf(" %3d%%", vol)
	} else {
		output = fmt.Sprintf(" %3d%%", vol)
	}

	fullText = output
	shortText = output

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
