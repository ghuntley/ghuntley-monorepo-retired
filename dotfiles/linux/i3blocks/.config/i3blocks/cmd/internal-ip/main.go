package main

import (
	"fmt"
	"net"
	"os"
	"strings"
)

func main() {

	// Set display texts to defaults.
	var fullText string = "unknown"
	var shortText string = "unknown"

	// Get all interfaces associated with this machine.
	ifaces, err := net.Interfaces()
	if err != nil {

		// Write an error to STDERR, fallback display values
		// to STDOUT and exit with failure code.
		fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to retrieve local interfaces: %s", err.Error())
		fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
		os.Exit(0)
	}

	for _, iface := range ifaces {

		// If current interface is not marked as being
		// active, continue to next for loop iteration.
		if (iface.Flags & net.FlagUp) != 1 {
			continue
		}

		// If this interface is the machine's loopback
		// interface, continue to next for loop iteration.
		if (iface.Flags & net.FlagLoopback) == 4 {
			continue
		}

		// Retrieve all addresses belonging to
		// found active interface.
		addrs, err := iface.Addrs()
		if err != nil {
			fmt.Fprintf(os.Stderr, "[i3blocks-go] Failed to retrieve IP addresses associated with %s: %s", iface.Name, err.Error())
			fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
			os.Exit(0)
		}

		// Extract the actual IP from address string
		// and remove subnet information.
		ipRaw := strings.Split(addrs[0].String(), "/")

		fullText = ipRaw[0]
		shortText = ipRaw[0]

		break
	}

	// Write out gathered information to STDOUT.
	fmt.Fprintf(os.Stdout, "%s\n%s\n", fullText, shortText)
	os.Exit(0)
}
