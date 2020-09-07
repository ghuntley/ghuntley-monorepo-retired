UPSTREAM repo = https://github.com/numbleroot/i3blocks-go

# i3blocks-go

[![License: GPLv3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://github.com/numbleroot/i3blocks-go/blob/master/LICENSE) [![Go Report Card](https://goreportcard.com/badge/github.com/numbleroot/i3blocks-go)](https://goreportcard.com/report/github.com/numbleroot/i3blocks-go)

Scripts for alternative [i3blocks](https://github.com/vivien/i3blocks) blocklets to be used in the [i3 window manager](https://i3wm.org/). I wanted to modify and expand the default blocklets available with i3blocks. Additionally, I preferred them to be in ELF format and not an interpreted scripting language which is why they are implemented in Go. My final i3blocks setup looks like:

![Screenshot of numbleroot's i3blocks configuration using i3blocks-go](screenshot.png)


## Requirements

If you want to build this project from source, you need to have a working [Go](https://golang.org/) installation. If you do not currently have Go installed and configured, please find out your distribution's recommended way of doing so and follow it through.

As this project provides alternative blocklets for a configured i3blocks bar in an i3 window manager, both i3 and i3blocks are required to be present. [Fira Mono](https://mozilla.github.io/Fira/) is used in later examples, so install the font on your system if you would like to configure your bar to look like the one in above screenshot.


## Installation

### Build From Source

First, retrieve this project by running:
```
$ go get -u github.com/numbleroot/i3blocks-go
```

Change into the newly created directory and `make install` the provided source files:
```
$ cd ${GOPATH}/src/github.com/numbleroot/i3blocks-go
$ make install
```

Now, all compiled blocklets will be ready to be executed at `~/.config/i3blocks-go/`:
```
$ ls ~/.config/i3blocks-go
battery  date-time  internal-ip  load-average  public-ip  temperature  uptime
```


## Usage in i3blocks

Provided will be a minimum needed configuration to integrate this project's blocklets into i3 and i3blocks.

The `bar` section of your `~/.config/i3/config` file should at least contain the following:
```
bar {
        status_command i3blocks
        font pango:Fira Mono 20
}
```

Next, consult provided [i3blocks example config](https://github.com/numbleroot/i3blocks-go/blob/master/example-i3blocks.conf) to select, configure, and display the blocklets you would like to use.

Reload i3 and enjoy beautiful blocklets.


## Blocklet Specifics

**public-ip**: Queries [ip.wirelab.org](https://ip.wirelab.org/) in defined interval to determine your public IP address.

**internal-ip**: Displays the first found IP address associated with the first network interface marked as being active and not the loopback interface of your machine.

**uptime**: By default, shows your machine's uptime in format `hh:mm`. If you would like to see the seconds value as well, change the `command` value of blocklet `uptime` to `command=~/.config/i3blocks-go/uptime -showSeconds`. Consults your system's `/proc/uptime` file.

**load-average**: If enough space is available, shows your machine's load average for the last 1, 5, and 15 minutes provided by `/proc/loadavg` file. If space is limited, only shows the load average of the last minute.

**temperature**: Reads the CPU temperature from `/sys/class/hwmon/hwmon0/temp1_input` and displays the value. The font color is set accordingly to provided `-highTemp XX` and `-criticalTemp YY` temperature values.

**battery**: Consults three files located at `/sys/class/power_supply/` to simply show your machine's current charge percentage appropriately colored. Exclude this blocklet if your machine does not feature a battery.

**volume**: Executes `amixer sget Master` and extracts volume level in percent as well as whether your speakers are currently muted. Outputs fitting icon and volume level.

**date-time**: Displays current date and time formatted as `yyyy-mm-dd hh:mm`. If you would like to see seconds as well, append flag `-showSeconds` to `command`.


## License

This project is [GPLv3](https://github.com/numbleroot/i3blocks-go/blob/master/LICENSE) licensed.
