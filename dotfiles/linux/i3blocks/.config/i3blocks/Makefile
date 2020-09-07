.PHONY: all clean deps build public-ip internal-ip uptime load-average temperature battery date-time volume install dir copy

all: clean build

clean:
	rm -f public-ip internal-ip uptime load-average temperature battery date-time volume

deps:
	go get -t ./...

build: public-ip internal-ip uptime load-average temperature battery date-time volume

public-ip:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/public-ip/

internal-ip:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/internal-ip/

uptime:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/uptime/

load-average:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/load-average/

temperature:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/temperature/

battery:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/battery/

date-time:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/date-time/

volume:
	CGO_ENABLED=0 go build -ldflags '-extldflags "-static"' ./cmd/volume/

install: build dir copy

dir:
	mkdir -p ~/.config/i3blocks-go

copy:
	cp public-ip ~/.config/i3blocks-go/
	cp internal-ip ~/.config/i3blocks-go/
	cp uptime ~/.config/i3blocks-go/
	cp load-average ~/.config/i3blocks-go/
	cp temperature ~/.config/i3blocks-go/
	cp battery ~/.config/i3blocks-go/
	cp date-time ~/.config/i3blocks-go/
	cp volume ~/.config/i3blocks-go/
