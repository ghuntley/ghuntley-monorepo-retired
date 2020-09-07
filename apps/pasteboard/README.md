![Icon](https://i.imgur.com/rVJVL3U.png)
## Pasteboard

Your favourite OSX command line clipboard manipulation libraries `pbcopy` and `pbpaste`ported over to Windows.

* pbcopy copies the standard input and places it in the specified pasteboard.
* pbpaste copies the data from the pasteboard and writes it to the standard output.

## Installation

Installation is done via [Chocolatey](https://chocolatey.org/packages/pasteboard):

    # cinst pasteboard

## Usage

To copy your SSH key to the clipboard:

    # cd .ssh
    # type id_rsa.pub | pbcopy

To write the contents of your clipboard to a file:

    # cd C:\tmp
    # pbpaste > output.txt


## With thanks to
* The icon "<a href="https://thenounproject.com/term/clipboard/28312" target="_blank">Clipboard</a>" designed by <a href="https://thenounproject.com/Ilsur" target="_blank">Ilsur Aptukov</a> from The Noun Project.
