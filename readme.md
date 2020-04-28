# File Manager for DrRacket

![default](https://user-images.githubusercontent.com/22510026/43937527-cf6141a8-9c90-11e8-9277-9d6e20b12e8b.png)

`files-viewer` provides a file manager for DrRacket. The project is under MIT/Apache 2.0 licenses (same as Racket).

Issues and pull requests are welcome.

The current version of `files-viewer` is 0.00.46.

## Installation

`files-viewer` could be installed in command-line via `raco pkg install files-viewer`.
Alternatively, it could be installed via DrRacket's package manager 
(_File > Package Manager_)

Please see the [documentation](https://docs.racket-lang.org/files-viewer/) for more details.

## Usage

To open/edit a file, double click the item.

To call the popup menu, right click the file manager.

To show or hide the file manager, go to _View > Show/Hide the File Manager_. 

## Update Log

- 0.00.44: Don't open any binary files in new tabs by default.
- 0.00.45: Add git push button and git pull button (by run commands "git pull origin master" and "git push origin master")
- 0.00.46: Add commit message window;add template for info.rkt;set the default value for terminal luancher on windows and macos,closes #23


