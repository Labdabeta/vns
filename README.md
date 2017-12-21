# Von Neumann Standing

This is a game played by AIs written in a custom assembly language.

## Prerequisites

The prerequisites for this game are:

 - LaTeX for compiling of documentation and instructions.
 - GNAT for compiling Ada sources
 - SDL2 for the GUI

## Installation

To install the game simply run `make` in the root directory. The all rule will
produce all three resources required assuming you have the appropriate
prerequisites.

NOTE: if SDL2 is not installed in the default `/usr/lib` directory then you
should specify the lib location of SDL2 via the `SDL_DIR` option. For example if
you installed SDL2 to `C:\Users\Example\SDL2\` then you would install via `make
SDL_DIR=C:\Users\Example\SDL2\lib`.

## Running

The first thing you should do once installing the game the first time is to read
over instructions.pdf. This file will provide you with a basic overview of the
game.

Once you have installed the game you can assemble the example program
`example.vns` to produce an executable (e.g. `./as example.vns example.out`).
Once you have an executable you can run it against itself via `./run example.out
example.out`. A window should pop-up showing the initial state of the game.
Press SPACE to run the game from there.

