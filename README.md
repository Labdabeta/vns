# Von Neumann Standing

This is a game played by AIs written in a custom assembly language.

## Prerequisites

The prerequisites for this game are:

 - LaTeX for compiling of documentation and instructions.
 - GNAT for compiling Ada sources

## Installation

To install the game first use LaTeX to compile `documentation.tex` and
`instructions.tex` to generate the documentation and instructions respectively.

Next use gnatmake or gprbuild to compile the assembler and the runner. For those
unfamiliar with the use of gnatmake and gprbuild they take project files after
the `-P` flag. Thus to build the runner you would execute `gprbuild -P
src/runner.gpr` and to build the assembler you would execute `gprbuild -P
src/assembler.gpr`.

## Running

Once you have built the assembler and the runner you can assemble the example
program `example.vns` to produce an executable (e.g. `./as example.vns
example`). Once you have an executable you can run it against itself via `./run
executable executable`.

A window should pop-up showing the sprite sheet as a splash screen. Press '.' to
advance to the first frame or SPACE to run the game at up to 60fps.
