# Hasbrain - a brainfuck interpreter

A simple project to pass time. I might write more advanced program analysis
tools and a compiler later, at least to remind myself of how the Haskell LLVM
bindings work.

This repository contains a `hasbrain` library with two brainfuck parsers and
interpreters. It also provides a `hasbrain` executable with the single command
`hasbrain interpret FILE` that runs the (slightly nicer) interpreter on the
program in `FILE`. Very rudimentary documentation can be viewed by running
`hasbrain interpret` alone; by default, the interpreter takes program input from
stdin, encoded as pairs of hexadecimal numbers, and writes the program output to
stdout in the same format (with a little leading and trailing whitespace).

## The brainfuck language

A brainfuck program is a sequence of single-character instructions, possibly
interspersed with other, ignored, characters. These instructions are considered
to run on a virtual machine consisting of:

- a pointer into a data tape, filled with numbers of some description and possibly finite;
- an instruction list with an instruction pointer that may point past the end of
  the list, indicating a halt state;
- a stack of input values that the machine can take from one at a time; and
- a list of output values (initially empty) that the machine can add to as it
  runs

The instructions as they exist in source code and how they affect the virtual
machine are given below. Note that the only restriction on the syntax of a
brainfuck program is that the left and right brackets be balanced.

```
> | increment data pointer
< | decrement data pointer
+ | increment data pointer value
- | decrement data pointer value
. | output data pointer value
, | accept one input value and store in the data pointer
[ | if the data pointer value is zero, jump to the instruction after the matching ]
] | if the data pointer value is nonzero, jump to the instruction after the matching [
```

The [Wikipedia article](https://en.wikipedia.org/wiki/Brainfuck) gives a
description of the brainfuck language and its usual variants. The interpreters
in this package make the following (somewhat lax) decisions regarding the
virtual machine:

- all values are unsigned 8-bit numbers
- the data tape is two-way infinite and all cells are initialized to 0
- increment and decrement operations are performed modulo 256, so in particular
  overflow and underflow are not errors
- the input list is an infinite stream of values, and insufficient input is
  extended with 0 values as needed

## Possible future improvements

- Pretty-printed terminal output for the interpreter. A simple improvement:
  adding an option in the hex output for spaces between numbers and a limit on
  the number of columns so that the result is a nice table.
- ASCII input (with escape sequences) for the interpreter, since many brainfuck
  programs assume this encoding for input.
- Different "standards compliance" modes: setting limits on the tape length,
  having overflow and underflow cause exceptions.
- Better executable errors: nicer "file not found" errors, for one.
- Better command line interface: help text (e.g., explanations of the
  formatting), a `--help` option, a list of possible commands, better command
  documentation.
- A `Loop` interpreter with reified, streaming stepping instead of the current
  `MonadInteract` approach. This would allow for a streaming command line
  interpreter; the current one simply reads in all input at once, then writes
  all output at once. It would also make writing an interactive interpreter
  easier, one where input to the brainfuck program would be adjustable based on
  its prior output.
