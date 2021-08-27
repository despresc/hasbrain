# Hasbrain - a brainfuck interpreter

A simple project to pass time. I might write more advanced program analysis
tools and a compiler later, at least to remind myself of how the Haskell LLVM
bindings work.

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
  `MonadInteract` approach. I suppose the states would be `Yield Word8 Thing`
  and `Read (Word8 -> Thing)`? This would allow for a streaming command line
  interpreter; the current one simply reads in all input at once, then writes
  all output at once. It would also make writing an interactive interpreter
  easier, one where input to the brainfuck program would be adjustable based on
  its prior output.
