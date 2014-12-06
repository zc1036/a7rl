
# a7rl

**a7rl** is an interpreter for an exceedingly simple dialect of lisp that runs on the ATMega128 with the MicaZ mote. It is written using the [lavrock](https://github.com/zc1036/lavrock) high-level assembly library to generate code that can be assembled with an AVRASM32-compatible assembler, like [AVRA](http://avra.sourceforge.net). It's nowhere near complete yet, but I plan to keep working on it until it is. See conventions.org for more implementation details like register usage, though it may be outdated since I had to shirk some conventions to reach a certain milestone in time for the last lab of the semester.

## Implementation

If you compile the source code now, you'll get an assembly program which reads and evaluates a single form, and then enters a perpetual sleep-state until reset. Following is a very high overview of what a7rl can do at this time.

### Datatypes

A7rl supports the following types:

- 16-bit integer ("fixnum")
- "Cons" (basically a linked-list node that can store any two objects (or no objects) in the head (CAR) and tail (CDR) parts)
- Symbol
- Readtable
- Array
- Function (with subtypes normal function, macro, or special form)

All but conses and symbols are self-evaluating.

As of now, only integers, symbols, and conses can be read by the reader; functions and strings cannot be created but functions  can be called. Support for these hasn't been added due to time constraints, but support is easy to implement and will be done soon.

The internals support evaluating (besides the self-evaluating forms) special forms, functions, and macros, but they can't be created via evaluated code yet.

### Features

There are only a few features at the moment:

**Reader**

- The reader reads in an object from standard input according to the readtable (currently a statically-determined string embedded in the program image) and returns it. It supports lists, symbols, and fixnums.
- The readtable, stored in the dynamic variable `*READTABLE*`, determines what happens when a character is read. The ASCII characters in the numeric ranges 9, 10, 13, [32, 97), and [123, 127) are supported, which means all the printable characters except for lowercase letters.

**Evaluator**

- The evaluator supports dynamic and lexical variables (though only lexical can be created (with `LET`) right now), function calls (to special forms, macros, and normal functions, but these can only be created in assembly for now).

### Functions

A7rl supports the following three forms (all are "special forms"):

- `SET-LEDS`: Takes a single fixnum as an argument and sets the LED lights on the mote to reflect the least-significant three bits, where a "1" bit means "off". (This will be changed in the future so that 1 means on.)
- `LET`: Creates a lexical binding. Example:

        (LET (X 1) X)
        => 1

- `+`: adds two fixnums. Example:

        (LET (X 1) (+ X 4))
        => 5

## Future work

There are plans for a garbage collector but it hasn't been implemented yet, and if too much memory is allocated, the program will fatally error (freeze).

Support has to be added for operations like subtraction and creating functions and macros. The reader should be able to do things like read negative numbers, and characters probably need to have a type associated with them (i.e. constituent character, macro character, etc), though each readtable will require a non-negligible 72 more bytes in  memory if another byte is allocated for each character.
