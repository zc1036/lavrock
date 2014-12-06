
# Lavrock

**Lavrock** is a library for writing and (in the future) simulating AVR assembly in Common Lisp, geared particularly toward the ATMega128 microcontroller. It can be thought of as a "high-level assembly" DSL for lisp, and it allows full access to the Common Lisp language and libraries during execution.

The purpose of the library is to make use of Common Lisp's execellent code manipulation features to take some of the monotony and repetetiveness out of writing AVR assembly applications, thereby removing common sources of errors. For example, programmers using Lavrock should not need to worry about whether registers are pushed and popped onto and from stack in the right order, and the added second dimension of code structure, absent in normal assembly code, makes programs much easier to understand after they have been written.

## Features

At present, Lavrock has only the features that were necessary to implement [a7rl](https://github.com/zc1036/a7rl). They are:

- Automatic preservation and restoration of registers, both callee- and caller-preserved (though non are declared as being caller-preserved at the moment, and the capability to do so should be moved to the client-side in the future) declared to be used by a function
- Constructs for "if", "if-else", and "loop"
- Instructions that support using single registers, or multiple registers for multibyte operations, uniformly
- Dead-code elimination at the function level
- Scoped labels

## Future plans

- Support for all AVR instructions and ATMega128 registers
- If/else/else-if blocks and "switch" blocks
- Simulation capabilities
  - The simulator aspect of Lavrock is skeletal at present because there was insufficient time to develop it alongside the other features
  - Support for "variables"; allow functions to declare not only what registers that they use, but also simply variable names, which will be assigned registers depending on their size, and will be automatically spilled to the stack when necessary. This is one of the bigger features because it requires register allocation and all that that implies, and will require modification of the instructions to be able to accept variables as operands.
- Integration with Common Lisp's conditions system to facilitate easy debugging
