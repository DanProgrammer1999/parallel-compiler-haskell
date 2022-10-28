# Parallel Compiler in Haskell

This research project is inspired by the work of A. Hsu on parallel compilers in APL.I attempt to not just replicate his results, but to provide a Haskell library for a parallel compiler with easy-to-use pure functions.As in the original research, this implementation focuses on two crucial steps of building a compiler: *lifting functions* and *flattening expressions*.

The library allows to convert an AST to an array-based representation, provides intermediate steps of the whole process, and implements many general array operators, such as [key](https://xpqz.github.io/learnapl/key.html), `innerProduct`, `selectRows`, and many others.

## Technologies
- Haskell
  - Accelerate
  - Criterion
