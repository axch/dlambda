dlambda - A Differentiable GADT-based interpreter
=================================================

This is me playing around with two ideas:

- Writing an interpreter for a toy language that uses GADTs to (i)
  type-check the object langauge, and (ii) avoid dispatching when
  evaluating.  This I unapologetically cribbed from
  https://github.com/goldfirere/glambda.

- Writing that interpreter so that functions defined in the object
  language can be differentiated by applying Ed Kmett's `ad` library
  through the interpreter.
