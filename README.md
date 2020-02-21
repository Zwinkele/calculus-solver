# calculus-solver

Current progress:
    - The data structures that we expect to need for the project are in Structures.hs.
    - We've written the first draft of the code for basic parsing of expressions and laws in Parser.hs, and have an empty derivation function in Derivation.hs. Eventually, we'd like to read laws from a file, and read expressions from user input.

Future plans:
    - Finishing touches on parsing code (week 1)
    - Reasoning (week 2)
    - Special feature (week 3)
        - Possible idea: LaTeX output!

Details we've considered:
    - Function application looks a lot like multiplication i.e. is y(x) the same as y*x or y applied to x?
        - Possible solution: treat y(x) as function application always, require explicit * for multiplication.
    - Treating + and * as strictly binary operations might make simplification annoying.
        - Solution: Use lists
    - Maybe we want constants to be instances of Real instead of just Ints.
    - Unsure what d/dx(y) should be.
        - Possible solution: Treat non-x variables as constants unless they're functions, so d/dx(y) = 0, but d/dx(y(x)) = y'(x).
    - Do we want to give up on input like d/dx(x^x) or should we add arbitrary powers and logarithms?