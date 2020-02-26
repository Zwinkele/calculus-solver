# calculus-solver

Current progress:
    - The data structures that we expect to need for the project are in Structures.hs.
    - Parsing of expressions and laws is implemented in Parser.hs, and we also have a function to read laws from a .txt file.
    - Derivation.hs has the first draft of our code to produce calculations. We're not really happy with how messy it is; ideally we would take advantage of the monadic structure of Maybe and List to make it a bit less verbose. We plan to implement special logic for the laws d/dx(constant)=0 and d/dx(x)=1. We also will need special logic for dealing with constants like 2x-2x=0 or 1+2=3

Future plans:
    - Reasoning (week 2)
        - Clean up derivation code
        - Implement special cases
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
        - Solution: We're adding powers to the structure, logarithms can be defined in laws.txt.