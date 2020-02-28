# calculus-solver

To run our solver, simply use "stack run". It will ask for the user to input an expression to simplify,
    like "d/dx(a+b)", and then print out the results in a (not very) pretty format.
    We plan to implement printing to a prettier format during the last week of the project.
    You can also try "stack test" to make sure our tests work, but it won't be very illuminating.



Current progress:

    - The data structures that we expect to need for the project are in Structures.hs.

    - Parsing of expressions and laws is implemented in Parser.hs, 
        and we also have a function to read laws from a .txt file.

    - The code for producing calculations from the given laws has been cleaned up a bit in Derivation.hs.
        We've just recently written functions to apply special rules that aren't able to be specified
        in laws.txt, like math with constants and d/dx(x)=1.
        We are still working on completely simplifying expressions to make it imitate 
        an actual calculus solution better.



Hardcoded logic:

    - d/dx(x) = 1

    - d/dx(int) = 0

        - more generally, d/dx(anything without an x)=0

    - all math w/ constants and +,-,*,^



Future plans:

    - Reasoning (week 2)

        - Clean up derivation code

        - Implement special cases

    - Special feature (week 3)

        - Possible idea: LaTeX output!



Details we've considered:

    - Function application looks a lot like multiplication i.e. is y(x) the same as y*x or y applied to x?

        - Possible solution: treat y(x) as function application always, 
            require explicit * for multiplication.

    - Treating + and * as strictly binary operations might make simplification annoying.

        - Solution: Use lists

    - Maybe we want constants to be instances of Real instead of just Ints.

    - Unsure what d/dx(y) should be.

        - Possible solution: 
            Treat non-x variables as constants unless they're functions, 
            so d/dx(y) = 0, but d/dx(y(x)) = y'(x).

    - Do we want to give up on input like d/dx(x^x) or should we add arbitrary powers and logarithms?

        - Solution: We're adding powers to the structure, logarithms can be defined in laws.txt.
