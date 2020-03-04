# calculus-solver

To run our solver, simply use "stack run". It will ask for the user to input an expression to simplify,
    like "d/dx(a+b)", and then print out the results in a (not very) pretty format.
    It will also create a file output.tex in the main folder, which can then be compiled into a nice .pdf.
    We have included example.pdf in the main folder as a sample.


Current progress:

    - The data structures that we expect to need for the project are in Structures.hs.

    - Parsing of expressions and laws is implemented in Parser.hs, 
        and we also have a function to read laws from a .txt file.

    - Application of rules and special cases for simplification is handled in Derivation.hs.
        - Special cases for simplification include:
            - Simplifying d/dx(x) = 1 and d/dx(constant) = 0
            - Simplifying constant math (e.g. 1+2=3)
            - Combining like multiplication terms (e.g. x^2*x^4 = x^6)
            - Combining like addition terms (e.g. 3*x^2*y + 6*y*x^2 = 9*y*x^2)
        - Cases for simplification hidden because they apply to restructuring data structures only:
            - Combining nested ACOperations
            - Unwrapping an ACoperation with only one expression in the expression list

    - LaTeX.hs handles rendering of a Calculation object into a .tex file.

    - Printer.hs contains some basic code for printing calculations to the terminal
        (mostly for testing purposes)
