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

    - LaTeX.hs handles rendering of a Calculation object into a .tex file.

    - Printer.hs contains some basic code for printing calculations to the terminal
        (mostly for testing purposes)
