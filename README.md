# First-Order-Logic-Simplifier

This was a project I created to learn how to make a compiler. This is a source-to-source compiler which takes a logical expression as a string and converts it to its CNF (conjunctive normal form).

The focus here is on hand-crafting a lexer and parser to convert the string expression to an AST representing the logical expression, then applying transformations on this AST to convert it into CNF. 

To use this program, add a logical expression to bin/main.ml (examples given) and the program will print out all the steps of the transformation to CNF. 
