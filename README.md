# First-Order-Logic-Simplifier

I really enjoyed my univerity's compilers course and was inspired to try and create my own compiler for fun to implement some of the ideas that had been taught in the lectures. Another one of my favourite courses at the time was Logic and Proof, which involved a lot of simplification of logical expressions. And so I came up with an idea to create a source-to-source compiler which takes a logical expression in string form and converts it into its simplified [CNF](https://en.wikipedia.org/wiki/Conjunctive_normal_form) form while printing every step of this transformation. Eg. ¬(A ∨ B) ∨ C => (¬A ∧ ¬B) ∨ C => (¬A ∨ C) ∧ (¬B ∨ C). This was very useful as it allowed me to check my answers on my Logic and Proof worksheets!

The focus of this project was to gain a much more detailed and in-depth understanding of how lexers and parsers work, so instead of using prebuilt tools, I decided to design and hand-craft my own tools. I learnt a lot from this project, and I found it very exciting to apply the theory from my course to a real life compiler! 

To use this program, add a logical expression to bin/main.ml (examples given) and run the program. This will print out all the steps of the transformation to CNF. 
