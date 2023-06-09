# ProveML

A basic theorem prover in Ocaml that leverages the resolution-refutation method to prove or disprove a proposition given a knowledge base.

## How-To

- Ensure you have Ocaml installed. Instructions can be found online.
- Clone this repository by running <code>git clone</code>
- Run <code>make build</code> to compile the code.
- Edit <code>bin/main.ml</code> to use the the resolution-algorithm
- Steps for defining propositions, creating knowledge bases, and calling the theorem prover are provided as comments in <code>bin/main.ml</code>
- Run <code>make prover</code> to run the file <code>bin/main.ml</code>.

## Notes

- All code for processing propositions, converting them to CNF (Conjunctive Normal Form), and then resolving them is avaiable in the <code>lib</code> folder.
- Documentation can be generated by calling <code>make doc</code>. Calling <code>make opendoc</code> also opens the html file with the documentation.
- Unit tests are written in <code>test/main.ml</code> and can be run by calling <code>make test</code>.
- To run tests on the resolution algorithm, edit <code>test/main.ml</code> and call <code>make test</code>.

## Possible feature additions

- Adding a parser and lexer which could allow for a REPL implementation as well as parsing user generated knowledge bases
- Expanding support to First Order Logic - helping prove many more theorems
- Employing NLP to convert english sentences to FOL, and returning english answers (Attempto Controlled English) as proofs - creating a logically "correct" chatbot (upto the soundness and completeness of FOL).
- Reach out to me at aa779@cornell.edu if you have questions, feedback, or want to collaborate on exapnding this project!
