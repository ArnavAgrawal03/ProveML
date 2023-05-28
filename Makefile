build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

count:
	dune clean
	cloc --by-file --include-lang=OCaml ..

prover:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	dune test