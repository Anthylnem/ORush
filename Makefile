all : orush

orush : port.cmo moves.cmo solver.cmo
	ocamlc -o orush port.ml moves.ml solver.ml rush.ml

port.cmo : port.ml
	ocamlc -c port.ml

moves.cmo : moves.ml
	ocamlc -c port.ml moves.ml

solver.cmo : solver.ml
	ocamlc -c port.ml moves.ml solver.ml

rush.cmo : rush.ml
	ocamlc -c rush.ml
