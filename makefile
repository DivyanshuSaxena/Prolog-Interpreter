all: lexer.cmo parser.cmo calc.cmo interpreter.cmo
	ocamlc -g -o calc unix.cma interpreter.cmo lexer.cmo parser.cmo calc.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml parser.cmi
	ocamlc -g -c parser.ml

calc.cmo: calc.ml
	ocamlc -g -c calc.ml

parser.cmi: parser.mli
	ocamlc -g -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll       # generates lexer.ml

parser.mli: parser.mly interpreter.cmo
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

parser.ml: parser.mly
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

interpreter.cmo: interpreter.ml
	ocamlc -g -c interpreter.ml

clean:
	rm -f *.cm*
	rm -f *.mli
	rm -f lexer.ml parser.ml