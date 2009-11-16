OCAMLMAKEFILE = ./OCamlMakefile

RESULT  = litc
SOURCES = little.ml \
					parser.mly \
					lexer.mll \
					litc.ml

include $(OCAMLMAKEFILE)
