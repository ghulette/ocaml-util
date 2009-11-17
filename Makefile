OCAMLMAKEFILE = ./OCamlMakefile

RESULT  = litc
SOURCES = util.ml \
          little.ml \
					parser.mly \
					lexer.mll \
					litc.ml

include $(OCAMLMAKEFILE)
