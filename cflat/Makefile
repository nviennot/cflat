OBJS = parser.cmo scanner.cmo backend.cmo sast.cmo cflat.cmo

CFLAGS="-m32"

cflat : $(OBJS) lib.o
	ocamlc -o cflat $(OBJS)

.PHONY: test
test : cflat tester.py test-cflat.txt
	./tester.py test-cflat.txt

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f cflat parser.ml parser.mli scanner.ml testall.log *.cmo *.cmi *.o *.s test

# Generated by ocamldep *.ml *.mli
backend.cmo: ast.cmi
backend.cmx: ast.cmi
cflat.cmo: scanner.cmo sast.cmo parser.cmi backend.cmo
cflat.cmx: scanner.cmx sast.cmx parser.cmx backend.cmx
parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi
sast.cmo: ast.cmi
sast.cmx: ast.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmi

