OBJS = ast.cmo parser.cmo scanner.cmo sast.cmo compile.cmo toplevel.cmo

TESTS = \
print

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml compile.ml toplevel.ml \
	$(TESTS:%=tests/)

aml : $(OBJS)
	ocamlc str.cma unix.cma -o aml $(OBJS) 

.PHONY : test
test : aml testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

aml.tar.gz : $(TARFILES)
	cd .. && tar czf aml/aml.tar.gz $(TARFILES:%=aml/%)

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff *.class aml

# Generated by ocamldep *.ml *.mli
ast.cmo: ast.cmi
ast.cmx: ast.cmi
compile.cmo: ast.cmo 
compile.cmx: ast.cmx 
toplevel.cmo: sast.cmo scanner.cmo parser.cmi compile.cmo ast.cmi
toplevel.cmx: sast.cmx scanner.cmx parser.cmx compile.cmx ast.cmx
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
sast.cmo: ast.cmi
sast.cmx: ast.cmx 
parser.cmi: ast.cmi
ast.cmi:
