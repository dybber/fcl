TESTS=00_scalar_bool 01_scalar_int 02_scalar_double 03_function_mono 04_function_poly 05_signatures 06_let_poly 07_let_poly_extra

FCLFILES=$(TESTS:%=%.fcl)
TYPES=$(TESTS:%=%.fcl.types)

test: parsetest typechecktest

parsetest:
	@(for f in $(FCLFILES); do fcl --test-parser $$f; done;)

typechecktest:
	@(for f in $(FCLFILES); do echo -n "Typechecking $$f: "; fcl --typecheck --no-prelude $$f | diff $$f.types - && echo "OK."; done;)
