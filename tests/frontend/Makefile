TESTS=00_scalar_bool 01_scalar_int 02_scalar_double 03_function_mono 04_function_poly

FCLFILES=$(TESTS:%=%.fcl)
TYPES=$(TESTS:%=%.fcl.types)

test: parsetest typechecktest

parsetest:
	@(for f in $(FCLFILES); do fcl --test-parser $$f; done;)

typechecktest:
	@(for f in $(FCLFILES); do echo -n "Typechecking $$f: "; fcl --typecheck --no-prelude $$f | diff $$f.types - && echo "OK."; done;)
