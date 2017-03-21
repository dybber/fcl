include ../../../shared/config.mk

build: $(BENCHNAME)

%.c %.cl: %.fcl
	fcl -o $* $*.fcl

$(BENCHNAME): $(BENCHNAME).c $(BENCHNAME).cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $(BENCHNAME).c $(LIB)

run: build
	./$(BENCHNAME)

clean:
	rm -f $(BENCHNAME) $(BENCHNAME).c $(BENCHNAME).cl

fcl:
	make -C ../../../..
