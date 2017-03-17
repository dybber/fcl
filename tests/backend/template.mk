OUTPUTDIRS=build results
FCLFILES=$(TESTS:%=%.fcl)
CFILES= $(TESTS:%=build/%.c)
RESFILES= $(TESTS:%=results/%.res)

.PHONY: all
all: directories $(CFILES) test

directories: $(OUTPUTDIRS)

$(OUTPUTDIRS):
	mkdir -p $(OUTPUTDIRS)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"

.PRECIOUS: results/%.out build/%.exe

build/%.c: %.fcl
	fcl -o build/$* $*.fcl 

build/%.exe: build/%.c
	gcc -Wall -Wno-unused-variable -O2 -D_DEBUG -std=c99 -I../../../microcl/ -I../../../include/ -I/usr/local/cuda/include/ -L../../../microcl/ -L/usr/local/cuda/lib64 -o $@ $< -lmcl -lOpenCL

results/%.out: build/%.exe
	$< > $@

results/%.res: results/%.out
	@(diff -aq $< $*.expected > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.fcl: OK" > $@ \
         ; else \
             if [ -e $*.expected ]; then \
                echo "Test $*.fcl: *** ERR: file $< differs from $*.expected ***" > $@ \
             ; else \
                echo "Test $*.fcl: *** ERR: file $*.expected does not exist ***" > $@ \
             ; fi \
         ; fi)

clean:
	rm -rf build/* results/*.out results/*.res
