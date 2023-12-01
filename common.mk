# Common build and test rules for OCaml solutions.
# Include from a problem directory: include ../../../common.mk

.PHONY: test clean

ALL := main.exe

# Run all tests by feeding each inputNN.txt to the program and comparing
# its output against the corresponding outputNN.txt.  Stops on the first
# failure, showing the diff.
test: $(ALL)
	@for inf in input??.txt; do \
		outf="output$$(echo $$inf | cut -c 6-)" ; \
		cp $$inf input.txt ; \
		./main.exe < input.txt > output.txt ; \
		cmp $$outf output.txt > /dev/null ; \
		if [[ $$? -eq 0 ]]; then \
			echo -e "\033[0;32mTest $$inf passed\033[0m" ; \
		else \
			echo -e "\033[0;31mTest $$inf failed\033[0m" ; \
			diff output.txt $$outf ; \
			exit 1 ; \
		fi ; \
	done ;
	@rm -f input.txt output.txt

# Compile an OCaml source file to a native executable.
%.exe: %.ml
	ocamlopt -o $@ $<

# Remove build artifacts and temporary files.
clean:
	rm -f *~ \#*
	rm -f input.txt output.txt
	rm -f $(ALL) *.o *.cm[ix]
