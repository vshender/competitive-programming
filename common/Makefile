.PHONY: test clean

ALL := main.exe

test: $(ALL)
	@for if in input??.txt; do \
		of="output$$(echo $$if | cut -c 6-)" ; \
		cp $$if input.txt ; \
		./main.exe < input.txt > output.txt ; \
		cmp $$of output.txt > /dev/null ; \
		if [[ $$? -eq 0 ]]; then \
			echo -e "\033[0;32mTest $$if passed\033[0m" ; \
		else \
			echo -e "\033[0;31mTest $$if failed\033[0m" ; \
			diff output.txt $$of ; \
			exit 1 ; \
		fi ; \
	done ;
	@rm -f input.txt output.txt

%.exe: %.ml
	ocamlopt -o $@ $<

clean:
	rm -f *~ \#*
	rm -f input.txt output.txt
	rm -f $(ALL) *.o *.cm[ix]
