PROG=repogen
VERSION=version_gen.ml

.DEFAULT: all
.PHONY: clean byte native auto

byte: auto
	ocamlbuild $(PROG).byte

native: auto
	ocamlbuild $(PROG).native

clean:
	ocamlbuild -clean
	rm -f $(VERSION) 

all: byte

newversion:
	@rm -f $(VERSION)

auto: newversion $(VERSION)

version_gen.ml:
	@echo "(* version.ml *)" > $@
	@echo "(* generated in " `date` "*)" >> $@ 
	@echo 'let repogen_HASH = "'`git rev-parse HEAD`'"' >> $@
	@echo 'let repogen_TAG  = "'`git tag`'"'  >> $@
	@echo 'let repogen_BUILD_TIME = "'`date --rfc-3339="seconds"`'"' >> $@

