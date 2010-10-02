PROG=repogen


.PHONY: clean byte native

byte:
	ocamlbuild $(PROG).byte

native:
	ocamlbuild $(PROG).native

clean:
	ocamlbuild -clean

all: byte

