KaTest: KaTest.native
	cp $< $@

KaTest.native: 
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build KaTest KaTest.native
