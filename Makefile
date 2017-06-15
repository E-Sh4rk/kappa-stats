KaStats: KaStats.native
	cp $< $@

KaStats.native: clean
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build KaStats KaStats.native
