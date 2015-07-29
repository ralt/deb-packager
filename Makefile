SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd) $(wildcard t/*.lisp)

.PHONY: dist manual

deb-packager: $(SOURCES)
	@sbcl --load dump-image.lisp --quit

manual:
	@mkdir -p dist/
	@pandoc -s -t man docs/deb-packager.md > dist/deb-packager.1
	@gzip dist/deb-packager.1

dist:
	@make deb-packager
	@make manual
	@mkdir -p dist/
	@cp deb-packager dist/
	@./deb-packager package
	@mv deb-packager_*_amd64.deb dist/
	@rm -f dist/deb-packager dist/deb-packager.1.gz
