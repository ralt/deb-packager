ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd) $(wildcard t/*.lisp)

.PHONY: dist manual

deb-packager: quicklisp-manifest.txt $(SOURCES)
	@buildapp  --manifest-file quicklisp-manifest.txt \
		--eval '(push "$(ROOT_DIR)/" asdf:*central-registry*)' \
		--load-system deb-packager \
		--eval '(deb-packager:disable-debugger)' \
		--compress-core \
		--output deb-packager --entry deb-packager:main

quicklisp-manifest.txt:
	@sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :deb-packager)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

manual:
	@pandoc -s -t man docs/deb-packager.md > dist/deb-packager.1
	@gzip dist/deb-packager.1

dist:
	@make deb-packager
	@make manual
	@mkdir -p dist/
	@cp deb-packager dist/
	@./deb-packager dist/deb-packager.lisp
	@mv deb-packager_*_amd64.deb dist/
	@rm -f dist/deb-packager
