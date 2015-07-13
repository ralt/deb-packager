ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

deb-packager: quicklisp-manifest.txt
	@buildapp  --manifest-file quicklisp-manifest.txt \
		--eval '(push "$(ROOT_DIR)/" asdf:*central-registry*)' \
		--load-system deb-packager \
		--eval '(deb-packager:disable-debugger)' \
		--output deb-packager --entry deb-packager:main

quicklisp-manifest.txt:
	@sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :deb-packager)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'
