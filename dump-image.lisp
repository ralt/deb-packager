(ql:quickload :deb-packager)

(deb-packager:disable-debugger)

(sb-ext:save-lisp-and-die "deb-packager"
                          :toplevel #'deb-packager:main
                          :executable t
                          :compression t
                          :purify t)
