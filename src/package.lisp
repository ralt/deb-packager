(defpackage #:deb-packager
  (:use #:cl)
  (:shadowing-import-from #:immutable-struct :ftype)
  (:export :define-deb-package
           :main
           :disable-debugger
           :deb-package
           :deb-file
           :changelog-entry
           :initialize-control-files
           :initialize-data-files
           :write-deb-file
           :define-deb-package-from-source))

(in-package #:deb-packager)

;; External packages ftype declarations needed
(ftype flexi-streams:get-output-stream-sequence stream (vector (unsigned-byte 8)))
(ftype flexi-streams:make-in-memory-input-stream (vector (unsigned-byte 8)) stream)
(ftype salza2:compress-data (simple-array (unsigned-byte 8)) symbol (vector (unsigned-byte 8)))
