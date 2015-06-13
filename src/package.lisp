(defpackage #:deb-package
  (:use #:cl)
  (:shadowing-import-from #:immutable-struct :ftype)
  (:export :define-deb-package))
