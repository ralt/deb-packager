(asdf:defsystem #:deb-packager
  :description "Simply create a debian package by defining an s-expression."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:deb-packager-test)))
  :around-compile (lambda (thunk)
                    (declaim (optimize (safety 3)))
                    (funcall thunk))
  :depends-on (:immutable-struct
               :local-time
               :archive
               :flexi-streams
               :salza2
               :ironclad
               :djula
               :cl-ppcre)
  :components ((:module
                "src"
                :components
                ((:file "package")
                 (:file "deb-packager" :depends-on ("deb-package"))
                 (:file "deb-package" :depends-on ("ar"
                                                   "control-archive"
                                                   "data-archive"
                                                   "files"))
                 (:file "ar")
                 (:file "control-archive")
                 (:file "data-archive")
                 (:file "files")))))
