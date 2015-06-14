(asdf:defsystem #:deb-package
  :description "Simply create a debian package by defining an s-expression."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:deb-package-test)))
  :around-compile (lambda (thunk)
                    (declaim (optimize (safety 3)))
                    (funcall thunk))
  :depends-on (:immutable-struct :local-time)
  :components ((:module
                "src"
                :components
                ((:file "package")
                 (:file "deb-package")))))
