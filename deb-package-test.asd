(asdf:defsystem #:deb-package-test
  :description "Test package for deb-package."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:deb-package :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module
                "t"
                :components
                ((:file "package")
                 (:test-file "deb-package"))))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) c)))
