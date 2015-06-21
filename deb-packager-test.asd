(asdf:defsystem #:deb-packager-test
  :description "Test package for deb-packager."
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:deb-packager :prove :alexandria)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module
                "t"
                :components
                ((:file "package")
                 (:test-file "deb-packager"))))
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) c)))
