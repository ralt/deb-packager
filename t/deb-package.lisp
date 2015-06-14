(in-package #:deb-package-test)

(define-deb-package foo
  (:changelog
   (:version "1.0-1"
    :author "Foo Bar <foo@bar.com>"
    :message "Lorem ipsum.")
   (:version "1.1-1"
    :author "Bar Foo <bar@foo.com>"
    :message "Dolor sit amet.")))
