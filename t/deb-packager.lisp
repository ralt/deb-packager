(in-package #:deb-packager-test)

(deb-packager:define-deb-package foo
  (:changelog
   (:version "1.0-1"
    :author "Foo Bar <foo@bar.com>"
    :message "Lorem ipsum."
    :date 1434665940) ;; unix timestamp
   (:version "1.1-1"
    :author "Bar Foo <bar@foo.com>"
    :message "Dolor sit amet."
    :date 1434665998)))
