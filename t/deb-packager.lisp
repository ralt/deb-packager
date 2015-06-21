(in-package #:deb-packager-test)

(deb-packager:define-deb-package foo
  (:architecture "amd64")
  (:depends ("vim")) ; optional
  (:description "foobar bar qux")
  (:long-description "foo bar
bar
qux") ; optional
  (:maintainer "Foo Bar <foo@bar.com>")
  (:changelog
   (:version "1.0-1"
    :author "Foo Bar <foo@bar.com>"
    :message "Lorem ipsum."
    :date 1434665940) ;; unix timestamp
   (:version "1.1-1"
    :author "Bar Foo <bar@foo.com>"
    :message "Dolor sit amet."
    :date 1434665998))
  (:files
   (:path #p"usr/bin/foo" ; path to install to, without leading slash
    ;; :content is a byte vector of the file's content.
    :content (alexandria:read-file-into-byte-vector
              (asdf:system-relative-pathname "deb-packager-test" "t/fixtures/foo"))
    :mode 755)
   (:path #p"usr/bin/bar"
    :content (alexandria:read-file-into-byte-vector
              (asdf:system-relative-pathname "deb-packager-test" "t/fixtures/bar")))))
