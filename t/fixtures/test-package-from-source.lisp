(define-deb-package-from-source bar
  (:architecture "amd64")
  (:depends ("vim (> 7)")) ; optional
  (:build-depends ("make" "coreutils"))
  (:source
   (:folder #p"t/fixtures/")
   (:type :autotools)
   (:patches (#p"t/fixtures/patch")))
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
  (:control-files
   (:path #p"postinst"
    :mode 755
    :content (alexandria:read-file-into-byte-vector
              (asdf:system-relative-pathname "deb-packager-test" "t/fixtures/postinst")))))
