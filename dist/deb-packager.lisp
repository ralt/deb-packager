(define-deb-package deb-packager
  (:architecture "amd64")
  (:description "Simply create a debian package by defining an s-expression.")
  (:long-description "Defining a debian package by using devscripts is relatively
painful. A list of files have to be created to be able to create the
debian package we want. deb-packager's aim is to solve this issue, and
make it painless to create a debian package. It also aims to be able to
reproduce everything dpkg-buildpackage can do.")
  (:maintainer "Florian Margaine <florian@margaine.com>")
  (:changelog
   (:version "1.0"
    :author "Florian Margaine <florian@margaine.com>"
    :message "Initial release."
    :date 1437004038))
  (:data-files
   (:path #p"usr/bin/deb-packager"
    :mode 755
    :content (alexandria:read-file-into-byte-vector "dist/deb-packager"))
   (:path #p"usr/share/man/man1/deb-packager.1.gz"
    :mode 644
    :content (alexandria:read-file-into-byte-vector "dist/deb-packager.1.gz"))))
