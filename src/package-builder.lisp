(in-package #:deb-packager)

(defun build-package (name source-folder arch depends)
  (let ((chroot-folder (cat "/tmp/"
                            (string-downcase (symbol-name name))
                            "-"
                            (write-to-string (get-universal-time)))))
    (run (cat "mkdir -p " chroot-folder))
    (run (cat "cdebootstrap --arch " arch " jessie " chroot-folder
              " http://http.debian.net/debian"))
    (run (cat "chroot " chroot-folder " apt-get update"))
    (run (cat "cp -Rp " (namestring source-folder) " " chroot-folder "/tmp/"))
    (run (cat "chroot " chroot-folder
              " apt-get install " (format nil "~{~A ~}" depends)))))
