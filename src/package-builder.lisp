(in-package #:deb-packager)

(defun run (command)
  (multiple-value-bind (output error-output status)
      (uiop:run-program command :output t :error-output t :ignore-error-status t)
    (unless (= status 0)
      (format t "An error occured~%"))
    (values output error-output)))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun build-source (name source-folder arch depends)
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
              " apt-get install -y " (format nil "~{~A ~}" depends)))
    (run (cat "chroot " chroot-folder
              " mkdir -p /tmp/installed"))
    (run (cat "chroot " chroot-folder
              " ./configure && make && make DESTDIR=/tmp/installed install"))))
