(in-package #:deb-packager)

(defun run (command)
  (let ((s (make-string-output-stream)))
    (uiop:run-program command)
    (get-output-stream-string s)))

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
              " apt-get install " (format nil "~{~A ~}" depends)))))
