(in-package #:deb-packager)

(defun run (command)
  (multiple-value-bind (output error-output status)
      (uiop:run-program command :output t :error-output t :ignore-error-status t)
    (unless (= status 0)
      (format t "An error occured~%"))
    (values output error-output)))

(defun run-in-chroot (folder command)
  (run (cat "chroot " folder " bash -c '" command "'")))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun build-source (name source-folder arch depends repository)
  (let ((chroot-folder (cat "/tmp/"
                            (string-downcase (symbol-name name))
                            "-"
                            (write-to-string (get-universal-time))))
        (project-folder (first (last (pathname-directory (pathname source-folder))))))
    (run (cat "mkdir -p " chroot-folder))
    (run (cat "cdebootstrap --arch " arch
              " stable "
              chroot-folder
              " " repository
              " --include=" (format nil "~{~A~^,~}" depends)))
    (run (cat "cp -Rp " (namestring source-folder) " " chroot-folder "/tmp/"))
    (run-in-chroot chroot-folder "mkdir -p /tmp/installed")
    (run-in-chroot chroot-folder (cat "cd /tmp/" project-folder "; ./configure && make && make DESTDIR=/tmp/installed install"))))
