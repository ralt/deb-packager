(in-package #:deb-packager)

(defun run (command)
  (multiple-value-bind (output error-output status)
      (uiop:run-program command :output t :error-output t :ignore-error-status t)
    (unless (= status 0)
      (format t "An error occured~%"))
    (values output error-output)))

(defun run-in-chroot (folder command)
  (run (cat "sudo chroot " folder " bash -c '" command "'")))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defgeneric build-source (type name forms)
  (:documentation "Builds the files from a defined source."))

(defmethod build-source ((type (eql :autotools)) name forms)
  (let ((chroot-folder (cat "/tmp/"
                            (string-downcase (symbol-name name))
                            "-"
                            (write-to-string (get-universal-time)))))
    (build-autotools (first (get-item (get-item forms :source) :folder))
                     chroot-folder
                     (first (get-item forms :architecture))
                     (first (get-item forms :build-depends))
                     (or (first (get-item (get-item forms :source) :repository))
                         "http://http.debian.net/debian"))
    (cleanup-files chroot-folder)
    (get-installed-files (cat chroot-folder "/tmp/installed/"))))

(defun build-autotools (source-folder chroot-folder arch depends repository)
  (let ((project-folder (first (last (pathname-directory (pathname source-folder))))))
    (run (cat "mkdir -p " chroot-folder))
    (run (cat "sudo cdebootstrap --arch " arch
              " stable "
              chroot-folder
              " --flavour=build "
              repository
              " --include=" (format nil "~{~A~^,~}" depends)))
    (run (cat "cp -Rp " (namestring source-folder) " " chroot-folder "/tmp/"))
    (run-in-chroot chroot-folder "mkdir -p /tmp/installed")
    (run-in-chroot chroot-folder (cat "cd /tmp/" project-folder "; ./configure && make && make DESTDIR=/tmp/installed install"))))

(defun get-installed-files (installed-files)
  (let ((data-files nil))
    (cl-fad:walk-directory
     installed-files
     #'(lambda (file)
         (let* ((stat (sb-posix:stat file))
                (mode (sb-posix:stat-mode stat)))
           (push `(:path ,(pathname
                           (format
                            nil
                                  "~{~A~^/~}"
                                  (nthcdr 3 (cl-ppcre:split "/" (namestring file)))))
                         :content ,(alexandria:read-file-into-byte-vector file)
                         :mode ,(parse-integer (subseq (format nil "~o" mode) 2)))
                 data-files))))
    data-files))

(defun cleanup-files (folder)
  (run (cat "sudo rm -rf " folder)))
