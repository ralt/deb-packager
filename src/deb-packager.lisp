(in-package #:deb-packager)

(ftype get-item list symbol list)
(defun get-item (list keyword)
  (rest
   (find-if #'(lambda (item)
                (eq (car item) keyword))
            list)))

(defmacro define-deb-package (name &body forms)
  `(let*
       ((changelog-entries
         (make-array
          ,(length (get-item forms :changelog))
          :initial-contents (list ,@(mapcar
                                     #'(lambda (entry)
                                         `(make-instance 'changelog-entry ,@entry))
                                     (get-item forms :changelog)))))
        (package (make-instance 'deb-package
                                :name ',name
                                :changelog changelog-entries
                                :description ,@(get-item forms :description)
                                :architecture ,@(get-item forms :architecture)
                                :depends ',@(if (get-item forms :depends)
                                                (get-item forms :depends)
                                                '(nil))
                                :long-description
                                ,@(if (get-item forms :long-description)
                                      (get-item forms :long-description)
                                      '(""))
                                :maintainer ,@(get-item forms :maintainer)))
        (control-files
         (make-array
          ,(length (get-item forms :control-files))
          :initial-contents (list ,@(mapcar
                                     #'(lambda (file)
                                         `(make-instance
                                           'deb-file
                                           :path ,(getf file :path)
                                           :content ,(getf file :content)
                                           :size (length ,(getf file :content))
                                           :mode ,(or (getf file :mode) 644)))
                                     (get-item forms :control-files)))))
        (data-files
         (make-array
          ,(length (get-item forms :data-files))
          :initial-contents (list ,@(mapcar
                                     #'(lambda (file)
                                         `(make-instance
                                           'deb-file
                                           :path ,(getf file :path)
                                           :content ,(getf file :content)
                                           :size (length ,(getf file :content))
                                           :mode ,(or (getf file :mode) 644)))
                                     (get-item forms :data-files))))))
     (initialize-control-files package control-files)
     (initialize-data-files package data-files)
     (write-deb-file (package-pathname package) package)))

(defmacro define-deb-package-from-source (name source-folder &body forms)
  `(build-source ',name
                 ,source-folder
                 ,@(get-item forms :architecture)
                 ,@(get-item forms :build-depends)))

(ftype write-deb-file pathname deb-package null)
(defun write-deb-file (path package)
  (with-open-file (s path :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (write-bytes (ar-global-header) s)
    (write-bytes (ar-add-entry #p"debian-binary" (debian-binary)) s)
    (write-bytes (ar-add-entry #p"control.tar.gz" (control-archive package)) s)
    (write-bytes (ar-add-entry #p"data.tar.gz" (data-archive package)) s)))

(ftype write-bytes (vector (unsigned-byte 8)) stream null)
(defun write-bytes (bytes stream)
  (loop
     :for byte across bytes
     :do (write-byte byte stream)))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :version
   :description "print the version"
   :short #\v
   :long "version"))

(defun help ()
  (opts:describe
   :prefix "deb-packager - Simply create a debian package by defining an s-expression."
   :usage-of "deb-packager"
   :args "FILE"))

(defun main (args)
  (declare (ignore args))
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~A needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~A as argument of ~A~%"
                  (opts:raw-arg condition)
                  (opts:option condition))))
    (when-option (options :help)
      (help)
      (uiop:quit))
    (when-option (options :version)
      (format t "1.0~%")
      (uiop:quit))
    (unless free-args
      (help)
      (uiop:quit))
    (when (> (length free-args) 1)
      (format t "fatal: too many arguments~%")
      (uiop:quit -1))
    (let ((file (pathname (first free-args))))
      (unless (probe-file file)
        (format t "fatal: could not find ~A~%" (namestring file))
        (uiop:quit -1))
      (in-package #:deb-packager)
      (handler-case
          (load file :verbose nil :print nil)
        (file-error () (format t "fatal: file already exists~%"))
        (error () (format t "fatal: unknown error~%"))))))

(defun disable-debugger ()
  (labels
      ((exit (c h)
         (declare (ignore h))
         (format t "~A~%" c)
         (sb-ext:exit)))
    (setf *debugger-hook* #'exit)))
