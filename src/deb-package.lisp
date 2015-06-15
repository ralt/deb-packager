(in-package #:deb-package)

(ftype get-item list symbol list)
(defun get-item (list keyword)
  (rest
   (find-if #'(lambda (item)
                (getf item keyword))
            list)))

(defmacro define-deb-package (name &body forms)
  `(let ((changelog-entries
          (make-array
           ,(length (get-item forms :changelog))
           :initial-contents (list ,@(mapcar
                                      #'(lambda (entry)
                                          `(make-instance 'changelog-entry ,@entry))
                                      (get-item forms :changelog))))))
     (let ((package (make-instance 'deb-package
                                   :name ',name
                                   :changelog changelog-entries)))
       (write-deb-file (package-pathname package) package))))

(defclass changelog-entry ()
  ((version :initarg :version
            :type string
            :initform (error "Version required."))
   (author :initarg :author
           :type string
           :initform (error "Author required."))
   (message :initarg :message
            :type string
            :initform (error "Message required.")))
  (:documentation "A single changelog entry."))

(defclass deb-package ()
  ((name :initarg :name
         :type symbol
         :initform (error "Name required."))
   (changelog :initarg :changelog
              :type (vector changelog-entry)
              :reader changelog
              :initform (error "Changelog required.")))
  (:documentation "Holds all the data required to generate a debian package."))

(ftype name deb-package string)
(defun name (package)
  "Gets the name of a package."
  (string-downcase (symbol-name (slot-value package 'name))))

(ftype package-pathname deb-package pathname)
(defun package-pathname (package)
  "Gets the pathname of a package."
  (pathname (concatenate 'string (name package) ".deb")))

(ftype write-deb-file pathname deb-package null)
(defun write-deb-file (path package)
  (with-open-file (s path :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (write-bytes (ar-global-header) s)
    (write-bytes (ar-add-entry #p"debian-binary" (debian-binary)) s)
    (write-bytes (ar-add-entry #p"control.tar" (control-archive package)) s)))

(ftype write-bytes (vector (unsigned-byte 8)) stream null)
(defun write-bytes (bytes stream)
  (loop
     :for byte across bytes
     :do (write-byte byte stream)))

(ftype debian-binary (vector (unsigned-byte 8)))
(defun debian-binary ()
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x32 #x2E #x30 #x0A)))

(ftype control-archive deb-package (vector (unsigned-byte 8)))
(defun control-archive (package)
  (let* ((out-stream (flexi-streams:make-in-memory-output-stream))
         (archive (archive:open-archive 'archive:tar-archive out-stream
                                        :direction :output)))
    (dolist (entry (control-archive-entries package)
             (archive:finalize-archive archive))
      (archive:write-entry-to-archive archive entry
                                      :stream (control-archive-get-entry-stream
                                               entry)
                                      :recurse-into-directory-entries nil))
    (flexi-streams:get-output-stream-sequence out-stream)))

(ftype control-archive-entries deb-package list)
(defun control-archive-entries (package)
  (list
   (make-instance 'archive::tar-entry
                  :pathname "control"
                  :mode (logand archive::+permissions-mask+ 33188)
                  :typeflag (archive::typeflag-for-mode 33188)
                  :uid 0
                  :gid 0
                  :size 154
                  :mtime 1434405316)))

(ftype control-archive-get-entry-stream archive::tar-entry flexi-streams:flexi-stream)
(defun control-archive-get-entry-stream (entry)
  (cond ((string= (archive::entry-pathname entry) "control")
         (flexi-streams:make-in-memory-input-stream
          (string-to-vector "Package: foo
Version: 1.0-1
Architecture: all
Maintainer: Foo Bar <foo@bar.com>
Depends: vim
Section: misc
Priority: optional
Description: foobar baz qux
")))
        (t (flexi-streams:make-in-memory-input-stream
            (string-to-vector "")))))
