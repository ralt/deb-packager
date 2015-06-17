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
              :initform (error "Changelog required."))
   (data-files :type (vector deb-file)))
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

(ftype package-control-stream deb-package flexi-streams:in-memory-input-stream)
(defun package-control-stream (package)
  (flexi-streams:make-in-memory-input-stream
   (string-to-vector (format nil "Package: ~A
Version: 1.0-1
Architecture: all
Maintainer: Foo Bar <foo@bar.com>
Depends: vim
Section: misc
Priority: optional
Description: foobar baz qux
" (name package)))))

(ftype package-md5sums-stream deb-package (values stream integer))
(defun package-md5sums-stream (package)
  (let ((md5sums-vector
         (string-to-vector
          (format
           nil
           "~{~A~%~}"
           (loop
              :for data-file across (package-data-files package)
              :collect (concatenate
                        'string
                        (ironclad:byte-array-to-hex-string
                         (ironclad:digest-sequence 'ironclad:md5 (content data-file)))
                        " "
                        (namestring (path data-file))))))))
    (values
     (flexi-streams:make-in-memory-input-stream md5sums-vector)
     (length md5sums-vector))))

(defclass deb-file ()
  ((path :initarg :path :reader path :type pathname)
   (content :initarg :content :reader content :type (vector (unsigned-byte 8)))))

(ftype package-data-files deb-package (vector deb-file))
(defun package-data-files (package)
  (if (slot-boundp package 'data-files)
      (slot-value package 'data-files)
      (setf (slot-value package 'data-files)
            (make-array
             3
             :initial-contents
             (list
              (make-instance
               'deb-file
               :path (pathname
                      (format nil "usr/share/doc/~A/copyright" (name package)))
               :content (package-copyright))
              (make-instance
               'deb-file
               :path (pathname
                      (format nil "usr/share/doc/~A/README.Debian" (name package)))
               :content (package-readme))
              (make-instance
               'deb-file
               :path (pathname
                      (format nil "usr/share/doc/~A/changelog.Debian.gz" (name package)))
               :content (package-changelog package)))))))

(ftype package-copyright (vector (unsigned-byte 8)))
(defun package-copyright ()
  (string-to-vector "Copyright file."))

(ftype package-readme (vector (unsigned-byte 8)))
(defun package-readme ()
  (string-to-vector "Readme file."))

(ftype package-changelog deb-package (vector (unsigned-byte 8)))
(defun package-changelog (package)
  (salza2:compress-data
   (string-to-vector (format nil "Changelog file for ~A." (name package)))
   'salza2:gzip-compressor))
