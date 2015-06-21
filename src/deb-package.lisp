(in-package #:deb-packager)

(defclass changelog-entry ()
  ((version :initarg :version
            :type string
            :reader version
            :initform (error "Version required."))
   (author :initarg :author
           :type string
           :reader author
           :initform (error "Author required."))
   (message :initarg :message
            :type string
            :reader message
            :initform (error "Message required."))
   (date :initarg :date
         :type integer
         :reader date
         :initform (error "Date required.")))
  (:documentation "A single changelog entry."))

(defclass deb-file ()
  ((path :initarg :path :reader path :type pathname)
   (content :initarg :content :reader content :type (vector (unsigned-byte 8)))
   (size :initarg :size :reader size :type integer)
   (mode :initarg :mode :reader mode :type integer)))

(defclass deb-package ()
  ((name :initarg :name
         :type symbol
         :initform (error "Name required."))
   (description :initarg :description
                :type string
                :reader description
                :initform (error "Description required."))
   (long-description :initarg :long-description
                     :type string)
   (changelog :initarg :changelog
              :type (vector changelog-entry)
              :reader changelog
              :initform (error "Changelog required."))
   (data-files :type (vector deb-file))
   (architecture :initarg :architecture
                 :type string
                 :reader architecture
                 :initform "all")
   (maintainer :initarg :maintainer
               :type string
               :reader maintainer
               :initform (error "Maintainer required."))
   (depends :initarg :depends
            :type list
            :reader depends))
  (:documentation "Holds all the data required to generate a debian package."))

(ftype name deb-package string)
(defun name (package)
  "Gets the name of a package."
  (string-downcase (symbol-name (slot-value package 'name))))

(ftype package-pathname deb-package pathname)
(defun package-pathname (package)
  "Gets the pathname of a package."
  (pathname (concatenate 'string
                         (name package)
                         "_"
                         (package-version package)
                         "_"
                         (architecture package)
                         ".deb")))

(ftype package-version deb-package string)
(defun package-version (package)
  (version (elt (changelog package) (- (length (changelog package)) 1))))

(ftype package-long-description deb-package string)
(defun package-long-description (package)
  "Returns a formatted long description for the package."
  (format nil "~{ ~A~%~}" (cl-ppcre:split "\\n"
                                          (slot-value package 'long-description))))
