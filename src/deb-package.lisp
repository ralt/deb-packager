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
   (size :initarg :size :reader size :type integer)))

(defclass deb-package ()
  ((name :initarg :name
         :type symbol
         :initform (error "Name required."))
   (changelog :initarg :changelog
              :type (vector changelog-entry)
              :reader changelog
              :initform (error "Changelog required."))
   (data-files :type (vector deb-file))
   (architecture :initarg :architecture
                 :type string
                 :reader architecture
                 :initform "all"))
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
               :content (package-copyright)
               :size (length (package-copyright)))
              (make-instance
               'deb-file
               :path (pathname
                      (format nil "usr/share/doc/~A/README.Debian" (name package)))
               :content (package-readme package)
               :size (length (package-readme package)))
              (make-instance
               'deb-file
               :path (pathname
                      (format nil "usr/share/doc/~A/changelog.Debian.gz" (name package)))
               :content (package-changelog package)
               :size (length (package-changelog package))))))))
