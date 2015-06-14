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
     (write-deb-file (make-instance 'deb-package
                              :name ',name
                              :changelog changelog-entries))))

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
         :reader name
         :initform (error "Name required."))
   (changelog :initarg :changelog
              :type (vector changelog-entry)
              :reader changelog
              :initform (error "Changelog required.")))
  (:documentation "Holds all the data required to generate a debian package."))

(ftype write-deb-file pathname deb-package null)
(defun write-deb-file (path package)
  (with-open-file (s path :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (write-bytes (ar-global-header) s)
    (write-bytes (ar-add-entry #p"debian-binary" (debian-binary)) s)))

(ftype write-bytes (vector integer) stream null)
(defun write-bytes (bytes stream)
  (mapcar #'(lambda (byte)
              (write-byte byte stream))
          bytes))

(ftype ar-global-header (vector integer))
(defun ar-global-header ()
  (concatenate '(vector integer) "!<arch>" #(#x0A)))

(ftype ar-add-entry pathname (vector integer) (vector integer))
(defun ar-add-entry (path contents)
  (concatenate '(vector integer)
               (ar-entry-filename path)
               (ar-entry-timestamp)
               (ar-entry-owner)
               (ar-entry-group)
               (ar-entry-file-mode)
               (ar-entry-file-size contents)
               (ar-entry-file-magic)))

(ftype ar-entry-filename pathname (vector integer))
(defun ar-entry-filename (path)
  (ar-fixed-integer-vector 16 (namestring path)))

(ftype integer-to-ascii-bytes integer (vector integer))
(defun integer-to-ascii-bytes (integer)
  (let ((ascii-integer (format nil "~D" integer)))
    (make-array (length ascii-integer)
                :initial-contents ascii-integer
                :element-type 'integer)))

(ftype ar-fixed-integer-vector integer vector
       &key (:element-type symbol) (:initial-element integer)
       (vector integer))
(defun ar-fixed-integer-vector (dimension bytes &key
                                                  (element-type 'integer)
                                                  (initial-element #x20))
  (loop
     :with return = (make-array dimension
                                :element-type element-type
                                :initial-element initial-element)
     :for i from 0 to (- (length bytes) 1)
     :do (setf (aref return i) (elt bytes i))
     :finally (return return)))

(ftype ar-entry-timestamp (vector integer))
(defun ar-entry-timestamp ()
  (ar-fixed-integer-vector
   12
   (integer-to-ascii-bytes
    (local-time:timestamp-to-unix (local-time:now)))))

(ftype ar-entry-owner (vector integer))
(defun ar-entry-owner ()
  ;; root
  #(#x30 #x20 #x20 #x20 #x20 #x20))

(ftype ar-entry-group (vector integer))
(defun ar-entry-group ()
  ;; root
  #(#x30 #x20 #x20 #x20 #x20 #x20))

(ftype ar-entry-file-mode (vector integer))
(defun ar-entry-file-mode ()
  ;; 100644 in octal->ascii-hex
  #(#x31 #x30 #x30 #x36 #x34 #x34 #x20 #x20))

(ftype ar-entry-file-size (vector integer) (vector integer))
(defun ar-entry-file-size (contents)
  )

(ftype ar-entry-file-magic (vector integer))
(defun ar-entry-file-magic ()
  #())

(ftype debian-binary (vector integer))
(defun debian-binary ()
  #(#x32 #x2E #x30))
