(in-package #:deb-package)

(ftype string-to-vector string (vector integer))
(defun string-to-vector (string)
  (loop
     :with vector = (make-array (length string) :element-type 'integer)
     :for i from 0 to (- (length string) 1)
     :do (setf (aref vector i) (char-code (elt string i)))
     :finally (return vector)))

(ftype ar-global-header (vector integer))
(defun ar-global-header ()
  (concatenate '(vector integer) (string-to-vector "!<arch>") #(#x0A)))

(ftype ar-add-entry pathname (vector integer) (vector integer))
(defun ar-add-entry (path contents)
  (concatenate '(vector integer)
               (ar-entry-filename path)
               (ar-entry-timestamp)
               (ar-entry-owner)
               (ar-entry-group)
               (ar-entry-file-mode)
               (ar-entry-file-size contents)
               (ar-entry-file-magic)
               contents))

(ftype ar-entry-filename pathname (vector integer))
(defun ar-entry-filename (path)
  (ar-fixed-integer-vector 16 (string-to-vector (namestring path))))

(ftype integer-to-ascii-bytes integer (vector integer))
(defun integer-to-ascii-bytes (integer)
  (string-to-vector (format nil "~D" integer)))

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
  (ar-fixed-integer-vector 10 (integer-to-ascii-bytes (length contents))))

(ftype ar-entry-file-magic (vector integer))
(defun ar-entry-file-magic ()
  #(#x60 #x0A))
