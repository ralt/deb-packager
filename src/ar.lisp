(in-package #:deb-packager)

(ftype string-to-vector string (vector (unsigned-byte 8)))
(defun string-to-vector (string)
  (loop
     :with vector = (make-array (length string) :element-type '(unsigned-byte 8))
     :for i from 0 to (- (length string) 1)
     :do (setf (aref vector i) (char-code (elt string i)))
     :finally (return vector)))

(ftype ar-global-header (vector (unsigned-byte 8)))
(defun ar-global-header ()
  (concatenate '(vector (unsigned-byte 8)) (string-to-vector "!<arch>") #(#x0A)))

(ftype ar-add-entry pathname (vector (unsigned-byte 8)) (vector (unsigned-byte 8)))
(defun ar-add-entry (path contents)
  (concatenate '(vector (unsigned-byte 8))
               (ar-entry-filename path)
               (ar-entry-timestamp)
               (ar-entry-owner)
               (ar-entry-group)
               (ar-entry-file-mode)
               (ar-entry-file-size contents)
               (ar-entry-file-magic)
               contents))

(ftype ar-entry-filename pathname (vector (unsigned-byte 8)))
(defun ar-entry-filename (path)
  (ar-fixed-unsigned-byte-vector 16 (string-to-vector (namestring path))))

(ftype integer-to-ascii-bytes integer (vector (unsigned-byte 8)))
(defun integer-to-ascii-bytes (integer)
  (string-to-vector (format nil "~D" integer)))

(ftype ar-fixed-unsigned-byte-vector unsigned-byte vector
       &key (:element-type symbol) (:initial-element integer)
       (vector (unsigned-byte 8)))
(defun ar-fixed-unsigned-byte-vector (dimension bytes &key
                                                  (element-type '(unsigned-byte 8))
                                                  (initial-element #x20))
  (loop
     :with return = (make-array dimension
                                :element-type element-type
                                :initial-element initial-element)
     :for i from 0 to (- (length bytes) 1)
     :do (setf (aref return i) (elt bytes i))
     :finally (return return)))

(ftype ar-entry-timestamp (vector (unsigned-byte 8)))
(defun ar-entry-timestamp ()
  (ar-fixed-unsigned-byte-vector
   12
   (integer-to-ascii-bytes
    (local-time:timestamp-to-unix (local-time:now)))))

(ftype ar-entry-owner (vector (unsigned-byte 8)))
(defun ar-entry-owner ()
  ;; root
  (make-array 6
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x30 #x20 #x20 #x20 #x20 #x20)))

(ftype ar-entry-group (vector (unsigned-byte 8)))
(defun ar-entry-group ()
  ;; root
  (make-array 6
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x30 #x20 #x20 #x20 #x20 #x20)))

(ftype ar-entry-file-mode (vector (unsigned-byte 8)))
(defun ar-entry-file-mode ()
  ;; 100644 in octal->ascii-hex
  (make-array 8
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x31 #x30 #x30 #x36 #x34 #x34 #x20 #x20)))

(ftype ar-entry-file-size (vector (unsigned-byte 8)) (vector (unsigned-byte 8)))
(defun ar-entry-file-size (contents)
  (ar-fixed-unsigned-byte-vector 10 (integer-to-ascii-bytes (length contents))))

(ftype ar-entry-file-magic (vector (unsigned-byte 8)))
(defun ar-entry-file-magic ()
  (make-array 2
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x60 #x0A)))
