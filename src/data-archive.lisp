(in-package #:deb-packager)

(ftype data-archive deb-package (vector (unsigned-byte 8)))
(defun data-archive (package)
  (let* ((out-stream (flexi-streams:make-in-memory-output-stream))
         (archive (archive:open-archive 'archive:tar-archive out-stream
                                        :direction :output)))
    (dolist (entry (data-archive-entries package)
             (archive:finalize-archive archive))
      (archive:write-entry-to-archive archive (getf entry :entry)
                                      :stream (getf entry :stream)
                                      :recurse-into-directory-entries nil))
    (let ((bits-vector (flexi-streams:get-output-stream-sequence out-stream)))
      ;; salza2 wants a simple-array, not a vector
      (salza2:compress-data (make-array
                             (length bits-vector)
                             :element-type '(unsigned-byte 8)
                             :initial-contents bits-vector)
                            'salza2:gzip-compressor))))

(ftype data-archive-entries deb-package list)
(defun data-archive-entries (package)
  (let ((mtime 1434405316)
        (folders))
    (loop
       :for data-file across (package-data-files package)
       :when (not (member (directory-namestring (path data-file))
                          folders
                          :test #'string=))
       :collect (list
                 :entry (make-instance 'archive::tar-entry
                                       :pathname (pathname
                                                  (directory-namestring
                                                   (path data-file)))
                                       :mode 16877
                                       :typeflag (archive::typeflag-for-mode
                                                  16877)
                                       :uid 0
                                       :gid 0
                                       :size 0
                                       :mtime mtime)
                 :stream (flexi-streams:make-in-memory-input-stream
                          (make-array 0 :element-type '(unsigned-byte 8))))
       :when (not (member (directory-namestring (path data-file))
                          folders
                          :test #'string=))
       :do (push (directory-namestring (path data-file)) folders)
       :collect (list
                 :entry (make-instance 'archive::tar-entry
                                       :pathname (path data-file)
                                       :mode (data-mode (mode data-file))
                                       :typeflag (archive::typeflag-for-mode
                                                  (data-mode (mode data-file)))
                                       :uid 0
                                       :gid 0
                                       :size (size data-file)
                                       :mtime mtime)
                 :stream (flexi-streams:make-in-memory-input-stream
                          (content data-file))))))

(ftype data-mode integer &optional boolean integer)
(defun data-mode (incomplete-octal-mode &optional (directory-p nil))
  "Some juggling to get an incomplete octal mode,
e.g. 644, to a string, add the 100 for 'file', still
in octal, so we have e.g. '100644', then transform
this string in a decimal integer, e.g. 33188."
  (multiple-value-bind (integer-mode)
      (parse-integer
       (concatenate 'string (if directory-p "000" "100") (write-to-string incomplete-octal-mode))
       :radix 8)
    integer-mode))
