(in-package #:deb-packager)

(ftype control-archive deb-package (vector (unsigned-byte 8)))
(defun control-archive (package)
  (let* ((out-stream (flexi-streams:make-in-memory-output-stream))
         (archive (archive:open-archive 'archive:tar-archive out-stream
                                        :direction :output)))
    (dolist (entry (control-archive-entries package)
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

(ftype control-archive-entries deb-package list)
(defun control-archive-entries (package)
  (append
   (let ((mode (logand archive::+permissions-mask+ 33188))
         (typeflag (archive::typeflag-for-mode 33188))
         (mtime 1434405316))
     (append
      (multiple-value-bind (md5sums-stream md5sums-length)
          (package-md5sums-stream package)
        (list
         (list
          :entry (make-instance 'archive::tar-entry
                                :pathname "md5sums"
                                :mode mode
                                :typeflag typeflag
                                :uid 0
                                :gid 0
                                :size md5sums-length
                                :mtime mtime)
          :stream md5sums-stream)))
      (multiple-value-bind (control-stream control-length)
          (package-control-stream package)
        (list
         (list
          :entry (make-instance 'archive::tar-entry
                                :pathname "control"
                                :mode mode
                                :typeflag typeflag
                                :uid 0
                                :gid 0
                                :size control-length
                                :mtime mtime)
          :stream control-stream)))
      (loop
         :for control-file across (package-control-files package)
         :collect (list
                   :entry (make-instance 'archive::tar-entry
                                         :pathname (path control-file)
                                         :mode (data-mode (mode control-file))
                                         :typeflag (archive::typeflag-for-mode
                                                    (data-mode
                                                     (mode control-file)))
                                         :uid 0
                                         :gid 0
                                         :size (size control-file)
                                         :mtime 1434405316)
                   :stream (flexi-streams:make-in-memory-input-stream
                            (content control-file))))))))
