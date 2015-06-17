(in-package #:deb-package)

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
  (let ((mode (logand archive::+permissions-mask+ 33188))
        (typeflag (archive::typeflag-for-mode 33188))
        (mtime 1434405316))
    (list
     (list
      :entry (make-instance 'archive::tar-entry
                            :pathname "control"
                            :mode mode
                            :typeflag typeflag
                            :uid 0
                            :gid 0
                            :size 154
                            :mtime mtime)
      :stream (package-control-stream package))
     (list
      :entry (make-instance 'archive::tar-entry
                            :pathname "md5sums"
                            :mode mode
                            :typeflag typeflag
                            :uid 0
                            :gid 0
                            :size (length md5sums)
                            :mtime mtime)
      :stream (package-md5sums-stream package)))))

(ftype control-archive-get-entry-stream archive::tar-entry flexi-streams:flexi-stream)
(defun control-archive-get-entry-stream (entry)
  (cond ((string= (archive::entry-pathname entry) "control")
)
        (t (flexi-streams:make-in-memory-input-stream
            (string-to-vector "")))))
