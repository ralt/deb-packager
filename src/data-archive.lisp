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
    (flexi-streams:get-output-stream-sequence out-stream)))

(ftype data-archive-entries deb-package list)
(defun data-archive-entries (package)
  (let ((mode (logand archive::+permissions-mask+ 33188))
        (typeflag (archive::typeflag-for-mode 33188))
        (mtime 1434405316))
    (loop
       :for data-file across (package-data-files package)
       :collect (list
                 :entry (make-instance 'archive::tar-entry
                                       :pathname (path data-file)
                                       :mode mode
                                       :typeflag typeflag
                                       :uid 0
                                       :gid 0
                                       :size (size data-file)
                                       :mtime mtime)
                 :stream (flexi-streams:make-in-memory-input-stream
                          (content data-file))))))
