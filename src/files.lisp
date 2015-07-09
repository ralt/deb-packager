(in-package #:deb-packager)

(djula:add-template-directory (asdf:system-relative-pathname "deb-packager"
                                                             "templates/"))

(ftype debian-binary (vector (unsigned-byte 8)))
(defun debian-binary ()
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x32 #x2E #x30 #x0A)))

(defparameter +control-template+ (djula:compile-template* "control"))

(ftype package-control-stream deb-package flexi-streams:in-memory-input-stream)
(defun package-control-stream (package)
  (flexi-streams:make-in-memory-input-stream
   (string-to-vector
    (djula:render-template*
     +control-template+ nil
     :name (name package)
     :version (package-version package)
     :architecture (architecture package)
     :maintainer (maintainer package)
     :depends (format nil "~{~A~^ ~}" (depends package))
     :description (description package)
     :long-description (package-long-description package)))))

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

(defparameter +copyright-template+ (djula:compile-template* "copyright"))

(ftype package-copyright (vector (unsigned-byte 8)))
(defun package-copyright ()
  (string-to-vector (djula:render-template* +copyright-template+)))

(defparameter +readme-template+ (djula:compile-template* "README.Debian"))

(ftype package-readme deb-package (vector (unsigned-byte 8)))
(defun package-readme (package)
  (string-to-vector (djula:render-template* +readme-template+ nil
                                            :name (name package))))

(defparameter +changelog-template+ (djula:compile-template* "changelog.Debian"))

(ftype package-changelog deb-package (vector (unsigned-byte 8)))
(defun package-changelog (package)
  (salza2:compress-data
   (string-to-vector
    (djula:render-template*
     +changelog-template+ nil
     :name (name package)
     :entries (reverse
               (loop
                  :for entry across (changelog package)
                  :collect (list
                            :version (version entry)
                            :message (format-changelog-message (message entry))
                            :author (author entry)
                            :date (local-time:format-rfc1123-timestring
                                   nil
                                   (local-time:unix-to-timestamp (date entry))))))))
   'salza2:gzip-compressor))

(ftype format-changelog-message string string)
(defun format-changelog-message (message)
  (format nil "  * ~A" message))

(ftype package-control-files deb-package (vector deb-file))
(defun package-control-files (package)
  (slot-value package 'control-files))

(ftype package-data-files deb-package (vector deb-file))
(defun package-data-files (package)
  (slot-value package 'data-files))

(ftype initialize-control-files deb-package (vector deb-file) null)
(defun initialize-control-files (package files)
  (setf (slot-value package 'control-files)
        (make-array
         (length files)
         :initial-contents files))
  nil)

(ftype initialize-data-files deb-package (vector deb-file) null)
(defun initialize-data-files (package files)
  (setf (slot-value package 'data-files)
        (make-array
         (+ 3 (length files))
         :initial-contents
         (concatenate
          '(vector deb-file)
          (list (make-instance
                 'deb-file
                 :path (pathname
                        (format nil "usr/share/doc/~A/copyright" (name package)))
                 :content (package-copyright)
                 :size (length (package-copyright))
                 :mode 644)
                (make-instance
                 'deb-file
                 :path (pathname
                        (format nil "usr/share/doc/~A/README.Debian" (name package)))
                 :content (package-readme package)
                 :size (length (package-readme package))
                 :mode 644)
                (make-instance
                 'deb-file
                 :path (pathname
                        (format nil "usr/share/doc/~A/changelog.Debian.gz" (name package)))
                 :content (package-changelog package)
                 :size (length (package-changelog package))
                 :mode 644))
          files)))
  nil)
