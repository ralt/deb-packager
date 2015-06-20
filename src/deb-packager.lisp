(in-package #:deb-packager)

(ftype get-item list symbol list)
(defun get-item (list keyword)
  (rest
   (find-if #'(lambda (item)
                (getf item keyword))
            list)))

(defmacro define-deb-package (name &body forms)
  `(let ((changelog-entries
          (make-array
           ,(length (car (get-item forms :changelog)))
           :initial-contents (list ,@(mapcar
                                      #'(lambda (entry)
                                          `(make-instance 'changelog-entry ,@entry))
                                      (car (get-item forms :changelog)))))))
     (let ((package (make-instance 'deb-package
                                   :name ',name
                                   :changelog changelog-entries
                                   :description ,@(get-item forms :description)
                                   :architecture ,@(get-item forms :architecture)
                                   :depends ',@(if (get-item forms :depends)
                                                   (get-item forms :depends)
                                                   '(nil))
                                   :maintainer ,@(get-item forms :maintainer))))
       (write-deb-file (package-pathname package) package))))

(ftype write-deb-file pathname deb-package null)
(defun write-deb-file (path package)
  (with-open-file (s path :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (write-bytes (ar-global-header) s)
    (write-bytes (ar-add-entry #p"debian-binary" (debian-binary)) s)
    (write-bytes (ar-add-entry #p"control.tar" (control-archive package)) s)
    (write-bytes (ar-add-entry #p"data.tar" (data-archive package)) s)))

(ftype write-bytes (vector (unsigned-byte 8)) stream null)
(defun write-bytes (bytes stream)
  (loop
     :for byte across bytes
     :do (write-byte byte stream)))
