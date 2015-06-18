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
           ,(length (get-item forms :changelog))
           :initial-contents (list ,@(mapcar
                                      #'(lambda (entry)
                                          `(make-instance 'changelog-entry ,@entry))
                                      (get-item forms :changelog))))))
     (let ((package (make-instance 'deb-package
                                   :name ',name
                                   :changelog changelog-entries)))
       (write-deb-file (package-pathname package) package))))
