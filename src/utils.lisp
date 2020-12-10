(defpackage #:docker-gh-webhook/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:ensure-list)
  (:export #:with-env
           #:hget))
(in-package #:docker-gh-webhook/utils)

(defmacro with-env1 ((key &optional val) &body body)
  (once-only (key val)
    (with-gensyms (before)
      `(let ((,before (uiop:getenv ,key)))
         (unwind-protect
             (progn
               (setf (uiop:getenv ,key) (or ,val ""))
               ,@body)
           (setf (uiop:getenv ,key) (or ,before "")))))))

(defmacro with-env ((&rest bindings) &body body)
  (if (null bindings)
      `(progn ,@body)
      `(with-env1 ,(first bindings)
         (with-env ,(rest bindings)
           ,@body))))

(defun hget (hash keys &optional default)
  (let ((keys (ensure-list keys)))
    (block nil
      (reduce (lambda (val key)
                (when val
                  (multiple-value-bind (retval existsp)
                      (gethash key val)
                    (if existsp
                        retval
                        (return default)))))
              keys
              :initial-value hash))))
