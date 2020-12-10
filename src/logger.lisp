(defpackage #:github-webhook/logger
  (:use #:cl)
  (:export #:log-info
           #:log-warn
           #:log-error))
(in-package #:github-webhook/logger)

(defun logger (stream level message args)
  (format stream "~&[~A] ~A~%"
          level
          (apply #'format nil message args)))

(defun log-info (message &rest args)
  (logger t :info message args))

(defun log-warn (message &rest args)
  (logger *error-output* :warn message args))

(defun log-error (message &rest args)
  (logger *error-output* :error message args))
