(defpackage #:github-webhook/server
  (:use #:cl
        #:lack.request)
  (:import-from #:github-webhook/app
                #:make-app)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:export #:up
           #:down))
(in-package #:github-webhook/server)

(defvar *server*)

(defun up (&key hooks-dir
                secret
                exit-on-failure
                (port 5000))
  (check-type hooks-dir (or string pathname))
  (check-type secret (or null string))
  (when (boundp '*server*)
    (cerror "Restart" "Server is already running")
    (clack:stop *server*))

  (let* ((hooks-dir-path (uiop:directory-exists-p hooks-dir)))
    (unless hooks-dir-path
      (error "Hooks directory '~A' doesn't exist" hooks-dir))
    (let ((app (make-app :hooks-dir hooks-dir-path
                         :secret secret
                         :exit-on-failure exit-on-failure)))
      (setf *server* (clack:clackup app :port port)))))

(defun down ()
  (when (boundp '*server*)
    (clack:stop *server*)
    (makunbound '*server*))
  (values))
