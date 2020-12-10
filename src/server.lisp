(defpackage #:docker-gh-webhook/server
  (:use #:cl
        #:lack.request)
  (:import-from #:docker-gh-webhook/app
                #:make-app)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:export #:up
           #:down))
(in-package #:docker-gh-webhook/server)

(defvar *server*)

(defun up (&key hooks-dir
                secret
                exit-on-failure
                port)
  (check-type hooks-dir string)
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
    (clack:stop *server*)))
