(defpackage #:github-webhook
  (:nicknames #:github-webhook/app)
  (:use #:cl
        #:github-webhook/signature
        #:lack.request)
  (:import-from #:github-webhook/handler
                #:make-hooks-handler)
  (:import-from #:babel
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:yason
                #:parse)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:import-from #:nail)
  (:export #:make-app))
(in-package #:github-webhook)

(define-condition invalid-request (error) ())

(defun check-request (req)
  (let* ((headers (request-headers req))
         (content-type (getf (request-env req) :content-type))
         (event (gethash "x-github-event" headers)))
    (unless (and (eql (search "application/json" content-type) 0)
                 event)
      (error 'invalid-request))))

(defun main (handler event action payload content)
  (check-type handler function)
  (check-type event string)
  (check-type action (or string null))
  (check-type payload hash-table)
  (check-type content string)
  (funcall handler
           event
           action
           payload
           content))

(defun make-app (&key hooks-dir secret exit-on-failure)
  (check-type hooks-dir pathname)
  (check-type secret (or null string))
  (let ((secret (and secret
                     (string-to-octets secret)))
        (handler (make-hooks-handler hooks-dir
                                     :exit-on-failure exit-on-failure))
        (logger (make-instance 'nail:logger)))
    (lambda (env)
      (nail:with-logger logger
        (block app
          (let ((req (make-request env)))
            ;; Accept only POST requests
            (unless (eq (request-method req) :post)
              (return-from app '(404 () ("Not Found"))))

            (handler-case
                (let ((headers (request-headers req))
                      (content (request-content req)))
                  (when secret
                    (check-signature secret headers content))
                  (check-request req)
                  (let* ((event (gethash "x-github-event" (request-headers req)))
                         (content-json (octets-to-string content))
                         (payload (handler-case
                                      (yason:parse content-json)
                                    (error () (error 'invalid-request))))
                         (action (gethash "action" payload)))
                    (make-thread
                      (lambda ()
                        (main handler event action payload content-json))
                      :initial-bindings `((*standard-output* . ,*standard-output*)
                                          (*error-output* . ,*error-output*))))
                  '(200 () ("OK")))
              (invalid-signature ()
                '(403 () ("Invalid signature")))
              (invalid-request ()
                '(400 () ("Invalid request"))))))))))
