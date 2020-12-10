(defpackage #:github-webhook/signature
  (:use #:cl)
  (:import-from #:ironclad
                #:make-hmac
                #:hmac-digest
                #:update-hmac
                #:byte-array-to-hex-string)
  (:export #:check-signature
           #:invalid-signature))
(in-package #:github-webhook/signature)

(define-condition invalid-signature (error) ())

(defun verify-hub-signature (secret body signature)
  (let ((hmac (make-hmac (byte-array-to-hex-string (hmac-digest secret))
                         :sha256)))
    (update-hmac hmac body)
    (equal (format nil "sha256=~A"
                   (byte-array-to-hex-string (hmac-digest hmac)))
           signature)))

(defun starts-with (prefix value)
  (check-type prefix string)
  (check-type value string)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defun check-signature (secret headers content)
  (check-type secret (vector (unsigned-byte 8)))
  (let ((signature (gethash "x-hub-signature-256" headers)))
    (or (and (stringp signature)
             (starts-with "sha256=" signature)
             (verify-hub-signature secret
                                   content
                                   signature))
        (error 'invalid-signature))))
