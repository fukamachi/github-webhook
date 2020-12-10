(in-package #:cl-user)

(defvar *hooks-dir*
  (let* ((hooks-dir
           (uiop:getenv-pathname "GH_HOOKS_DIR"))
         (hooks-dir
           (and hooks-dir
                (uiop:ensure-directory-pathname hooks-dir))))
    hooks-dir))

(defvar *secret-key*
  (uiop:getenv "GH_SECRET"))

(unless *hooks-dir*
  (error "Hooks directory is required but not set: GH_HOOKS_DIR"))

(unless (uiop:directory-exists-p *hooks-dir*)
  (error "Hooks directory '~A' doesn't exist" *hooks-dir*))

(github-webhook:make-app
  :hooks-dir *hooks-dir*
  :secret *secret-key*)
