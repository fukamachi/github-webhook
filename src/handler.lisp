(defpackage #:github-webhook/handler
  (:use #:cl
        #:github-webhook/logger)
  (:import-from #:github-webhook/utils
                #:hget
                #:with-env)
  (:export #:make-hooks-handler))
(in-package #:github-webhook/handler)

(defun file-executable-p (file)
  (sb-unix:unix-access (namestring file) sb-unix:x_ok))

;; Available environment variables
;;
;; Common:
;;   GH_HOOK_EVENT_NAME
;;   GH_HOOK_EVENT_PATH
;;   GH_HOOK_ACTION
;;   GH_HOOK_SENDER
;;   GH_HOOK_REPOSITORY
;;   GH_HOOK_REPOSITORY_OWNER
;;   GH_HOOK_REPOSITORY_NAME
;;   GH_HOOK_ORGANIZATION
;;
;; Other:
;;   GH_HOOK_REF
;;   GH_HOOK_BRANCH
;;   GH_HOOK_TAG
;;   GH_HOOK_PACKAGE_NAME
;;   GH_HOOK_PACKAGE_TYPE
;;   GH_HOOK_PACKAGE_VERSION

(defun hook-scripts (hooks-dir event action)
  ;; hooks/*
  ;; hooks/<event>/*
  ;; hooks/<event>/<action>/*
  (let* ((event-dir (merge-pathnames event hooks-dir))
         (action-dir (merge-pathnames action event-dir)))
    (mapcar #'file-executable-p
            (append (uiop:directory-files hooks-dir)
                    (and (uiop:directory-exists-p event-dir)
                         (uiop:directory-files event-dir))
                    (and (uiop:directory-exists-p action-dir)
                         (uiop:directory-files action-dir))))))

(defun extract-ref (value type)
  (when value
    (let ((prefix (format nil "refs/~A/" type)))
      (when (eq (search prefix value) 0)
        (subseq value (length prefix))))))

(defun make-hooks-handler (hooks-dir &key exit-on-failure)
  (check-type hooks-dir pathname)
  (lambda (event action payload content)
    (let ((scripts (hook-scripts hooks-dir event action)))
      (when scripts
        (uiop:with-temporary-file (:stream payload-stream
                                   :pathname payload-path
                                   :direction :io
                                   :external-format :utf-8
                                   :prefix "event"
                                   :type "json")
          (write-string content payload-stream)
          (with-env (("GH_HOOK_EVENT_NAME" event)
                     ("GH_HOOK_EVENT_PATH" (namestring payload-path))
                     ("GH_HOOK_ACTION" action)
                     ("GH_HOOK_SENDER" (hget payload '("sender" "login")))
                     ("GH_HOOK_REPOSITORY" (hget payload '("repository" "full_name")))
                     ("GH_HOOK_REPOSITORY_NAME" (hget payload '("repository" "name")))
                     ("GH_HOOK_REPOSITORY_OWNER" (hget payload '("repository" "owner" "login")))
                     ("GH_HOOK_ORGANIZATION" (hget payload '("organization" "name")))
                     ;; Available only 'push' events
                     ("GH_HOOK_REF" (hget payload '("ref")))
                     ("GH_HOOK_BRANCH" (extract-ref (hget payload '("ref")) "heads"))
                     ("GH_HOOK_TAG" (extract-ref (hget payload '("ref")) "tags"))
                     ;; Available only 'package' events
                     ("GH_HOOK_PACKAGE_NAME" (hget payload '("package" "name")))
                     ("GH_HOOK_PACKAGE_TYPE" (hget payload '("package" "package_type")))
                     ("GH_HOOK_PACKAGE_VERSION" (hget payload '("package" "package_version" "version")))
                     ;; Secret envrionemnt variables
                     ("GH_SECRET_KEY" nil))
            (block exit
              (handler-bind ((error
                               (lambda (e)
                                 (log-warn "Hook '~A' exited with code=~A."
                                           (uiop:subprocess-error-command e)
                                           (uiop:subprocess-error-code e))
                                 (when exit-on-failure
                                   (return-from exit))
                                 (let ((restart (find-restart 'continue e)))
                                   (if restart
                                       (invoke-restart restart)
                                       (abort e))))))
                (dolist (script scripts)
                  (log-info "Running '~A'." script)
                  (uiop:run-program script
                                    :output t
                                    :error-output t)
                  (log-info "Finished '~A'." script))))))))))
