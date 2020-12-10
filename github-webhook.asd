(defsystem "github-webhook"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Docker container to listen for GitHub webhook events"
  :depends-on ("clack"
               "hunchentoot"
               "lack"
               "lack-request"
               "ironclad"
               "yason"
               "babel"
               "alexandria")
  :pathname "src"
  :components
  ((:file "server" :depends-on ("app"))
   (:file "app" :depends-on ("signature" "handler"))
   (:file "handler" :depends-on ("logger" "utils"))
   (:file "logger")
   (:file "signature")
   (:file "utils")))
