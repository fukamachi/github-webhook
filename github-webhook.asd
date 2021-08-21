(defsystem "github-webhook"
  :version "0.1.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Docker container to listen for GitHub webhook events"
  :depends-on ("lack"
               "lack-request"
               "ironclad"
               "yason"
               "babel"
               "nail"
               "alexandria")
  :pathname "src"
  :components
  ((:file "app" :depends-on ("signature" "handler"))
   (:file "handler" :depends-on ("utils"))
   (:file "signature")
   (:file "utils")))

(defsystem "github-webhook/server"
  :depends-on ("github-webhook"
               "clack"
               "hunchentoot")
  :pathname "src"
  :components
  ((:file "server")))
