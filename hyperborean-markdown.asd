;;;; hyperborean-markdown.asd

(asdf:defsystem #:hyperborean-markdown
  :description "A simple lisp -> markdown converter."
  :author "K1D77A"
  :license  "GPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "hyperborean-markdown")))))
