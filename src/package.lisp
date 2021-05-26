;;;; package.lisp

(defpackage #:hyperborean-markdown
  (:use #:cl)
  (:nicknames #:hm)
  (:export #:parse-to-md
           #:with-markdown
           #:with-markdown-to-string
           #:with-markdown-to-file

           #:def-syntax
           #:def-special
           #:def-inline-syntax

           #:initiate-environment
           #:initiate-spacing

           ))
