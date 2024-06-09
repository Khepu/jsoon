(asdf:defsystem #:jsoon
  :description "SIMD-based JSON parser"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "abm-vops")
               (:file "inst")
               (:file "jsoon")))
