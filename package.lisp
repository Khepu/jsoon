(eval-when (:compile-toplevel :load-toplevel)
  (require :sb-simd)
  (ql:quickload :jsown)
  (ql:quickload :uiop)
  (ql:quickload :fiveam))

(defpackage :jsoon
  (:use #:cl #:uiop #:fiveam))
