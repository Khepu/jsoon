(ql:quickload :jsown)
(ql:quickload :uiop)
(ql:quickload :fiveam)

(defpackage :jsoon
  (:use #:cl #:uiop #:fiveam))

(in-package :jsoon)

(require :sb-simd)

(defparameter *data* (uiop:read-file-string "./10mb.json"))

(defparameter *mock* "   {  \"my-data\":  123}")

(defconstant +chunk-length+ 32)

(defconstant +space+        (sb-simd-avx2:s8.32 (char-code #\space)))
(defconstant +tab+          (sb-simd-avx2:s8.32 (char-code #\tab)))
(defconstant +newline+      (sb-simd-avx2:s8.32 (char-code #\newline)))
(defconstant +double-quote+ (sb-simd-avx2:s8.32 (char-code #\")))
(defconstant +backslash+    (sb-simd-avx2:s8.32 (char-code #\\)))

(deftype string-chunk ()
  `(array (signed-byte 8) (,+chunk-length+)))

(declaim (inline rightmost-bit rightmost-bit-index unset-rightmost-bit chunk %unescape-char))

(defun rightmost-bit (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (logand (1+ (lognot n))
          n))

(defun rightmost-bit-index (n)
  "Returns a 0-based index of the location of the rightmost set bit of `n'."
  (declare (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (truncate (coerce (log (rightmost-bit n) 2)
                    'single-float)))

(defun unset-rightmost-bit (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (the fixnum (logxor n (rightmost-bit n))))

(defun chunk (string index &optional (pad-character #\Nul))
  (declare (type simple-string string)
           (type fixnum index)
           (type character pad-character)
           (optimize (speed 3) (safety 1)))
  (let ((chunk (make-array (list +chunk-length+)
                           :element-type '(signed-byte 8)
                           :initial-element (char-code pad-character))))
    (declare (type string-chunk chunk))
    (loop with upper-limit = (min (+ index +chunk-length+)
                                  (length string))
          for i from index below upper-limit
          for j from 0
          for character = (char-code (char string i))
          do (setf (aref chunk j) character))
    chunk))

(defun skip-whitespace (string index)
  "Skips to the first non-whitespace character."
  (declare (type simple-string string)
           (type fixnum index))
  ;; Padding with a spaces to make detection of chunks with just whitespace
  ;; easier when they are less than `+chunk-length+'.
  (let* ((chunk (chunk string index #\space))
         ;; Packing here instead of inside `chunk' to avoid pointer coercion
         (chunk (sb-simd-avx2:s8.32-aref chunk 0))
         (space-mask   (sb-simd-avx2:s8.32/= chunk +space+))
         (tab-mask     (sb-simd-avx2:s8.32/= chunk +tab+))
         (newline-mask (sb-simd-avx2:s8.32/= chunk +newline+))
         ;; Equality checks with simd will produce unsigned results of the same length
         (whitespace-pack (sb-simd-avx2:u8.32-and space-mask
                                                  tab-mask
                                                  newline-mask))
         ;; The produced mask is backwards
         (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
    (if (zerop whitespace-bitmap)
        (values nil 0)
        (values (+ index (rightmost-bit-index whitespace-bitmap))
                whitespace-bitmap))))

(defun skip-to-next-character (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (loop with current-index = index
        with new-index
        with whitespace-bitmap
        do (multiple-value-setq (new-index whitespace-bitmap)
             (skip-whitespace string current-index))
        unless new-index
          do (incf current-index +chunk-length+)
        until new-index
        finally (return new-index)))

(defun %unescape-char (out string index)
  "Write the unescaped character to `out' and return an index pointing at the
first character after the escaped sequence."
  (declare (type simple-string string)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (let ((escaped-character (char string (1+ index))))
    (case escaped-character
      (#\n (prog1 (incf index 2)
             (write-char #\linefeed out)))
      (#\f (prog1 (incf index 2)
             (write-char #\linefeed out)))
      (#\b (prog1 (incf index 2)
             (write-char #\backspace out)))
      (#\r (prog1 (incf index 2)
             (write-char #\return out)))
      (#\t (prog1 (incf index 2)
             (write-char #\tab out)))
      (#\u (error "Not supported"))
      (t (prog1 (incf index 2)
           (write-char escaped-character out))))))

(defun %unescape-string (parsed-string
                         raw-string-length
                         string
                         current-index
                         double-quote-bitmap
                         next-double-quote
                         backslash-bitmap
                         next-backslash)
  (loop with frozen-index = current-index ;; fixing current index as that was used to generate the initial chunk
        with remaining-string = (- raw-string-length frozen-index)
        with boundary = (min remaining-string (or next-double-quote +chunk-length+))
        while (and next-backslash (< next-backslash boundary))
        for backslash-index = (+ frozen-index next-backslash)
        when (< current-index backslash-index)
          do (write-string string
                           parsed-string
                           :start current-index
                           :end backslash-index)
             ;; move to the next double-quote if we just escaped one
        when (eql 1 (- next-double-quote next-backslash))
          do (setf double-quote-bitmap (unset-rightmost-bit double-quote-bitmap)
                   next-double-quote (unless (zerop double-quote-bitmap)
                                       (rightmost-bit-index backslash-bitmap))
                   boundary (min (or next-double-quote +chunk-length+)
                                 remaining-string))
        do (setf current-index (%unescape-char parsed-string string backslash-index)
                 ;; if we just unescaped a backslash then skip a backslash
                 backslash-bitmap (if (eql #\\ (char string (1- current-index)))
                                      (unset-rightmost-bit (unset-rightmost-bit backslash-bitmap))
                                      (unset-rightmost-bit backslash-bitmap))
                 next-backslash (unless (zerop backslash-bitmap)
                                  (rightmost-bit-index backslash-bitmap)))

        finally (let ((end-of-string (+ frozen-index boundary)))
                  (when (> end-of-string current-index)
                    (write-string string
                                  parsed-string
                                  :start current-index
                                  :end end-of-string))
                  (return (values (max end-of-string current-index)
                                  double-quote-bitmap
                                  next-double-quote)))))

(define-condition unmatched-string-delimiter (error)
  ((starting-position :initarg :starting-position
                      :accessor starting-position))
  (:report (lambda (condition stream)
             (format stream "Could not find end of string starting at ~a!"
                     (starting-position condition)))))

(defun %parse-string (string index)
  (declare (type simple-string string)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (with-output-to-string (parsed-string)
    (loop with current-index = (incf index)
          with raw-string-length = (length string)
          with next-double-quote = nil
          when (>= current-index raw-string-length)
            do (error 'unmatched-string-delimiter :starting-position (1- index))
          while (< current-index ; next-double-quote
                   raw-string-length
                   )
          ;; `next-' prefix essentially means offset from `current-index'
          do (let* ((chunk (chunk string current-index))
                    (chunk (sb-simd-avx2:s8.32-aref chunk 0))
                    (double-quote-mask   (sb-simd-avx2:s8.32= chunk +double-quote+))
                    (double-quote-bitmap (sb-simd-avx2:u8.32-movemask double-quote-mask))
                    (next-double-quote (unless (zerop double-quote-bitmap)
                                         (rightmost-bit-index double-quote-bitmap)))

                    (backslash-mask   (sb-simd-avx2:s8.32= chunk +backslash+))
                    (backslash-bitmap (sb-simd-avx2:u8.32-movemask backslash-mask))
                    (next-backslash (unless (zerop backslash-bitmap)
                                      (rightmost-bit-index backslash-bitmap))))
               (cond
                 ;; the end of the string is known and no escaping is needed
                 ((and next-double-quote
                       (or (not next-backslash)
                           (< next-double-quote next-backslash)))
                  (write-string string
                                parsed-string
                                :start current-index
                                :end (+ current-index next-double-quote))
                  (return))

                 ;; no string delimiter found and nothing to unescape, move forward
                 ((and (not next-double-quote) (not next-backslash))
                  (write-string string
                                parsed-string
                                :start current-index
                                :end (setf current-index (min raw-string-length
                                                              (+ current-index +chunk-length+)))))

                 ((or (and (not next-double-quote) next-backslash)
                      (> next-double-quote next-backslash))
                  (multiple-value-setq (current-index double-quote-bitmap next-double-quote)
                    (%unescape-string parsed-string
                                      raw-string-length
                                      string
                                      current-index
                                      double-quote-bitmap
                                      next-double-quote
                                      backslash-bitmap
                                      next-backslash))))))))

(defun %parse-object (string index)
  (declare (type simple-string string)
           (type fixnum index))
  ;; parse a string
  ;; next char should be ':'
  ;; parse recursively
  ;; do for all attributes
  ;; find matching }
  )

(defun %parse-array (string index)
  ;; recursively parse
  ;; if a ',' follows then repeat
  ;; if a ']' follows then stop
  )

(defun %parse-number (string index)
  ;; parse until the next space
  ;; if a '.' is included then it's a real, otherwise integer
  )

(defun parse (string &optional (index 0))
  (declare (type simple-string string))
  (let* ((index (skip-to-next-character string index))
         (character (char string index)))
    (cond
      ((eql character #\{)          (%parse-object string index))
      ((eql character #\[)          (%parse-array string index))
      ((eql character #\")          (%parse-string string index))
      ((or (digit-char-p character)
           (eql character #\-))    (%parse-number string index))
      (t (error "Failed to parse JSON at position '~a': Unexpected character '~a'"
                index
                character)))))

;;
;; Tests
;;

(5am:def-suite :jsoon-tests)

(5am:test :test-rightmost-bit-index
  (5am:is (eql 0 (rightmost-bit-index #b1)))
  (5am:is (eql 2 (rightmost-bit-index #b100)))
  (5am:is (eql 2 (rightmost-bit-index #b11011100))))

(5am:test :test-skip-whitespace
  (5am:is (equal '(0 #b1)
                 (multiple-value-list (skip-whitespace "a" 0))))
  (5am:is (equal '(nil #b0)
                 (multiple-value-list (skip-whitespace " " 0)))
          "No non-whitespace characters found")
  (5am:is (equal '(2 #b100)
                 (multiple-value-list (skip-whitespace "  a   " 0)))
          "Bitmask is reversed & leading zeroes are ommitted."))

(5am:test :test-%parse-string
  (let ((long-string "this is a very very very very large test"))
    (5am:is (string= (format nil "~a~%" long-string)
                     (%parse-string (format nil "\"~a\\n\"" long-string) 0))
            "Test strings larger than chunk size with escape sequence"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\" #\") 'string)))
    (5am:is (string= "\\\""
                     (%parse-string str 0))
            "Unescape backslash and then double-quote"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\") 'string)))
    (5am:signals unmatched-string-delimiter (%parse-string str 0))))
