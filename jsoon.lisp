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

(deftype bitmap ()
  `(unsigned-byte ,+chunk-length+))

(deftype bitmap-index ()
  `(or nil
       (unsigned-byte ,(truncate (log +chunk-length+ 2)))))

(define-condition unmatched-string-delimiter (error)
  ((starting-position :initarg :starting-position
                      :accessor starting-position))
  (:report (lambda (condition stream)
             (format stream "Could not find end of string starting at ~a!"
                     (starting-position condition)))))

(define-condition unexpected-character (error)
  ((index :initarg :index
          :accessor index)
   (value :initarg :value
          :accessor value))
  (:report (lambda (condition stream)
             (format stream "Unexpected character '~a' at position ~a!"
                     (value condition)
                     (index condition)))))

(declaim (inline rightmost-bit rightmost-bit-index unset-rightmost-bit chunk %unescape-char next-offset high-surrogate-p %parse-surrogate surrogate-char))

(defun rightmost-bit (n)
  (declare (type bitmap n)
           (dynamic-extent n)
           (optimize (speed 3) (safety 1)))
  (logand (1+ (lognot n))
          n))

(defun rightmost-bit-index (n)
  "Returns a 0-based index of the location of the rightmost set bit of `n'."
  (declare (type fixnum n)
           (optimize (speed 3) (safety 1))
           (dynamic-extent n))
  (truncate (the single-float (log (rightmost-bit n) 2))))

(defun unset-rightmost-bit (n)
  (declare (type bitmap n)
           (optimize (speed 3) (safety 1))
           (dynamic-extent n))
  (logxor n (rightmost-bit n)))

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

(defun high-surrogate-p (code-value)
  "Character numbers between U+D800 and U+DFFF (inclusive) are reserved for use
with the UTF-16 encoding form (as surrogate pairs) and do not directly represent
characters. (Stolen from https://github.com/madnificent/jsown/blob/master/reader.lisp#L82C1-L87C55)"
  (declare (optimize (speed 3) (safety 1)))
  (<= #xD800 code-value #xDFFF))

(defun surrogate-char (high-surrogate low-surrogate)
  (declare (type fixnum high-surrogate low-surrogate)
           (optimize (speed 3) (safety 1)))
  (code-char (+ #x10000
                (- low-surrogate #xDC00)
                (ash (- high-surrogate #xD800) 10))))

(defun %parse-surrogate (string index)
  (declare (type simple-string string)
           (type fixnum index)
           (dynamic-extent string index))
  (parse-integer string
                 :start index
                 :end (+ index 4)
                 :radix 16))

(defun %unescape-char (out string index)
  "Write the unescaped character to `out' and return an `index' pointing at the
first character after the escaped sequence."
  (declare (type simple-string string)
           (type string-stream out)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (incf index)
  (let ((escaped-character (char string index))
        (skip-next-backslash-p nil))
    (values (case escaped-character
              (#\n (prog1 (incf index)
                     (write-char #\linefeed out)))
              (#\f (prog1 (incf index)
                     (write-char #\linefeed out)))
              (#\b (prog1 (incf index)
                     (write-char #\backspace out)))
              (#\r (prog1 (incf index)
                     (write-char #\return out)))
              (#\t (prog1 (incf index)
                     (write-char #\tab out)))
              (#\u (let ((surrogate (%parse-surrogate string (1+ index)))) ;; skip 'u'
                     (if (high-surrogate-p surrogate)
                         (let ((low-surrogate (%parse-surrogate string (+ index 7)))) ;; 4 digits + backslash + 'u' + 1 to skip 'u'
                           (write-char (surrogate-char surrogate low-surrogate) out)
                           (setf skip-next-backslash-p t)
                           (incf index 11))
                         (prog1 (incf index 5)
                           (write-char (code-char surrogate) out)))))
              (#\\ (prog1 (incf index)
                     (setf skip-next-backslash-p t)
                     (write-char escaped-character out)))
              (t (prog1 (incf index)
                   (write-char escaped-character out))))
            skip-next-backslash-p)))

(defun next-offset (bitmap)
  "Returns the rightmost bit index (0-based) if `bitmap' > 0, otherwise NIL"
  (declare (type bitmap bitmap)
           (optimize (speed 3) (safety 1))
           (dynamic-extent bitmap))
  (unless (zerop bitmap)
    (rightmost-bit-index bitmap)))

(defun %unescape-string (parsed-string
                         raw-string-length
                         string
                         current-index
                         double-quote-bitmap
                         next-double-quote
                         backslash-bitmap
                         next-backslash)
  (declare (type simple-string string)
           (type string-stream parsed-string)
           (type fixnum raw-string-length current-index)
           (dynamic-extent string parsed-string
                           backslash-bitmap next-backslash)
           (optimize (speed 3) (safety 1)))
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
        when (and next-double-quote
                  (eql 1 (- next-double-quote next-backslash)))
          do (setf double-quote-bitmap (unset-rightmost-bit double-quote-bitmap)
                   next-double-quote (next-offset double-quote-bitmap)
                   boundary (min (or next-double-quote +chunk-length+)
                                 remaining-string))
        do (multiple-value-bind (new-index skip-next-backslash-p)
               (%unescape-char parsed-string string backslash-index)
             (setf current-index new-index
                   backslash-bitmap (if skip-next-backslash-p
                                        (unset-rightmost-bit (unset-rightmost-bit backslash-bitmap))
                                        (unset-rightmost-bit backslash-bitmap))
                   next-backslash (next-offset backslash-bitmap)))

        finally (return (values current-index
                                double-quote-bitmap
                                next-double-quote))))

(defun %parse-string (string index)
  (declare (type simple-string string)
           (type fixnum index)
           (optimize (speed 3) (safety 1))
           (dynamic-extent string))
  (with-output-to-string (parsed-string)
    (loop with current-index = (incf index)
          with raw-string-length = (length string)
          when (>= current-index raw-string-length)
            do (error 'unmatched-string-delimiter :starting-position (1- index))
          while (< current-index raw-string-length)
          ;; `next-' prefix essentially means offset from `current-index'
          do (let* ((chunk (chunk string current-index))
                    (chunk (sb-simd-avx2:s8.32-aref chunk 0))
                    (double-quote-mask   (sb-simd-avx2:s8.32= chunk +double-quote+))
                    (double-quote-bitmap (sb-simd-avx2:u8.32-movemask double-quote-mask))
                    (next-double-quote (next-offset double-quote-bitmap))

                    (backslash-mask   (sb-simd-avx2:s8.32= chunk +backslash+))
                    (backslash-bitmap (sb-simd-avx2:u8.32-movemask backslash-mask))
                    (next-backslash (next-offset backslash-bitmap)))
               (declare (type bitmap double-quote-bitmap backslash-bitmap)
                        (dynamic-extent double-quote-bitmap next-double-quote
                                        backslash-bitmap next-backslash))
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

(defun next-whitespace (string index)
  (declare (type simple-string string)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (loop with string-length = (length string)
        for current-index from index by +chunk-length+
        while (< current-index string-length)
        do (let* ((chunk (chunk string index))
                  (chunk (sb-simd-avx2:s8.32-aref chunk 0))
                  (space-mask   (sb-simd-avx2:s8.32= chunk +space+))
                  (tab-mask     (sb-simd-avx2:s8.32= chunk +tab+))
                  (newline-mask (sb-simd-avx2:s8.32= chunk +newline+))
                  (whitespace-pack (sb-simd-avx2:u8.32-or space-mask
                                                          tab-mask
                                                          newline-mask))
                  (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
             (unless (zerop whitespace-bitmap)
               (return (+ current-index (rightmost-bit-index whitespace-bitmap)))))
        finally (when (>= current-index string-length)
                  string-length)))

(defun %parse-number (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (let ((end-of-number (next-whitespace string index))
        (*read-eval* nil))
    ;; HACK: A bit cheesy but it will do for now
    (read-from-string string
                      t
                      nil
                      :start index
                      :end end-of-number)))

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
      (t (error 'unexpected-character :index index
                                      :value character )))))

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
  (let ((empty-string (coerce #(#\" #\") 'string)))
    (5am:is (string= ""
                     (%parse-string empty-string 0))
            "Parse empty string"))
  (let ((long-string "this is a very very very very large test"))
    (5am:is (string= (format nil "~a~%" long-string)
                     (%parse-string (format nil "\"~a\\n\"" long-string) 0))
            "Test strings larger than chunk size with escape sequence"))
  (let ((long-string "this is a very very very very large test"))
    (5am:is (string= (format nil "~a~%123" long-string)
                     (%parse-string (format nil "\"~a\\n123\"" long-string) 0))
            "Test strings larger than chunk size with escape sequence followed by normal characters"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\" #\") 'string)))
    (5am:is (string= "\\\""
                     (%parse-string str 0))
            "Unescape backslash and then double-quote"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\") 'string)))
    (5am:signals unmatched-string-delimiter (%parse-string str 0)))
  (5am:is (string= (coerce #(#\SYMBOL_FOR_START_OF_HEADING) 'string)
                   (%parse-string "\"\\uD800\\u0001\"" 0))
          "UTF-16 character 2-bytes")
  (5am:is (string= (coerce #(#\HANGUL_SYLLABLE_HIB) 'string)
                   (%parse-string "\"\\uD799\"" 0))
          "UTF-16 character 1-byte"))
