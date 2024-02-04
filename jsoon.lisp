(ql:quickload :jsown)
(ql:quickload :uiop)
(ql:quickload :fiveam)

(defpackage :jsoon
  (:use #:cl #:uiop #:fiveam))

(in-package :jsoon)

(require :sb-simd)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

(defparameter *data* (uiop:read-file-string "./10mb.json"))

(defparameter *mock* "   {  \"my-data\":  123}")

(defconstant +chunk-length+ (the fixnum 32))

(defparameter +space+        (sb-simd-avx2:u8.32 (char-code #\space)))
(defparameter +tab+          (sb-simd-avx2:u8.32 (char-code #\tab)))
(defparameter +newline+      (sb-simd-avx2:u8.32 (char-code #\newline)))
(defparameter +double-quote+ (sb-simd-avx2:u8.32 (char-code #\")))
(defparameter +backslash+    (sb-simd-avx2:u8.32 (char-code #\\)))
(defparameter +comma+        (sb-simd-avx2:u8.32 (char-code #\,)))
(defparameter +closing-bracket+ (sb-simd-avx2:u8.32 (char-code #\])))
(defparameter +closing-brace+   (sb-simd-avx2:u8.32 (char-code #\})))

(deftype string-chunk ()
  `(array (unsigned-byte 8) (,+chunk-length+)))

(deftype bitmap ()
  `(unsigned-byte ,+chunk-length+))

(deftype bitmap-index ()
  `(or null
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

(declaim (inline rightmost-bit rightmost-bit-index unset-rightmost-bit chunk %unescape-char next-offset high-surrogate-p %parse-surrogate surrogate-char skip-to-next-character))


(defmacro chunk= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.32= (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,chunk)
                                          (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack-256 (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro chunk/= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.32/= (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,chunk)
                                           (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack-256 (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro pack (chunk)
  `(the (sb-ext:simd-pack-256 (unsigned-byte 8))
        (sb-simd-avx2:u8.32-aref ,chunk 0)))

(defun rightmost-bit (n)
  (declare (type bitmap n))
  (logand (1+ (lognot n))
          n))

(defun rightmost-bit-index (n)
  "Returns a 0-based index of the location of the rightmost set bit of `n'."
  (declare (type fixnum n))
  (the fixnum (truncate (log (rightmost-bit n) 2))))

(defun unset-rightmost-bit (n)
  (declare (type bitmap n))
  (logxor n (rightmost-bit n)))

(defun chunk (string index &optional (pad-character #\Nul))
  (declare (type simple-string string)
           (type fixnum index)
           (type character pad-character)
           (optimize (speed 3) (safety 0)))
  (let ((chunk (make-array (list +chunk-length+)
                           :element-type '(unsigned-byte 8)
                           :initial-element (char-code pad-character))))
    (declare (type string-chunk chunk))
    (loop with upper-limit = (min (+ index +chunk-length+)
                                  (length string))
          for i from index below upper-limit
          for j from 0
          for character = (char-code (char string i))
          do (setf (aref chunk j) character))
    chunk))

(defun next-offset (bitmap)
  "Returns the rightmost bit index (0-based) if `bitmap' > 0, otherwise NIL"
  (declare (type bitmap bitmap))
  (unless (zerop bitmap)
    (rightmost-bit-index bitmap)))

(defun skip-whitespace (string index)
  "Skips to the first non-whitespace character."
  (declare (type simple-string string)
           (type fixnum index))
  ;; Padding with a spaces to make detection of chunks with just whitespace
  ;; easier when they are less than `+chunk-length+'.
  (let* ((chunk (chunk string index #\space))
         ;; Packing here instead of inside `chunk' to avoid pointer coercion
         (chunk (pack chunk))
         (space-mask   (chunk/= chunk +space+))
         (tab-mask     (chunk/= chunk +tab+))
         (newline-mask (chunk/= chunk +newline+))
         ;; Equality checks with simd will produce unsigned results of the same length
         (whitespace-pack (sb-simd-avx2:u8.32-and space-mask
                                                  tab-mask
                                                  newline-mask))
         ;; The produced mask is backwards
         (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
    (unless (zerop whitespace-bitmap)
      (incf index (rightmost-bit-index whitespace-bitmap)))))

(defun not-whitespace-p (character)
  (declare (type character character)
           (optimize (speed 3) (safety 0)))
  (let ((character (char-code character)))
    (not (or (eql character #.(char-code #\space))
             (eql character #.(char-code #\tab))
             (eql character #.(char-code #\newline))))))

(defun skip-to-next-character (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (cond
    ((not-whitespace-p (char string index))        index)
    ((not-whitespace-p (char string (incf index))) index)
    (t
     (incf index)
     (loop with whitespace-bitmap
           for new-index = (skip-whitespace string index)
           unless new-index
             do (incf index +chunk-length+)
           until new-index
           finally (return new-index)))))

(defun high-surrogate-p (code-value)
  "Character numbers between U+D800 and U+DFFF (inclusive) are reserved for use
with the UTF-16 encoding form (as surrogate pairs) and do not directly represent
characters. (Stolen from https://github.com/madnificent/jsown/blob/master/reader.lisp#L82C1-L87C55)"
  (<= #xD800 code-value #xDFFF))

(defun surrogate-char (high-surrogate low-surrogate)
  (declare (type fixnum high-surrogate low-surrogate))
  (code-char (+ #x10000
                (- low-surrogate #xDC00)
                (ash (- high-surrogate #xD800) 10))))

(defun %parse-surrogate (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (parse-integer string
                 :start index
                 :end (+ index 4)
                 :radix 16))

(defun %unescape-char (out string index)
  "Write the unescaped character to `out' and return an `index' pointing at the
first character after the escaped sequence."
  (declare (type simple-string string)
           (type string-stream out)
           (type fixnum index))
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
                     (write-char #\\ out)))
              (t (prog1 (incf index)
                   (write-char escaped-character out))))
            skip-next-backslash-p)))

(defun %unescape-string (parsed-string
                         string
                         current-index
                         double-quote-bitmap
                         next-double-quote
                         backslash-bitmap
                         next-backslash)
  (declare (type simple-string string)
           (type string-stream parsed-string)
           (type fixnum current-index)
           (type bitmap-index next-double-quote next-backslash))
  (loop with raw-string-length = (length string)
        with frozen-index = current-index ;; fixing current index as that was used to generate the initial chunk
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
           (type fixnum index))
  (let ((current-index (incf index)))
    (declare (type fixnum current-index))
    (values
     (with-output-to-string (parsed-string)
       (loop with raw-string-length = (length string)
             when (>= current-index raw-string-length)
               do (error 'unmatched-string-delimiter :starting-position (1- index))
                  ;; `next-' prefix essentially means offset from `current-index'
             do (let* ((chunk (chunk string current-index))
                       (chunk (pack chunk))

                       (double-quote-mask   (chunk= chunk +double-quote+))
                       (double-quote-bitmap (sb-simd-avx2:u8.32-movemask double-quote-mask))
                       (next-double-quote (next-offset double-quote-bitmap))

                       (backslash-mask   (chunk= chunk +backslash+))
                       (backslash-bitmap (sb-simd-avx2:u8.32-movemask backslash-mask))
                       (next-backslash (next-offset backslash-bitmap)))
                  (cond
                    ;; the end of the string is known and no escaping is needed
                    ((and next-double-quote
                          (or (not next-backslash)
                              (< next-double-quote next-backslash)))
                     (write-string string
                                   parsed-string
                                   :start current-index
                                   :end (incf current-index next-double-quote))
                     (incf current-index)
                     (loop-finish))

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
                                         string
                                         current-index
                                         double-quote-bitmap
                                         next-double-quote
                                         backslash-bitmap
                                         next-backslash)))))))
     current-index)))

(defun end-of-number (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (loop with current-index of-type fixnum = index
        with string-length of-type fixnum = (length string)
        while (< current-index string-length)
        do (let* ((chunk (chunk string current-index))
                  (chunk (pack chunk))
                  (space-mask   (chunk= chunk +space+))
                  (tab-mask     (chunk= chunk +tab+))
                  (newline-mask (chunk= chunk +newline+))
                  (comma-mask   (chunk= chunk +comma+))
                  (closing-bracket-mask (chunk= chunk +closing-bracket+))
                  (closing-brace-mask   (chunk= chunk +closing-brace+))
                  (whitespace-pack (sb-simd-avx2:u8.32-or space-mask
                                                          tab-mask
                                                          newline-mask
                                                          comma-mask
                                                          closing-bracket-mask
                                                          closing-brace-mask))
                  (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
             (if (zerop whitespace-bitmap)
                 (setf current-index (min (+ current-index +chunk-length+)
                                          string-length))
                 (return-from end-of-number
                   (incf current-index (rightmost-bit-index whitespace-bitmap)))))
  finally (when (>= current-index string-length)
            (return string-length))))

(defun %parse-number (string index)
  (declare (type simple-string string)
           (type fixnum index))
  ;; NOTE: Even in the hacky approach, this might be worse than traversing the string
  (let ((end-of-number (end-of-number string index))
        (*read-default-float-format* 'double-float)
        (*read-eval* nil))
    ;; HACK: A bit cheesy but it will do for now
    (values
     (read-from-string string
                       t
                       nil
                       :start index
                       :end end-of-number)
     end-of-number)))

(defun %parse-object (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (let ((current-index (skip-to-next-character string (1+ index)))
        (parsed-object (make-hash-table :test 'equal)))
    (declare (type fixnum current-index))
    ;; empty object check
    (when (char= (char string current-index) #\})
      (return-from %parse-object (values parsed-object current-index)))
    (values
     (loop with new-index
           with parsed-key and parsed-value
           with raw-string-length = (length string)
           while (< current-index raw-string-length)
           do (progn
                (when (char/= #\" (char string current-index))
                  (error (format nil "Key not of type string at  position ~a!"
                                 current-index)))
                (multiple-value-setq (parsed-key new-index)
                  (%parse-string string (incf current-index)))
                (setf current-index (skip-to-next-character string new-index))
                (if (char= #\: (char string current-index))
                    (incf current-index)
                    (error (format nil "Missing ':' after key '~a' at position ~a!"
                                   parsed-key current-index)))
                (multiple-value-setq (parsed-value new-index)
                  (parse string current-index))
                (setf current-index (skip-to-next-character string new-index)
                      (gethash parsed-key parsed-object) parsed-value)
                (let ((character (char string current-index)))
                  (cond
                    ((char= #\} character) (return parsed-object))
                    ((char/= #\, character) (error (format nil "Expected ',' after object value. Instead found ~a at position ~a!"
                                                           character current-index)))
                    (t (setf current-index (skip-to-next-character string (incf current-index))))))))
     (incf current-index))))

(defun %parse-array (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (let ((current-index (skip-to-next-character string (1+ index))))
    ;; empty array check
    (when (char= (char string current-index) #\])
      (return-from %parse-array (values nil current-index)))

    (values
     (loop with new-index and parsed-element
           with raw-string-length = (length string)
           while (< current-index raw-string-length)
           do (progn
                (multiple-value-setq (parsed-element new-index)
                  (parse string current-index))
                (setf current-index (skip-to-next-character string new-index)))
           collect parsed-element

           do (let ((character (char string current-index)))
                (cond
                  ((char= character #\]) (loop-finish))
                  ((char= character #\,) (incf current-index))
                  (t (error (format nil "Missing array delimiter at position ~a!" current-index))))))
     (incf current-index))))

(defun %parse-null (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (if (and (char= #\u (char string (1+ index)))
           (char= #\l (char string (+ index 2)))
           (char= #\l (char string (+ index 3))))
      (values nil (incf index 4))
      (error (format nil "Expected 'null' at position ~a!" index))))

(defun parse (string &optional (index 0))
  (declare (type simple-string string)
           (type fixnum index))
  (let* ((index (skip-to-next-character string index))
         (character (char string index)))
    (cond
      ((char= character #\{) (%parse-object string index))
      ((char= character #\[) (%parse-array string index))
      ((char= character #\") (%parse-string string index))
      ((char= character #\n) (%parse-null string index))
      ((or (digit-char-p character)
           (char= character #\-))
       (%parse-number string index))
      (t (error 'unexpected-character :index index
                                      :value character)))))

;;
;; Tests
;;

(5am:def-suite :jsoon-tests)

(5am:test :test-rightmost-bit-index
  (5am:is (eql 0 (rightmost-bit-index #b1)))
  (5am:is (eql 2 (rightmost-bit-index #b100)))
  (5am:is (eql 2 (rightmost-bit-index #b11011100))))

(5am:test :test-skip-whitespace
  (5am:is (eql 0
               (skip-whitespace "a" 0)))
  (5am:is (null
           (skip-whitespace " " 0))
          "No non-whitespace characters found")
  (5am:is (eql 2
               (skip-whitespace "  a   " 0))
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

(5am:test :test-%parse-array
  (5am:is (equal nil
                 (%parse-array "[   ]" 0))
          "Empty array")
  (5am:is (equal '(1 2 3)
                 (%parse-array "[ 1   , 2,3]" 0))
          "Spacing between numbers")
  (5am:is (equal '("a" "b" "c")
                 (%parse-array "[ \"a\",\"b\"  ,  \"c\"]" 0))
          "Spacing between string")
  (5am:is (equal '("a" 2 3 "c")
                 (%parse-array "[ \"a\", 2, 3  ,  \"c\"]" 0))
          "Mixed types")
  (5am:is (equal '(1 2 (3 "a") 5)
                 (%parse-array "[1,2,[3, \"a\"],5]" 0))
          "Nesting 1")
  (5am:is (equal `(1 2 (3 (,(format nil "~%"))) 5)
                 (%parse-array "[1,2,[3, [\"\\n\"]],5]" 0))
          "Nesting 2")
  (5am:is (equal `(1 nil (3 (,(format nil "~%"))) 5 nil)
                 (%parse-array "[1,null,[3, [\"\\n\"]],5, null]" 0))
          "Nulls"))
