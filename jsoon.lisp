(in-package :jsoon)


(declaim (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))

(defparameter *data* (uiop:read-file-string "./10mb.json"))

(defparameter *mock* "   {  \"my-data\":  123}")

(defconstant +chunk-length+ (the fixnum 32))

(define-symbol-macro +space+        (sb-simd-avx2:u8.32 (char-code #\space)))
(define-symbol-macro +tab+          (sb-simd-avx2:u8.32 (char-code #\tab)))
(define-symbol-macro +newline+      (sb-simd-avx2:u8.32 (char-code #\newline)))
(define-symbol-macro +double-quote+ (sb-simd-avx2:u8.32 (char-code #\")))
(define-symbol-macro +backslash+    (sb-simd-avx2:u8.32 (char-code #\\)))
(define-symbol-macro +comma+        (sb-simd-avx2:u8.32 (char-code #\,)))
(define-symbol-macro +closing-bracket+ (sb-simd-avx2:u8.32 (char-code #\])))
(define-symbol-macro +closing-brace+   (sb-simd-avx2:u8.32 (char-code #\})))

(deftype string-chunk ()
  `(array (unsigned-byte 8) (,+chunk-length+)))

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

(declaim (inline rightmost-bit unset-rightmost-bit chunk
                 %unescape-char next-offset high-surrogate-p %parse-surrogate surrogate-char
                 skip-to-next-character %parse-decimal-segment %parse-exponent-segment
                 not-whitespace-p end-of-number %parse-number))

(defmacro chunk= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.32= (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,chunk)
                                          (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack-256 (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro chunk=->bm (chunk value)
  "Compare chunks and convert to bitmap"
  `(sb-simd-avx2:u8.32-movemask (chunk= ,chunk ,value)))

(defmacro chunk/= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.32/= (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,chunk)
                                           (the (sb-ext:simd-pack-256 (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack-256 (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro pack (chunk)
  `(the (sb-ext:simd-pack-256 (unsigned-byte 8))
        (sb-simd-avx2:u8.32-aref (the string-chunk ,chunk) 0)))

(defun rightmost-bit (n)
  (declare (type fixnum n))
  (the fixnum (logand (1+ (lognot n))
                      n)))

(defun unset-rightmost-bit (n)
  (declare (type fixnum n))
  (logxor n (rightmost-bit n)))

(defun chunk (string index &optional (pad-character #\Nul))
  (declare (type simple-string string)
           (type fixnum index)
           (type character pad-character)
           (optimize (speed 3) (safety 0) (compilation-speed 0)))
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
  (declare (type fixnum bitmap))
  (unless (zerop bitmap)
    (bit-scan-forward bitmap)))

(defun skip-whitespace (string index)
  "Skips to the first non-whitespace character."
  (declare (type simple-string string)
           (type fixnum index))
  (loop with string-length = (length string)
        if (< index string-length)
          do ;; Padding with a spaces to make detection of chunks with just whitespace
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
               (declare (type fixnum whitespace-bitmap))
               (if (zerop whitespace-bitmap)
                   (incf index +chunk-length+)
                   (return (incf index (bit-scan-forward whitespace-bitmap)))))
        else
          do (return string-length)))

(defun not-whitespace-p (character)
  (declare (type character character)
           (optimize (speed 3) (safety 0)))
  (not (or (char= character #\space)
           (char= character #\tab)
           (char= character #\newline))))

(defun skip-to-next-character (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (cond
    ((not-whitespace-p (schar string index))        index)
    ((not-whitespace-p (schar string (incf index))) index)
    (t (skip-whitespace string (incf index)))))

(defun high-surrogate-p (code-value)
  "Character numbers between U+D800 and U+DFFF (inclusive) are reserved for use
with the UTF-16 encoding form (as surrogate pairs) and do not directly represent
characters. (Stolen from https://github.com/madnificent/jsown/blob/master/reader.lisp#L82C1-L87C55)"
  (<= #xD800 code-value #xDFFF))

(defun surrogate-char (high-surrogate low-surrogate)
  (declare (type fixnum high-surrogate low-surrogate))
  (code-char (+ #x10000
                (decf low-surrogate #xDC00)
                (the fixnum (ash (decf high-surrogate #xD800) 10)))))

(defun %parse-surrogate (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (parse-integer string
                 :start index
                 :end (+ index 4)
                 :radix 16))

(defun %unescape-char (string index)
  "Write the unescaped character to `out' and return an `index' pointing at the
first character after the escaped sequence."
  (declare (type simple-string string)
           (type fixnum index))
  (let ((escaped-character (char string (incf index)))
        (skip-next-backslash-p nil))
    (incf index)
    (values
     (case escaped-character
       (#\n #\linefeed)
       (#\f #\linefeed)
       (#\b #\backspace)
       (#\r #\return)
       (#\t #\tab)
       (#\u (let ((surrogate (%parse-surrogate string index))) ;; skip 'u'
              (if (high-surrogate-p surrogate)
                  (let ((low-surrogate (%parse-surrogate string (+ index 6)))) ;; 4 digits + backslash + 'u' + 1 to skip 'u'
                    (setf skip-next-backslash-p t)
                    (incf index 10)
                    (surrogate-char surrogate low-surrogate))
                  (progn
                    (incf index 4)
                    (code-char surrogate)))))
       (#\\ (progn
              (setf skip-next-backslash-p t)
              #\\))
       (t escaped-character))
     index
     skip-next-backslash-p)))

(defun %unescape-string (string
                         current-index
                         double-quote-bitmap
                         next-double-quote
                         backslash-bitmap
                         next-backslash)
  (declare (type simple-string string)
           (type fixnum current-index)
           (type (or null fixnum) next-double-quote next-backslash))
  (values
   (with-output-to-string (parsed-string)
     (loop with raw-string-length = (length string)
           with frozen-index = current-index ;; fixing current index as that was used to generate the initial chunk
           with remaining-string = (- raw-string-length frozen-index)
           with boundary = (min remaining-string (or next-double-quote +chunk-length+))
           while (and next-backslash (< next-backslash boundary))
           for backslash-index = (the fixnum (+ frozen-index next-backslash))
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
           do (multiple-value-bind (unescaped-char new-index skip-next-backslash-p)
                  (%unescape-char string backslash-index)
                (write-char unescaped-char parsed-string)
                (setf current-index new-index
                      backslash-bitmap (if skip-next-backslash-p
                                           (unset-rightmost-bit (unset-rightmost-bit backslash-bitmap))
                                           (unset-rightmost-bit backslash-bitmap))
                      next-backslash (next-offset backslash-bitmap)))))
   current-index))


(defun %parse-string (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (incf index)
  (when (char= #\" (char string index))
    (return-from %parse-string (values "" index)))
  (let ((current-index index)
        (substrings nil)
        (chunk))
    (declare (type fixnum current-index)
             (dynamic-extent chunk substrings))
    (setf substrings
          (loop with done = nil
                with raw-string-length = (length string)
                when (>= current-index raw-string-length)
                  do (error 'unmatched-string-delimiter :starting-position (1- index))
                     ;; `next-' prefix essentially means offset from `current-index'
                collect (progn
                          (format nil "current-index: ~a~%" current-index)
                          (setf chunk (chunk string current-index))
                          (let* ((chunk (pack chunk))
                                 (double-quote-bitmap (chunk=->bm chunk +double-quote+))
                                 (next-double-quote (next-offset (the fixnum double-quote-bitmap)))
                                 (backslash-bitmap   (chunk=->bm chunk +backslash+))
                                 (next-backslash (next-offset (the fixnum backslash-bitmap))))
                            (declare (type (or null fixnum) next-double-quote next-backslash)
                                     (dynamic-extent next-double-quote next-backslash chunk
                                                     double-quote-bitmap backslash-bitmap))
                            (cond
                              ;; the end of the string is known and no escaping is needed
                              ((and next-double-quote
                                    (or (not next-backslash)
                                        (< next-double-quote next-backslash)))
                               (prog1
                                   (subseq string
                                           current-index
                                           (incf current-index next-double-quote))
                                 (incf current-index)
                                 (setf done t)))

                              ;; no string delimiter found and nothing to unescape, move forward
                              ((and (not next-double-quote) (not next-backslash))
                               (subseq string
                                       current-index
                                       (setf current-index (min raw-string-length
                                                                (+ current-index +chunk-length+)))))

                              ((or (and (not next-double-quote) next-backslash)
                                   (> next-double-quote next-backslash))
                               (multiple-value-bind (parsed-string new-index)
                                   (%unescape-string string
                                                     current-index
                                                     double-quote-bitmap
                                                     next-double-quote
                                                     backslash-bitmap
                                                     next-backslash)
                                 (setf current-index new-index)
                                 parsed-string)))))
                when done
                do (loop-finish)))
    (values
     (if (> (length substrings) 1)
         (apply #'concatenate 'string substrings)
         (car substrings))
     current-index)))

(defun %parse-exponent-segment (string index number)
  (declare (type simple-string string)
           (type fixnum index)
           (type double-float number))
  (multiple-value-bind (exponent current-index)
      (parse-integer string :start index :junk-allowed t)
    (declare (type fixnum exponent current-index))
    (let ((number (* number (expt 10.0d0 exponent))))
      (declare (type double-float number))
      (values number current-index))))

(defun %parse-decimal-segment (string index integer)
  (declare (type simple-string string)
           (type fixnum index integer))
  (multiple-value-bind (decimal current-index)
      (parse-integer string :start index :junk-allowed t)
    (declare (type fixnum decimal current-index))
    (let ((number (+ integer (/ decimal (expt 10.0d0 (- current-index index))))))
      (declare (type double-float number))
      (if (and current-index (char= #\e (char string current-index)))
          (%parse-exponent-segment string
                                   (incf current-index)
                                   number)
          (values number current-index)))))

(defun %parse-number (string index)
  (declare (type simple-string string)
           (type fixnum index))
  ;; Numbers have at most 3 segments. 0.12e-10. The 2 delimiters being `.' and `e'.
  (multiple-value-bind (integer current-index)
      (parse-integer string :start index :junk-allowed t)
    (declare (type fixnum integer))
    (if (and current-index (char= #\. (char string current-index)))
        (%parse-decimal-segment string
                                (incf current-index)
                                integer)
        (if (and current-index (char= #\e (char string current-index)))
            (%parse-exponent-segment string
                                     (incf current-index)
                                     (coerce integer 'double-float))
            (values integer current-index)))))

(defun %parse-object (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (let ((current-index (skip-to-next-character string (1+ index)))
        (parsed-object (make-hash-table :test 'equal
                                        :size 10
                                        :rehash-size 2
                                        :rehash-threshold 1)))
    (declare (type fixnum current-index))
    ;; empty object check
    (when (char= (char string current-index) #\})
      (return-from %parse-object (values parsed-object current-index)))
    (loop with raw-string-length = (length string)
          while (< current-index raw-string-length)
          when (char/= #\" (char string current-index))
            do (error "Key not of type string at  position ~a!" current-index)
          do (multiple-value-bind (parsed-key new-index)
                 (%parse-string string current-index)
               (setf current-index (skip-to-next-character string new-index))
               (if (char= #\: (char string current-index))
                   (incf current-index)
                   (error "Missing ':' after key '~a' at position ~a!" parsed-key current-index))
               (multiple-value-bind (parsed-value new-index)
                   (parse string current-index)
                 (setf current-index (skip-to-next-character string new-index)
                       (gethash parsed-key parsed-object) parsed-value)
                 (let ((character (char string current-index)))
                   (declare (dynamic-extent character)
                            (type character character))
                   (cond
                     ((char= #\} character) (loop-finish))
                     ((char/= #\, character) (error "Expected ',' after object value. Instead found ~a at position ~a!"
                                                    character current-index))
                     (t (setf current-index (skip-to-next-character string (incf current-index)))))))))
    (values parsed-object (incf current-index))))

(defun %parse-array (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (let ((current-index (skip-to-next-character string (incf index))))
    ;; empty array check
    (when (char= (char string current-index) #\])
      (return-from %parse-array (values nil (incf current-index))))
    (values
     (loop with raw-string-length = (length string)
           while (< current-index raw-string-length)
           collect (multiple-value-bind (parsed-element new-index)
                       (parse string current-index)
                     (setf current-index (skip-to-next-character string new-index))
                     parsed-element)
           do (let ((character (char string current-index)))
                (declare (dynamic-extent character)
                         (type character character))
                (cond
                  ((char= character #\]) (loop-finish))
                  ((char= character #\,) (incf current-index))
                  (t (error "Missing array delimiter at position ~a!" current-index)))))
     (incf current-index))))

(defun %parse-null (string index)
  (declare (type simple-string string)
           (type fixnum index))
  (if (and (char= #\u (char string (1+ index)))
           (char= #\l (char string (+ index 2)))
           (char= #\l (char string (+ index 3))))
      (values nil (incf index 4))
      (error "Expected 'null' at position ~a!" index)))

(defun parse (string &optional (index 0) (skip-p t))
  (declare (type simple-string string)
           (type fixnum index))
  (let* ((index (if skip-p
                    (skip-to-next-character string index)
                    index))
         (character (char string index)))
    (declare (dynamic-extent character)
             (type character character))
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

(defun rightmost-bit-index (n)
  "used just in testing"
  (declare (type fixnum n))
  (bit-scan-forward n))

(5am:test :test-bit-scan-forward
  (5am:is (eql 0 (rightmost-bit-index #b1)))
  (5am:is (eql 2 (rightmost-bit-index #b100)))
  (5am:is (eql 2 (rightmost-bit-index #b11011100))))

(5am:test :test-skip-whitespace
  (5am:is (eql 0 (skip-whitespace "a" 0)))
  (5am:is (eql 1 (skip-whitespace " " 0))
          "No non-whitespace characters found")
  (5am:is (eql 2 (skip-whitespace "  a   " 0))
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

(5am:test :test-%parse-number
  (5am:is (= 5
             (%parse-number "5.000" 0)))
  (5am:is (= 100
             (%parse-number "1e2" 0)))
  (5am:is (= 120
             (%parse-number "1.2e2" 0)))
  (5am:is (= 1.2d0
             (%parse-number "120.0e-2" 0))))
