(in-package :jsoon)


(declaim (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))

(defparameter *data* (uiop:read-file-string "./10mb.json"))

(defparameter *mock* "   {  \"my-data\":  123}")

(defconstant +chunk-length+ (the fixnum 16))

(define-symbol-macro +space+        (sb-simd-avx2:u8.16 (char-code #\space)))
(define-symbol-macro +tab+          (sb-simd-avx2:u8.16 (char-code #\tab)))
(define-symbol-macro +newline+      (sb-simd-avx2:u8.16 (char-code #\newline)))
(define-symbol-macro +double-quote+ (sb-simd-avx2:u8.16 (char-code #\")))
(define-symbol-macro +backslash+    (sb-simd-avx2:u8.16 (char-code #\\)))
(define-symbol-macro +comma+        (sb-simd-avx2:u8.16 (char-code #\,)))
(define-symbol-macro +closing-bracket+ (sb-simd-avx2:u8.16 (char-code #\])))
(define-symbol-macro +closing-brace+   (sb-simd-avx2:u8.16 (char-code #\})))

(sb-ext:define-hash-table-test string= sxhash)

(deftype string-chunk ()
  `(simple-array (unsigned-byte 8) (,+chunk-length+)))

(defstruct context
  (string "" :type simple-string)
  (index 0 :type fixnum)
  (buffer (make-array (list +chunk-length+) :element-type '(unsigned-byte 8)) :type string-chunk))

(defmacro with-context ((&rest slots) instance &body body)
  `(symbol-macrolet (,@(when (member 'string slots)
                         `((string (context-string ,instance))))
                     ,@(when (member 'index slots)
                         `((index (context-index ,instance))))
                     ,@(when (member 'buffer slots)
                         `((buffer (context-buffer ,instance)))))
     ,@body))

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

(declaim (inline unset-rightmost-bit chunk
                 %unescape-char next-offset high-surrogate-p %parse-surrogate surrogate-char
                 skip-whitespace skip-to-next-character %parse-decimal-segment
                 not-whitespace-p %parse-number %parse-array %parse-object))

(defmacro chunk= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.16= (the (sb-ext:simd-pack (unsigned-byte 8)) ,chunk)
                                          (the (sb-ext:simd-pack (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro chunk=->bm (chunk value)
  "Compare chunks and convert to bitmap"
  `(sb-simd-avx2:u8.16-movemask (chunk= ,chunk ,value)))

(defmacro chunk/= (chunk value)
  `(let ((value-mask (sb-simd-avx2:u8.16/= (the (sb-ext:simd-pack (unsigned-byte 8)) ,chunk)
                                           (the (sb-ext:simd-pack (unsigned-byte 8)) ,value))))
     (declare (type (sb-ext:simd-pack (unsigned-byte 8)) value-mask))
     value-mask))

(defmacro pack (chunk)
  `(the (sb-ext:simd-pack (unsigned-byte 8))
        (sb-simd-avx2:u8.16-aref (the string-chunk ,chunk) 0)))

(defun unset-rightmost-bit (n)
  (declare (type fixnum n))
  (the fixnum (logand n (- n 1))))

(defun chunk (context &optional (pad-character #\Nul))
  (declare (type character pad-character)
           (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))
  (with-context (string index buffer) context
    (loop with upper-limit = (min (+ index +chunk-length+)
                                  (length string))
          for i from index below upper-limit
          for j from 0
          for character = (char-code (char string i))
          do (setf (aref buffer j) character)
          finally (loop with padding = (char-code pad-character)
                        for p from upper-limit below +chunk-length+
                        do (setf (aref buffer p) padding)))))

(defun next-offset (bitmap)
  "Returns the rightmost bit index (0-based) if `bitmap' > 0, otherwise NIL"
  (declare (type fixnum bitmap))
  (unless (zerop bitmap)
    (bit-scan-forward bitmap)))

(defun skip-whitespace (context)
  "Skips to the first non-whitespace character."
  (with-context (string index buffer) context
    (loop with string-length = (length string)
          if (< index string-length)
            do ;; Padding with a spaces to make detection of chunks with just whitespace
               ;; easier when they are less than `+chunk-length+'.
               (progn
                 (chunk context #\space)
                 (let* (;; Packing here instead of inside `chunk' to avoid pointer coercion
                        (chunk (pack buffer))
                        (space-mask   (chunk/= chunk +space+))
                        (tab-mask     (chunk/= chunk +tab+))
                        (newline-mask (chunk/= chunk +newline+))
                        ;; Equality checks with simd will produce unsigned results of the same length
                        (whitespace-pack (sb-simd-avx2:u8.16-and space-mask
                                                                 tab-mask
                                                                 newline-mask))
                        ;; The produced mask is backwards
                        (whitespace-bitmap (sb-simd-avx2:u8.16-movemask whitespace-pack)))
                   (declare (type fixnum whitespace-bitmap))
                   (if (zerop whitespace-bitmap)
                       (incf index +chunk-length+)
                       (progn
                         (incf index (bit-scan-forward whitespace-bitmap))
                         (loop-finish)))))
          else
            do (progn
                 (setf index string-length)
                 (loop-finish)))))

(defun not-whitespace-p (character)
  (declare (type character character)
           (optimize (speed 3) (safety 0)))
  (not (or (char= character #\space)
           (char= character #\tab)
           (char= character #\newline))))

(defun skip-to-next-character (context)
  (with-context (string index) context
    (or (not-whitespace-p (char string index))
        (not-whitespace-p (char string (incf index)))
        (progn
          (incf index)
          (skip-whitespace context)))))

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

(defun %unescape-char (context)
  "Write the unescaped character to `out' and return an `index' pointing at the
first character after the escaped sequence."
  (with-context (string index) context
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
         skip-next-backslash-p))))

(defun %unescape-string (context
                         double-quote-bitmap
                         next-double-quote
                         backslash-bitmap
                         next-backslash)
  (declare (type (or null fixnum) next-double-quote next-backslash)
           (type fixnum double-quote-bitmap backslash-bitmap))
  (with-output-to-string (parsed-string)
    (with-context (string index) context
      (loop with raw-string-length = (length string)
            with frozen-index = index ;; fixing current index as that was used to generate the initial chunk
            with remaining-string = (- raw-string-length frozen-index)
            with boundary = (min remaining-string (or next-double-quote +chunk-length+))
            while (and next-backslash (< next-backslash boundary))
            for backslash-index = (the fixnum (+ frozen-index next-backslash))
            when (< index backslash-index)
              do (write-string string
                               parsed-string
                               :start index
                               :end backslash-index)
                 ;; move to the next double-quote if we just escaped one
            when (and next-double-quote
                      (eql 1 (- next-double-quote next-backslash)))
              do (setf double-quote-bitmap (unset-rightmost-bit double-quote-bitmap)
                       next-double-quote (next-offset double-quote-bitmap)
                       boundary (min (or next-double-quote +chunk-length+)
                                     remaining-string))
            do (progn
                 (setf index backslash-index)
                 (multiple-value-bind (unescaped-char skip-next-backslash-p)
                     (%unescape-char context)
                   (write-char unescaped-char parsed-string)
                   (setf backslash-bitmap (if skip-next-backslash-p
                                              (unset-rightmost-bit (unset-rightmost-bit backslash-bitmap))
                                              (unset-rightmost-bit backslash-bitmap))
                         next-backslash (next-offset backslash-bitmap))))))))

(defun %parse-string (context)
  (with-context (string index buffer) context
    (let ((substrings nil)
          (done nil)
          (raw-string-length (length string))
          (starting-position index))
      (declare (type fixnum raw-string-length)
               (dynamic-extent substrings))
      (when (char= #\" (char string (incf index)))
        (return-from %parse-string ""))
      (setf substrings
            (loop when (>= index raw-string-length)
                    do (error 'unmatched-string-delimiter :starting-position starting-position)
                       ;; `next-' prefix essentially means offset from `index'
                  collect (progn
                            (chunk context)
                            (let* ((chunk (pack buffer))
                                   (double-quote-bitmap (chunk=->bm chunk +double-quote+))
                                   (next-double-quote   (next-offset (the fixnum double-quote-bitmap)))
                                   (backslash-bitmap    (chunk=->bm chunk +backslash+))
                                   (next-backslash      (next-offset (the fixnum backslash-bitmap))))
                              (declare (type (or null fixnum) next-double-quote next-backslash))
                              (cond
                                ;; the end of the string is known and no escaping is needed
                                ((and next-double-quote
                                      (or (not next-backslash)
                                          (< next-double-quote next-backslash)))
                                 (prog1
                                     (subseq string
                                             index
                                             (incf index next-double-quote))
                                   (incf index)
                                   (setf done t)))

                                ;; no string delimiter found and nothing to unescape, move forward
                                ((and (not next-double-quote) (not next-backslash))
                                 (subseq string
                                         index
                                         (setf index (min raw-string-length
                                                          (+ index +chunk-length+)))))

                                ((or (and (not next-double-quote) next-backslash)
                                     (> next-double-quote next-backslash))
                                 (%unescape-string context
                                                   double-quote-bitmap
                                                   next-double-quote
                                                   backslash-bitmap
                                                   next-backslash)))))
                  until done))
      (if (consp substrings)
          (apply #'concatenate 'string substrings)
          (car substrings)))))

(defun %parse-exponent-segment (context number)
  (declare (type double-float number))
  (with-context (string index) context
    (multiple-value-bind (exponent current-index)
        (parse-integer string :start index :junk-allowed t)
      (declare (type fixnum exponent current-index))
      (let ((number (* number (expt 10.0d0 exponent))))
        (declare (type double-float number))
        (setf index current-index)
        number))))

(defun %parse-decimal-segment (context integer)
  (declare (type fixnum integer))
  (with-context (string index) context
    (multiple-value-bind (decimal current-index)
        (parse-integer string :start index :junk-allowed t)
      (declare (type fixnum decimal current-index))
      (let ((number (+ integer (/ decimal (expt 10.0d0 (- current-index index))))))
        (declare (type double-float number))
        (setf index current-index)
        (if (and current-index (char= #\e (char string current-index)))
            (progn
              (incf index)
              (%parse-exponent-segment context number))
            number)))))

(defun %parse-number (context)
  "Numbers have at most 3 segments. 0.12e-10. The 2 delimiters being `.' and `e'."
  (with-context (string index) context
    (multiple-value-bind (integer current-index)
        (parse-integer string :start index :junk-allowed t)
      (declare (type fixnum integer))
      (setf index current-index)
      (if (char= #\. (char string current-index))
          (progn
            (incf index)
            (%parse-decimal-segment context integer))
          (if (char= #\e (char string current-index))
              (progn
                (incf index)
                (%parse-exponent-segment context (coerce integer 'double-float)))
              integer)))))

(defun %parse-object (context)
  (with-context (string index) context
    (incf index)
    (skip-to-next-character context)
    (let ((parsed-object (make-hash-table :test 'equal
                                          :size 10
                                          :rehash-size 1.5
                                          :rehash-threshold 0.65)))
      ;; empty object check
      (when (char= (char string index) #\})
        (return-from %parse-object parsed-object))
      (loop with raw-string-length = (length string)
            while (< index raw-string-length)
            when (char/= #\" (char string index))
              do (error "Key not of type string at  position ~a!" index)
            do (let ((parsed-key (%parse-string context)))
                 (skip-to-next-character context)
                 (if (char= #\: (char string index))
                     (incf index)
                     (error "Missing ':' after key '~a' at position ~a!" parsed-key index))
                 (let ((parsed-value (%parse context)))
                   (setf (gethash parsed-key parsed-object) parsed-value)
                   (skip-to-next-character context)
                   (let ((character (char string index)))
                     (declare (type character character))
                     (cond
                       ((char= #\} character) (loop-finish))
                       ((char/= #\, character) (error "Expected ',' after object value. Instead found ~a at position ~a!"
                                                      character index))
                       (t (incf index)
                          (skip-to-next-character context)))))))
      (incf index)
      parsed-object)))

(defun %parse-array (context)
  (with-context (string index) context
    (incf index)
    (skip-to-next-character context)
    ;; empty array check
    (when (char= (char string index) #\])
      (return-from %parse-array nil))
    (loop with raw-string-length = (length string)
          while (< index raw-string-length)
          collect (let ((parsed-element (%parse context)))
                    (skip-to-next-character context)
                    parsed-element)
          do (let ((character (char string index)))
               (declare (type character character))
               (cond
                 ((char= character #\]) (loop-finish))
                 ((char= character #\,) (incf index))
                 (t (error "Missing array delimiter at position ~a!" index))))
          finally (incf index))))

(defun %parse-null (context)
  (with-context (string index) context
    (if (and (char= #\u (char string (1+ index)))
             (char= #\l (char string (+ index 2)))
             (char= #\l (char string (+ index 3))))
        (values nil (incf index 4))
        (error "Expected 'null' at position ~a!" index))))

(defun %parse (context &optional (skip-p t))
  (when skip-p
    (skip-to-next-character context))
  (with-context (string index) context
    (let ((character (char string index)))
      (cond
        ((char= character #\{) (%parse-object context))
        ((char= character #\[) (%parse-array context))
        ((char= character #\") (%parse-string context))
        ((char= character #\n) (%parse-null context))
        ((or (digit-char-p character)
             (char= character #\-))
         (%parse-number context))
        (t (error 'unexpected-character :index index
                                        :value character))))))

(defun parse (string &optional (index 0) (skip-p t))
  (declare (type simple-string string)
           (type fixnum index))
  (let ((context (make-context :string string :index index)))
    (%parse context skip-p)))

;;
;; Tests
;;

(5am:def-suite :jsoon-tests)

(defun rightmost-bit-index (n)
  "used just in testing"
  (declare (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (bit-scan-forward n))

(defun parse-string (string &optional (index 0))
  (%parse-string (make-context :string string :index index)))

(defun parse-array (string &optional (index 0))
  (%parse-array (make-context :string string :index index)))

(defun parse-number (string &optional (index 0))
  (%parse-number (make-context :string string :index index)))

(defun test/skip-whitespace (string &optional (index 0))
  (let ((context (make-context :string string :index index)))
    (skip-whitespace context)
    (context-index context)))

(5am:test :test-bit-scan-forward
  (5am:is (eql 0 (rightmost-bit-index #b1)))
  (5am:is (eql 2 (rightmost-bit-index #b100)))
  (5am:is (eql 2 (rightmost-bit-index #b11011100))))

(5am:test :test-skip-whitespace
  (5am:is (eql 0 (test/skip-whitespace "a" 0)))
  (5am:is (eql 1 (test/skip-whitespace " " 0))
          "No non-whitespace characters found")
  (5am:is (eql 2 (test/skip-whitespace "  a   " 0))
          "Bitmask is reversed & leading zeroes are ommitted."))

(5am:test :test-parse-string
  (let ((empty-string (coerce #(#\" #\") 'string)))
    (5am:is (string= ""
                     (parse-string empty-string 0))
            "Parse empty string"))
  (let ((long-string "this is a very very very very large test"))
    (5am:is (string= (format nil "~a~%" long-string)
                     (parse-string (format nil "\"~a\\n\"" long-string) 0))
            "Test strings larger than chunk size with escape sequence"))
  (let ((long-string "this is a very very very very large test"))
    (5am:is (string= (format nil "~a~%123" long-string)
                     (parse-string (format nil "\"~a\\n123\"" long-string) 0))
            "Test strings larger than chunk size with escape sequence followed by normal characters"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\" #\") 'string)))
    (5am:is (string= "\\\""
                     (parse-string str 0))
            "Unescape backslash and then double-quote"))
  (let ((str (coerce #(#\" #\\ #\\ #\\ #\") 'string)))
    (5am:signals unmatched-string-delimiter (parse-string str 0)))
  (5am:is (string= (coerce #(#\SYMBOL_FOR_START_OF_HEADING) 'string)
                   (parse-string "\"\\uD800\\u0001\"" 0))
          "UTF-16 character 2-bytes")
  (5am:is (string= (coerce #(#\HANGUL_SYLLABLE_HIB) 'string)
                   (parse-string "\"\\uD799\"" 0))
          "UTF-16 character 1-byte"))

(5am:test :test-%parse-array
  (5am:is (equal nil
                 (parse-array "[   ]" 0))
          "Empty array")
  (5am:is (equal '(1 2 3)
                 (parse-array "[ 1   , 2,3]" 0))
          "Spacing between numbers")
  (5am:is (equal '("a" "b" "c")
                 (parse-array "[ \"a\",\"b\"  ,  \"c\"]" 0))
          "Spacing between string")
  (5am:is (equal '("a" 2 3 "c")
                 (parse-array "[ \"a\", 2, 3  ,  \"c\"]" 0))
          "Mixed types")
  (5am:is (equal '(1 2 (3 "a") 5)
                 (parse-array "[1,2,[3, \"a\"],5]" 0))
          "Nesting 1")
  (5am:is (equal `(1 2 (3 (,(format nil "~%"))) 5)
                 (parse-array "[1,2,[3, [\"\\n\"]],5]" 0))
          "Nesting 2")
  (5am:is (equal `(1 nil (3 (,(format nil "~%"))) 5 nil)
                 (parse-array "[1,null,[3, [\"\\n\"]],5, null]" 0))
          "Nulls"))

(5am:test :test-%parse-number
  (5am:is (= 5
             (parse-number "5.000" 0)))
  (5am:is (= 100
             (parse-number "1e2" 0)))
  (5am:is (= 120
             (parse-number "1.2e2" 0)))
  (5am:is (= 1.2d0
             (parse-number "120.0e-2" 0))))
