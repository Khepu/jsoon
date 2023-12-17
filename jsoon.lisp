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

(defun rightmost-bit (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (logand (1+ (lognot n))
          n))

(defun rightmost-bit-index (n)
  "Returns a 0-based index of the location of the rightmost set bit of `n'."
  (declare (type fixnum n))
  (truncate (log (rightmost-bit n) 2)))

(defun unset-rightmost-bit (n)
  (declare (type fixnum n))
  (logxor n (rightmost-bit n)))

(defun chunk (string index &optional (pad-character #\Nul) cutoff)
  (declare (type string string)
           (type fixnum index)
           (type character pad-character))
  (let ((chunk (make-array (list +chunk-length+) :element-type '(signed-byte 8))))
    (declare (type (array (signed-byte 8) 1) chunk))
    (loop with upper-limit = (min (+ index +chunk-length+)
                                  (if cutoff
                                      (+ index cutoff)
                                      (- (length string) index)))
          for i from index below upper-limit
          for character = (char-code (char string i))
          do (setf (aref chunk i) character)
          finally (unless (eql pad-character #\Nul)
                    (loop with pad-value = (char-code pad-character)
                          for pad from upper-limit below +chunk-length+
                          do (setf (aref chunk pad) pad-value))))
    chunk))


(defun skip-whitespace (string index)
  "Skips to the first non-whitespace character."
  (declare (type string string)
           (type fixnum index))
  ;; NOTE: Padding with a spaces to make detection of chunks with just
  ;; whitespace easier when they are less than `+chunk-length+'.
  (let* ((chunk (chunk string index #\space))
         ;; NOTE: Packing here instead of inside `chunk' to avoid pointer coercion
         (chunk (sb-simd-avx2:s8.32-aref chunk 0))
         (space-mask   (sb-simd-avx2:s8.32/= chunk +space+))
         (tab-mask     (sb-simd-avx2:s8.32/= chunk +tab+))
         (newline-mask (sb-simd-avx2:s8.32/= chunk +newline+))
         ;; NOTE: Equality checks with simd will produce unsigned results of the
         ;; same length
         (whitespace-pack (sb-simd-avx2:u8.32-and space-mask
                                                  tab-mask
                                                  newline-mask))
         ;; NOTE: The produced mask is backwards
         (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
    (if (zerop whitespace-bitmap)
        (values nil 0)
        (values (+ index
                   (rightmost-bit-index whitespace-bitmap))
                whitespace-bitmap))))

(defun skip-to-next-character (string index)
  (declare (type string string)
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

(defun %parse-string (string index)
  (declare (type string string)
           (type fixnum index))
  (with-output-to-string (parsed-string)
    (loop with current-index = (1+ index)
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
                 ;; The end of the string is known and no escaping is needed
                 ((and next-double-quote (or (not next-backslash)
                                             (< next-double-quote next-backslash)))
                  (write-string (subseq string current-index
                                               (+ current-index next-double-quote)))
                  (return))
                 ;; no string delimiter found and nothing to unescape, move forward
                 ((and (not next-double-quote) (not next-backslash))
                  (incf current-index +chunk-length+))

                 ;; loop through all marked escaped characters and unescape them
                 ((> next-double-quote next-backslash)
                  (loop with frozen-index = current-index ;; fixing current index as that was used to generate the initial chunk
                        with local-backslash-bitmap = backslash-bitmap
                        with local-next-backslash = next-backslash
                        while (and local-next-backslash
                                   (> next-double-quote local-next-backslash))
                        for backslash-index = (+ frozen-index local-next-backslash)
                        ;; for escaped-char = (char string backslash-index)
                        for unescaped-char = (%unescape-char string backslash-index)
                        do (progn
                             (write-string (subseq string current-index backslash-index))
                             (write-char unescaped-char)
                             ;; set the index past the escaped character
                             (setf current-index (+ backslash-index 2) ;; skip step is wrong
                                   local-backslash-bitmap (unset-rightmost-bit local-backslash-bitmap)
                                   local-next-backslash (unless (zerop local-backslash-bitmap)
                                                          (rightmost-bit-index local-backslash-bitmap)))))))))))

(defun %parse-object (string index)
  (declare (type string string)
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

(defun prase (string &optional (index 0))
  (declare (type string string))
  (let* ((index (skip-to-next-character string index))
         (character (char string index)))
    (cond
      ((eql character #\{)          (%parse-object string index))
      ((eql character #\[)          (%parse-array string index))
      ((eql character #\")          (%parse-string string index))
      ((or (digit-char-p character)
           (eql charachter #\-))    (%parse-number string index))
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
