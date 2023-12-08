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

(defconstant +space+   (sb-simd-avx2:s8.32 (char-code #\space)))
(defconstant +tab+     (sb-simd-avx2:s8.32 (char-code #\tab)))
(defconstant +newline+ (sb-simd-avx2:s8.32 (char-code #\newline)))

(defun rightmost-set-bit (n)
  "Returns a 0-based index of the location of the rightmost set bit of `n'.
Best not used with negative input."
  (declare (type fixnum n))
  (let ((rightmost-bit (logand (1+ (lognot n))
                               n)))
    (truncate (log rightmost-bit 2))))

(defun chunk (string index &optional (pad-character #\Nul))
  (declare (type string string)
           (type fixnum index)
           (type character pad-character))
  (let ((chunk (make-array (list +chunk-length+) :element-type '(signed-byte 8))))
    (declare (type (array (signed-byte 8) 1) chunk))
    (loop with upper-limit = (min (+ index +chunk-length+)
                                  (- (length string) index))
          for i from index below upper-limit
          for character = (char-code (char string i))
          do (setf (aref chunk i) character)
          finally (when pad-character
                    (loop with pad-value = (char-code pad-character)
                          for pad from upper-limit below +chunk-length+
                          do (setf (aref chunk pad) pad-value))))
    chunk))

(defun skip-whitespace (string index)
  "Skips to the first non-whitespace character."
  (declare (type string string)
           (type fixnum index))
  ;; NOTE: Padding with a matching character to make detection of chunks with
  ;; just whitespace easier when they are less than `+chunk-length+'.
  (let* ((chunk (chunk string index #\space))
         (chunk (sb-simd-avx2:s8.32-aref chunk 0))
         (space-mask   (sb-simd-avx2:s8.32/= chunk +space+))
         (tab-mask     (sb-simd-avx2:s8.32/= chunk +tab+))
         (newline-mask (sb-simd-avx2:s8.32/= chunk +newline+))
         (whitespace-pack (sb-simd-avx2:u8.32-and space-mask
                                                  tab-mask
                                                  newline-mask))
         ;; NOTE: The produced mask is backwards
         (whitespace-bitmap (sb-simd-avx2:u8.32-movemask whitespace-pack)))
    (if (zerop whitespace-bitmap)
        (values nil 0)
        (values (+ index
                   (rightmost-set-bit whitespace-bitmap))
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

;;
;; Tests
;;

(fiveam:def-suite :jsoon-tests)

(fiveam:test :test-rightmost-set-bit
  (fiveam:is (eql 0 (rightmost-set-bit #b1)))
  (fiveam:is (eql 2 (rightmost-set-bit #b100)))
  (fiveam:is (eql 2 (rightmost-set-bit #b11011100))))

(fiveam:test :test-skip-whitespace
  (fiveam:is (equal '(nil 0)
                    (skip-whitespace "a" 0)))
  )
