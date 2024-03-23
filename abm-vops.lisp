(in-package :sb-vm)

(import '(sb-assem:inst))

(defknown jsoon::bit-scan-reverse ((unsigned-byte 64)) (integer 0 64)
    (foldable flushable movable))

(define-vop (jsoon::bit-scan-reverse)
  (:policy :fast-safe)
  (:translate jsoon::bit-scan-reverse)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 2
    (inst bsr r r)))

(defknown jsoon::bit-scan-forward ((unsigned-byte 64)) (integer 0 64)
    (foldable flushable movable))

(define-vop (jsoon::bit-scan-forward)
  (:policy :fast-safe)
  (:translate jsoon::bit-scan-forward)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 2
    (inst bsf r r)))
