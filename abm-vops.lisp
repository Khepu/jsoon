(in-package :jsoon)

(sb-c:defknown bit-scan-forward ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(in-package :sb-vm)

(define-vop (jsoon::bit-scan-forward)
  (:policy :fast)
  (:translate jsoon::bit-scan-forward)
  (:args (x :scs (any-reg) :target r))
  (:arg-types positive-fixnum)
  (:results (r :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst bsf x r)
    (inst dec r)))
