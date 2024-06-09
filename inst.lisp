(sb-ext:unlock-package (find-package :sb-assem))
(sb-ext:unlock-package (find-package :sb-disassem))

(in-package :sb-assem)

(defun %def-inst-encoder (symbol thing &optional accept-prefixes)
  (let ((function
          (or thing                     ; load-time effect passes the definition
              ;; (where compile-time doesn't).
              ;; Otherwise, take what we already had so that a compile-time
              ;; effect doesn't clobber an already-working hash-table entry
              ;; if re-evaluating a define-instruction form.
              (car (ensure-list (gethash symbol *inst-encoder*))))))
    (setf (gethash symbol *inst-encoder*)
          (if accept-prefixes (cons function t) function))))

(defmacro define-instruction (name lambda-list &rest options)
  (binding* ((fun-name (intern (symbol-name name) *backend-instruction-set-package*))
             (segment-name (car lambda-list))
             (vop-name nil)
             (emitter nil)
             (decls nil)
             (attributes nil)
             (cost nil)
             (dependencies nil)
             (delay nil)
             (pinned nil)
             (pdefs nil))
    (declare (ignorable pinned))
    (dolist (option-spec options)
      (multiple-value-bind (option args)
          (if (consp option-spec)
              (values (car option-spec) (cdr option-spec))
              (values option-spec nil))
        (case option
          (:emitter
           (when emitter
             (error "You can only specify :EMITTER once per instruction."))
           (setf emitter args))
          (:declare
           (setf decls (append decls args)))
          (:attributes
           (setf attributes (append attributes args)))
          (:cost
           (setf cost (first args)))
          (:dependencies
           (setf dependencies (append dependencies args)))
          (:delay
           (when delay
             (error "You can only specify :DELAY once per instruction."))
           (setf delay args))
          (:pinned
           (setf pinned t))
          (:vop-var
           (if vop-name
               (error "You can only specify :VOP-VAR once per instruction.")
               (setf vop-name (car args))))
          (:printer
           (let* ((inst-args (second args))
                  (names (mapcar #'car inst-args)))
             (when (> (length names) (length (remove-duplicates names)))
               (error "Duplicate operand names in ~S~%" args)))
           (destructuring-bind (name operands . options) args
             (push ``(,',name (,,@(mapcar (lambda (x) ``(,',(car x) ,,@(cdr x)))
                                          operands))
                              ,,@options) pdefs)))
          (t
           (error "unknown option: ~S" option)))))
    (when emitter
      (unless cost (setf cost 1))
      #+sb-dyncount
      (push `(when (segment-collect-dynamic-statistics ,segment-name)
               (let* ((info (sb-c:ir2-component-dyncount-info
                             (sb-c:component-info
                              sb-c:*component-being-compiled*)))
                      (costs (sb-c:dyncount-info-costs info))
                      (block-number (sb-c:block-number
                                     (sb-c:ir2-block-block
                                      (sb-c:vop-block ,vop-name)))))
                 (incf (aref costs block-number) ,cost)))
            emitter)
      (when assem-scheduler-p
        (if pinned
            (setf emitter
                  `((when (segment-run-scheduler ,segment-name)
                      (schedule-pending-instructions ,segment-name))
                    ,@emitter))
            (let ((flet-name (make-symbol (concatenate 'string "ENCODE-"
                                                       (string fun-name))))
                  (inst-name '#:inst))
              (setf emitter `((flet ((,flet-name (,segment-name)
                                       ,@emitter))
                                (if (segment-run-scheduler ,segment-name)
                                    (let ((,inst-name
                                            (make-instruction
                                             (incf (segment-inst-number
                                                    ,segment-name))
                                             #',flet-name
                                             (instruction-attributes
                                              ,@attributes)
                                             (progn ,@delay))))
                                      ,@(when dependencies
                                          `((note-dependencies
                                             (,segment-name ,inst-name)
                                             ,@dependencies)))
                                      (queue-inst ,segment-name ,inst-name))
                                    (,flet-name ,segment-name)))))))))
    `(progn
       #-sb-xc-host ; The disassembler is not used on the host.
       (setf (get ',fun-name 'sb-disassem::instruction-flavors)
             (list ,@pdefs))
       ,@(when emitter
           (let* ((operands (cdr lambda-list))
                  (accept-prefixes (eq (car operands) '&prefix)))
             (when accept-prefixes (setf operands (cdr operands)))
             `((eval-when (:compile-toplevel)
                 (%def-inst-encoder ',fun-name nil ',accept-prefixes))
               (%def-inst-encoder
                ',fun-name
                (named-lambda ,(string fun-name) (,segment-name ,@operands)
                  (declare ,@decls)
                  (let ,(and vop-name `((,vop-name *current-vop*)))
                    (block ,fun-name ,@emitter)))
                ',accept-prefixes))))
       ',fun-name)))

(in-package :sb-x86-64-asm)

(sb-assem::define-instruction blsr (segment dst src)
  (:printer ext-reg-reg/mem-no-width
            ((op '(#xf3 1))
             (opcode-prefix #x0f38)
             (reg/mem nil :type 'sized-reg/mem))
            '(:name :tab reg/mem ", " reg))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (not (eq size :qword))
       (error "BLSR instruction only supported for 64-bit operand size")))
   (emit-byte segment #xC4)
   (emit-byte segment #b11100010)
   (emit-byte segment (logior #b10000000
                              (ash (lognot (reg-id-num (reg-id dst))) 3)))
   ;; opcode
   (emit-byte segment #xf3)
   ;; mod-r/m byte
   (emit-byte segment (logior (ash (logior #b11000 1) 3)
                              (reg-encoding src segment)))))

(in-package :jsoon)

(sb-c:defknown %unset-rightmost-bit ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(in-package :sb-vm)

(define-vop (jsoon::%unset-rightmost-bit)
  (:policy :fast)
  (:translate jsoon::%unset-rightmost-bit)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types positive-fixnum)
  (:results (r :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst blsr x r)))
