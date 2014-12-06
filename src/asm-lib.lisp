
(in-package :asm-lib)

(defparameter mul16-lhs-lo r16)
(defparameter mul16-lhs-hi r17)
(defparameter mul16-rhs-lo r18)
(defparameter mul16-rhs-hi r19)

(defun-asm mul16 (r16 r17 r18 r19) (r20)
	(!!! "Multiply two 16-bit numbers (r17:16 and r19:r18 and get a 16-bit result"
		 "using the Peasant's multiplication method. The pointer-return register is left alone.")

	(!! "r17:r16 is divided in half repeatedly and r19:r18 is doubled until r17:r16 is zero.")

	(clr ret-nonptr-lo)
	(clr ret-nonptr-hi)
	
	(with-labels (START CONTINUE DONE)
		(label START)
		(cp mul16-lhs-lo zero)
		(cpc mul16-lhs-hi zero)

		(lds r20 SREG)

		(sbrc r20 SREG-Z)
		(rjmp DONE)

		(!! "if LHS is odd, add RHS to the result")
		(sbrs mul16-lhs-lo 0)
		(rjmp CONTINUE)

		(add ret-nonptr-lo mul16-rhs-lo)
		(adc ret-nonptr-hi mul16-rhs-hi)

		(label CONTINUE)

		(lsr mul16-lhs-lo)
		(ror mul16-lhs-hi)

		(lsl mul16-rhs-lo)
		(rol mul16-rhs-hi)

		(rjmp START)

		(label DONE)))

(defun subi16 (reghi reglo &optional (imm nil imm-supplied))
	(if imm-supplied
		(progn
			(subi reglo (lobyte imm))
			(ldi tmp-reg (hibyte imm))
			(sbc reghi tmp-reg))
		(subi16 (register16-hi reghi) (register16-lo reghi) reglo)))

(defun-asm memcpy (r16 r17 ;; destination
				   r18 r19 ;; source
				   r20 r21);; length
	              (r22 X Y);; temporaries
	(!! "X is the destination and Y is the source")
	(movw X-hi X-lo r17 r16)
	(movw Y-hi Y-lo r19 r18)

	(loop-while (:test (nonzero (r21 r20)))
	   (subi16 r21 r20 1) ;; can't use DEC because it doesn't affect SREG(C)

	   (ld r22 Y+)
	   (st X+ r22)))
