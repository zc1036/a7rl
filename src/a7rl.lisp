
;;;; The "driver" program for a7rl

(in-package :a7rl)

(defparameter init-funcs ())

(defparameter ret-nonptr-lo r8)
(defparameter ret-nonptr-hi r9)
(defparameter ret-ptr-lo r10)
(defparameter ret-ptr-hi r11)

(defparameter tmp-reg-lo r24)
(defparameter tmp-reg-hi r25)
(defparameter tmp-reg r25)

(defparameter ret-nonptr (list ret-nonptr-hi ret-nonptr-lo))
(defparameter ret-ptr (list ret-ptr-hi ret-ptr-lo))

(defun declare-stack-usage (&rest rest)
	"A dummy function to ensure this declaration is only used in the
	preamble of an a7rl function."

	(declare (ignore rest))

	(error "DECLARE-STACK-USAGE must appear in the preamble (first lines) of an a7rl assembly function."))

(defun process-declare-stack-usage (declaration)
	(destructuring-bind (&key (pointer-bytes 0) (non-pointer-bytes 0)) (cdr declaration)
		(values
		 `(progn ;; preamble
			  (assert (eql 0 (rem ,pointer-bytes 2))
					  ()
					  "POINTER-BYTES must be a multiple of 2 because pointers are two bytes each")

			  (!! (format nil "Declare stack frame size (~a bytes for non-lisp-pointers and ~a for lisp pointers)"
						  ,non-pointer-bytes ,pointer-bytes))

			  (dolist (section-size (list ,non-pointer-bytes ,pointer-bytes))
				  (ldi tmp-reg section-size)
				  (pushreg tmp-reg))

			  (!! #|end of stack frame declaration code|#))
		 `(progn ;; epilogue
			  (!! "Pop stack frame size markers")
			  (popreg tmp-reg)
			  (popreg tmp-reg)))))

(defmacro with-stack-space (reghi reglo bytes &body body)
	`(progn
		 (in tmp-reg-lo SPL)
		 (in tmp-reg-hi SPH)
		 (subi tmp-reg-lo ,bytes)
		 (sbc tmp-reg-hi zero)
		 (mov ,reglo tmp-reg-lo)
		 (mov ,reghi tmp-reg-hi)
		 (subi tmp-reg-lo 1)
		 (sbc tmp-reg-hi zero)
		 (out SPL tmp-reg-lo)
		 (out SPH tmp-reg-hi)

		 ,@body

		 (ldi tmp-reg-hi ,bytes)
		 (inc tmp-reg-hi)
		 (in tmp-reg-lo SPL)
		 (add tmp-reg-lo tmp-reg-hi)
		 (out SPL tmp-reg-lo)
		 (in tmp-reg-lo SPH)
		 (adc tmp-reg-lo zero)
		 (out SPH tmp-reg-lo)))

(defun ld-gcptr (dst src)
	(check-type dst register)
	(check-type src register16)

	;; if the high-bit is set, this is a program mem pointer; use lpm
	(with-labels (LOAD-SRAM-GCPTR DONE-LOAD-GCPTR)
		(sbrs Z-hi 7)
		(rjmp LOAD-SRAM-GCPTR)
		(andi Z-hi #b01111111) ;; take the 1 away
		(lpm dst src) ;; we use src and not Z in case the user wants increment
		(ori Z-hi #b10000000) ;; put the 1 back
		(rjmp DONE-LOAD-GCPTR)
		(label LOAD-SRAM-GCPTR)
		(ld dst src)
		(label DONE-LOAD-GCPTR)))

(defun call-gcptr ()
	;; if the high-bit is set, 
	(with-labels (CALL-LISP CALL-DONE)
		(sbrs Z-hi 7)
		(rjmp CALL-LISP)
		(andi Z-hi #b01111111)
		(icall)
		(ori Z-hi #b10000000)
		(rjmp CALL-DONE)
		(label CALL-LISP)
		(call fatal-error)
		(label CALL-DONE)))

(defun process-func-body (func-body)
	(if (and (consp (car func-body)) (eq (caar func-body) 'declare-stack-usage))
		(elet ((preamble epilogue (process-declare-stack-usage (car func-body))))
			(values preamble epilogue (cdr func-body)))
		func-body))

(defmacro defun-a7rl (name args locs &body body)
	(elet ((preamble-forms epilogue-forms processed-body (process-func-body body)))
		`(let ((asm-func-preamble-hook (λ () ,preamble-forms))
			   (asm-func-epilogue-hook (λ () ,epilogue-forms)))
			 (defun-asm ,name ,args ,locs ,@processed-body))))

(in-file "a7rl.asm" (make-vm :mode :output) t
	(codegen:defregister-synonym lavrock:zero r6)
	(codegen:defregister-synonym ret-nonptr-lo ret-nonptr-lo)
	(codegen:defregister-synonym ret-nonptr-hi ret-nonptr-hi)
	(codegen:defregister-synonym ret-ptr-lo ret-ptr-lo)
	(codegen:defregister-synonym ret-ptr-hi ret-ptr-hi)

	(in-package :a7rl-asm-file)

	(let ((r25 nil))
		(load "~/code/a7rl/src/object-repr.lisp")
		(load "~/code/a7rl/src/gc.lisp")
		(load "~/code/a7rl/src/asm-arrays.lisp")
		(load "~/code/a7rl/src/asm-lists.lisp")
		(load "~/code/a7rl/src/reader.lisp")
		(load "~/code/a7rl/src/eval.lisp")
		(load "~/code/a7rl/src/main.lisp"))

	(in-package :a7rl))
