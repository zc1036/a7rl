
;;;; Driver code for a7rl

(in-package :main)

(definterrupt lavrock:reset main
	(clr zero)

	(out SREG zero)

	(!! "Enable sleep")
	(ldi r16 #x20)
	(out MCUCR r16)

	(!! "Set up the stack")
	(ldi r16 (lobyte ramend))
	(out spl r16)
	(ldi r16 (hibyte ramend))
	(out sph r16)

	(!! "Call initialization functions")

	(dolist (func (nreverse init-funcs))
		(call func))

	(call asm-read)
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(call asm-eval)

	(call fatal-error))
