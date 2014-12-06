
;;;; A7RL garbage collector

(in-package :gc)

;; Evaluates to the second value of a form that evaluates to two or more values.
(defmacro second-value (form)
	(let ((a-sym (gensym)) (b-sym (gensym)))
		`(multiple-value-bind (,a-sym ,b-sym) ,form
			 (declare (ignore ,a-sym))
			 ,b-sym)))

;; (in-file "a7rl.asm" (make-vm :mode :output) t

(defregister-synonym gc-heap-ptr-lo r0)
(defregister-synonym gc-heap-ptr-hi r1)

;; CEILING's remainder is negative, so by subtracting, we are rounding up to the nearest multiple of 4.
;; We do this because objects must be aligned on a 4-byte boundary.
(defconstant-asm heap-start (- $rambegin (second-value (ceiling $rambegin 4))))
(defconstant-asm heap-end (+ 1 $rambegin (floor (- $ramend $rambegin) 2))) ;; + 1 because ramend is inclusive

(defconstant-asm relocation-bucket-count (ceiling (- $heap-end $heap-start) 4))

(defconstant-asm heap-relocation-buckets-begin heap-end)
(defconstant-asm heap-relocation-buckets-end (+ $heap-end ;; the # of bits per bucket * # of buckets divided by 8 to get bytes
												(ceiling (* (ceiling (log $relocation-bucket-count 2))
															$relocation-bucket-count)
														 8)))

(defconstant-asm globals-begin heap-relocation-buckets-end)

(defparameter globals-end $globals-begin)

(defun allocate-global (size)
	(let ((old-globals-end globals-end))
		(setf globals-end (+ globals-end size))
		old-globals-end))

(defmacro defglobal-word (name)
	`(defconstant-asm ,name (allocate-global 2)))

(defmacro defglobal-byte (name)
	`(defconstant-asm ,name (allocate-global 1)))

(defun-asm gc-init () (r16)
	(ldi r16 (lobyte heap-start))
	(mov gc-heap-ptr-lo r16)
	(ldi r16 (hibyte heap-start))
	(mov gc-heap-ptr-hi r16))

(push gc-init a7rl:init-funcs)

;; Allocates the given number of 4-byte blocks on the heap.
;; The argument is passed in r16-r17 (r16 is the LSB).
(defun-a7rl gc-alloc (r16 r17)
	                 (r18 r19  ;; temporaries for the current end of the heap
		              r20 r21) ;; temporaries for the current beginning of the heap

	(declare-stack-usage :non-pointer-bytes preserved-register-byte-count)

	(!! "Multiply the number by four to get our byte count")
	(left-shift-n (r17 r16) 2)

	(!! "Preserve the old value of the heap pointer in r20:r21 so we can return it")
	(movw r21 r20 gc-heap-ptr-hi gc-heap-ptr-lo)

	(!! "Add the byte count to the heap pointer")
	(add r16 gc-heap-ptr-lo)
	(adc r17 gc-heap-ptr-hi)

	(!! "load a pointer to the end of the heap into registers")
	(ldi r18 (lobyte heap-end))
	(ldi r19 (hibyte heap-end))

	(!! "stall if we're out of memory")
	(if-only (>= (r17 r16) (r19 r18))
		(jmp fatal-error))
	
	(!! "Make the heap pointer point to the new end of the heap")
	(movw gc-heap-ptr-hi gc-heap-ptr-lo r17 r16)

	(!! "Return a pointer to the allocated memory")
	(movw ret-ptr-hi ret-ptr-lo r21 r20))

(defun-a7rl gc-make-symbol (r16 r17  ; pointer to the symbol representation
							r18)     ; length of the symbol    
	                       (r19      ; arg space, tmp
							r20 r21  ; arg space, tmp
							X)       ; tmp pointer

	(declare-stack-usage :non-pointer-bytes preserved-register-byte-count)

	(with-registers (r16 r17)
		(!! "Round the length up to the nearest word (w/ at least 2 extra bytes) by adding five and"
			"dividing by 4 (gc-alloc wants count in blocks of 4 bytes) and allocate that sucker")
		(clr r17)
		(ldi r16 5)
		(add r16 r18)
		(adc r17 zero)
		(right-shift-n (r17 r16) 2)
		(call gc-alloc))

	(!! "Store the object header and length in the allocated space")
	(movw X-hi X-lo ret-ptr-hi ret-ptr-lo)
	(ldi r19 symbol-object-id)
	(st X+ r19)
	(st X+ r18)

	(!! "memcpy the contents over")
	(! "arg 3/3 to memcpy: length")
	(mov r20 r18)
	(clr r21)

	(! "arg 2/3 to memcpy: source")
	(movw r19 r18 r17 r16)

	(! "arg 1/3 to memcpy: destination")
	(movw r17 r16 X-hi X-lo)

	(call memcpy)

	(!! "Undo the previous adding of 2 bytes")
	(subi16 X-hi X-lo 2)

	(movw ret-ptr-hi ret-ptr-lo X-hi X-lo))

(defun-a7rl gc-make-cons (r16 r17  ;; car
						  r18 r19) ;; cdr
	                     (X r20)   ;; tmp

	(declare-stack-usage :non-pointer-bytes preserved-register-byte-count)

	(with-registers (r16 r17)
		(! "We want two words of memory (we only use five bytes)")
		(ldi r16 2)
		(clr r17)
		(call gc-alloc))

	(movw X-hi X-lo ret-ptr-hi ret-ptr-lo)

	(ldi r20 cons-object-id)
	(st X+ r20)
	(st X+ r16)
	(st X+ r17)
	(st X+ r18)
	(st X+ r19)

	(subi16 X 5)
	(movw ret-ptr-hi ret-ptr-lo X-hi X-lo))

(defun-a7rl gc-make-fixnum (r16 r17) ; the number to allocate
	                       (r18 X)   ; tmps
	(declare-stack-usage :non-pointer-bytes preserved-register-byte-count)

	(with-registers (r16 r17)
		(! "We want one word of memory (we only use three bytes)")
		(ldi r16 1)
		(clr r17)
		(call gc-alloc))

	(movw X-hi X-lo ret-ptr-hi ret-ptr-lo)

	(ldi r18 fixnum-object-id)
	(st X+ r18)
	(st X+ r16)
	(st X+ r17)

	(subi16 X 3)
	(movw ret-ptr-hi ret-ptr-lo X-hi X-lo))

(defun-a7rl gc-make-lexical-frame (r16 r17  ; pointer to function definition
								   r18 r19  ; pointer to program lexical closure stack frame
								   r20      ; # of arguments
								   r21)     ; type of function
	                              (Y        ; used to write to newly allocated lexical frame
								   Z        ; used to load from program memory
								   r22 r23) ; the byte length for pointer/pointer pairs
	(declare-stack-usage :non-pointer-bytes preserved-register-byte-count)

	(if-only (i!= r21 function-type-special-form)
		(!! "Put the CAR of the given function body (the argument list) into r17:16")
		(movw Z-hi Z-lo r17 r16)
		(adiw (list Z-hi Z-lo) 1)
		(ld-gcptr r16 Z+)
		(ld-gcptr r17 Z+))

	(with-registers (r16 r17 r20)
		(!! "We want at least 4 * n + 3 bytes")
		(clr r17)
		(left-shift-n (r17 r20) 2)
		(mov r23 r17)
		(mov r22 r20)
		(ldi r16 (+ 3 5)) ;; add five to 3 to do the rounding thing
		(add r16 r20)
		(adc r17 zero)
		(right-shift-n (r17 r16) 2)
		(call gc-alloc))

	(movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)

	(with-registers (Y)
		(with-registers (r18)
			(!! "Store the length")
			(st Y+ r20)

			(!! "If this is a special form, just zero-out the memory;"
				"else, fill it with the argument names.")
			(if-else (i= r21 function-type-special-form)
				(loop-while (:test (nonzero (r23 r22)))
				   (st Y+ zero)
				   (subi r22 1)
				   (sbc r23 zero))
				(progn
					(ldi r18 symbol-object-id)

					(!! "Store the argument names")
					(asm-mapc r17 r16
						(progn
							(movw r17 r16 Z-hi Z-lo)
							(call eval:assert-type)

							(st Y+ Z-lo)
							(st Y+ Z-hi)
							(st Y+ zero)
							(st Y+ zero))))))

		(!! "Store the pointer to the next frame")
		(st Y+ r18)
		(st Y+ r19))

	(movw ret-ptr-hi ret-ptr-lo Y-hi Y-lo))
