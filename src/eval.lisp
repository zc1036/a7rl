
;;;; a7rl evaluator

(in-package :eval)

(defglobal-word dynamic-vars-ptr)
(defglobal-word current-lexical-frame)

(defconstant-symbol readtable-sym "*READTABLE*")
(defconstant-symbol plus-sym "+")
(defconstant-symbol let-sym "LET")
(defconstant-symbol set-leds-sym "SET-LEDS")

(defconstant-readtable default-readtable)

(defun-asm asm-let-mapc-body () ()
	(if-only (i= r18 1)
		(movw r17 r16 Z-hi Z-lo)
		(call asm-eval)
		(movw Z-hi Z-lo ret-ptr-hi ret-ptr-lo))
	(st Y+ Z-lo)
	(st Y+ Z-hi)
	(dec r18))

(defun-a7rl asm-let-special-form ()
	                             (Z ; memory access
								  r16 r17 ; pointer to current lexical frame
								  r18 r19 ; args
								  r20 r21 ; bindings list
								  r22 r23 ; func body
								  X  ; preserve the new lexical frame pointer
								  Y) ; memory access
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(load-imm Z current-lexical-frame)
	(ld r16 Z+)
	(ld r17 Z)
	(movw Z-hi Z-lo r17 r16)

	(!! "Get the zeroth argument (bindings) into r21:20")
	(ldi r18 0)
	(call get-lexical-variable-by-index)
	(movw r21 r20 ret-ptr-hi ret-ptr-lo)

	(!! "Get the first argument (body) into r23:22")
	(ldi r18 1)
	(call get-lexical-variable-by-index)
	(movw r23 r22 ret-ptr-hi ret-ptr-lo)

	(with-registers (r20 r21)
		(!! "r16 and r17 are unused by gc-make-lexical-frame if r21 is FUNCTION-TYPE-SPECIAL-FORM")
		(mov r18 r16)
		(mov r19 r17)
		(ldi r20 1) ; one "argument" (binding)
		(ldi r21 function-type-special-form)
		(call gc-make-lexical-frame))

	(movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)
	(movw X-hi X-lo Y-hi Y-lo)
	(adiw (list Y-hi Y-lo) 1) ; skip length

	(ldi r18 2)
	
	(asm-mapc r21 r20
		(progn
			(if-only (zero r18)
				(break-while))
			(call asm-let-mapc-body))
		(call fatal-error))

	(load-imm Y current-lexical-frame)
	(st Y+ X-lo)
	(st Y X-hi)

	(movw r17 r16 r23 r22)
	(call asm-eval)

	(load-imm Y current-lexical-frame)
	(st Y+ Z-lo)
	(st Y Z-hi))

(export-asm-func asm-let-special-form)

(defconstant-special-form asm-let asm-let-special-form 2)

(defun-a7rl asm-plus-special-form ()
	                              (Z ; memory access
								   r16 r17
								   r18
								   r20 r21
								   r22 r23)
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Load the lexical frame pointer into r16:17")
	(load-imm Z current-lexical-frame)
	(ld r16 Z+)
	(ld r17 Z)

	(!! "Get the zeroth argument")
	(ldi r18 0)
	(call get-lexical-variable-by-index)
	(movw r23 r22 ret-ptr-hi ret-ptr-lo)

	(!! "Get the first argument")
	(ldi r18 1)
	(call get-lexical-variable-by-index)

	(!! "Evaluate the first argument")
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(call asm-eval)
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)

	(!! "Check type of LHS")
	(ldi r18 fixnum-object-id)
	(call assert-type)

	(!! "Load LHS into r21:20")
	(movw Z-hi Z-lo r17 r16)
	(adiw (list Z-hi Z-lo) 1) ;; skip header
	(ld-gcptr r20 Z+)
	(ld-gcptr r21 Z)

	(!! "Evaluate the zeroth argument")
	(movw r17 r16 r23 r22)
	(call asm-eval)
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)

	(!! "Check the type of the zeroth argument")
	(call assert-type)

	(!! "Load the RHS into r17:16")
	(movw Z-hi Z-lo r17 r16)
	(adiw (list Z-hi Z-lo) 1)
	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z)

	(add r16 r20)
	(adc r17 r21)

	(call gc-make-fixnum))

(export-asm-func asm-plus-special-form)

(defconstant-special-form asm-plus asm-plus-special-form 2)

(defun-a7rl asm-set-leds-special-form ()
	                                  (Z ; memory access
									   r16 r17
									   r18)
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Load the lexical frame pointer into r16:17")
	(load-imm Z current-lexical-frame)
	(ld r16 Z+)
	(ld r17 Z)

	(ldi r18 0)
	(!! "Get the zeroth argument")
	(call get-lexical-variable-by-index)
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)

	(!! "Evaluate the zeroth argument")
	(call asm-eval)
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)

	(!! "Check the type of the zeroth argument")
	(ldi r18 fixnum-object-id)
	(call assert-type)

	(!! "Load the RHS into r17:16")
	(movw Z-hi Z-lo r17 r16)
	(adiw (list Z-hi Z-lo) 1) ; skip header
	(ld-gcptr r16 Z+)

	(out PORTA r16)

	(clear-reg ret-ptr-hi ret-ptr-lo))

(export-asm-func asm-set-leds-special-form)

(defconstant-special-form asm-set-leds asm-set-leds-special-form 1)

(defun-a7rl eval-init ()
	                  (r16 r17 r18 r19 ; args
					   Y)              ; tmp
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Configure LEDs")

	(ldi r16 7)
    (out DDRA r16)

	(!! "Create the list of initial bindings")

	(! "*readtable*")
	(load-imm (list r17 r16) readtable-sym-gcptr)
	(load-imm (list r19 r18) default-readtable-gcptr)

	(call gc-make-cons)

	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(clear-reg r18 r19)

	(call gc-make-cons)

	(movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)

	(load-imm (list r17 r16) set-leds-sym-gcptr)
	(load-imm (list r19 r18) asm-set-leds-gcptr)

	(call gc-make-cons)

	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(movw r19 r18 Y-hi Y-lo)

	(call gc-make-cons)

	(movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)

	(load-imm (list r17 r16) plus-sym-gcptr)
	(load-imm (list r19 r18) asm-plus-gcptr)

	(call gc-make-cons)

	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(movw r19 r18 Y-hi Y-lo)

	(call gc-make-cons)

	(movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)

	(load-imm (list r17 r16) let-sym-gcptr)
	(load-imm (list r19 r18) asm-let-gcptr)

	(call gc-make-cons)

	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(movw r19 r18 Y-hi Y-lo)

	(call gc-make-cons)

	(movw r17 r16 ret-ptr-hi ret-ptr-lo)

	(load-imm Y dynamic-vars-ptr)

	(st Y+ r16)
	(st Y r17)

	(!! "Zero out the current lexical frame")
	(load-imm Y current-lexical-frame)
	(st Y+ zero)
	(st Y zero))

(push eval-init a7rl:init-funcs)

(defun-asm check-obj-types-equal (r16 r17 ; lhs
								  r18 r19 ; rhs
								  r20)    ; the object type
	                             (Z)      ; for loading from memory
	(!!! "If the arguments have the same type and that type is r20,"
		 "RET-NONPTR-LO will be nonzero; otherwise, it will be zero.")
	(clear-reg ret-nonptr-hi ret-nonptr-lo)

	(!! "Load the headers of the objects into r16 and r17")
	(movw Z-hi Z-lo r17 r16)
	(ld-gcptr r16 Z)

	(movw Z-hi Z-lo r19 r18)
	(ld-gcptr r17 Z)

	(!! "Make sure it's the same type of object, and that they're symbols.")
	(! "Mask off the program/sram tag")
	(andi r16 object-type-mask)
	(andi r17 object-type-mask)

	(if-only (= r16 r17)
		(if-only (= r16 r20)
			(inc ret-nonptr-lo))))

(defun-asm symbol-compare (r16 r17  ; lhs (pointer to symbol)
						   r18 r19) ; rhs (pointer to symbol)
						  (r20 r21 r22 Z) ; args, iterators, temps
	(!!! "Compares two symbols for equality (doesn't check type); if they are equal, RET-NONPTR-LO"
		 "is nonzero; else, it is zero.")

	(clear-reg ret-nonptr-lo ret-nonptr-hi)

	(if-only (zero (r17 r16))
		(ret))
	(if-only (zero (r19 r18))
		(ret))

	(!! "Check that lengths are equal")
	(movw Z-hi Z-lo r17 r16)

	(! "Skip the header byte")
	(adiw (list Z-hi Z-lo) 1)
	(ld-gcptr r20 Z+)
	(movw r17 r16 Z-hi Z-lo)

	(movw Z-hi Z-lo r19 r18)
	(adiw (list Z-hi Z-lo) 1)
	(ld-gcptr r21 Z+)
	(movw r19 r18 Z-hi Z-lo)

	(! "Ensure the lengths are equal, and if so, compare each byte in the representation for equality")
	(if-only (= r20 r21)
		(loop-while (:test (nonzero r20))
		   (movw Z-hi Z-lo r17 r16)
		   (ld-gcptr r21 Z+)
		   (movw r17 r16 Z-hi Z-lo)

		   (movw Z-hi Z-lo r19 r18)
		   (ld-gcptr r22 Z+)
		   (movw r19 r18 Z-hi Z-lo)
		   (if-only (!= r21 r22)
			   (break-while))
		   (dec r20)))

	(if-only (zero r20)
		(inc ret-nonptr-lo)))

(defun-asm search-list-symbol-callback (r16 r17  ; pointer to current (symbol . binding) pair
										r18 r19) ; callback argument, pointer to the symbol to search for
	                                   (r20 Z)   ; argument, temp
	(!! "Load the CAR of the CONS into r17:r16 (we increment the pointer first to skip the header byte)")
	(movw Z-hi Z-lo r17 r16)
	(adiw (list Z-hi Z-lo) 1)

	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z+)
	
	(ldi r20 symbol-object-id)

	(!! "If CHECK-OBJ-TYPES-EQUAL and SYMBOL-COMPARE return nonzero"
		"then load the CDR of the CONS into RET-PTR-HI:LO and return")
	(call check-obj-types-equal)

	(if-only (nonzero ret-nonptr-lo)
		(call symbol-compare)
		(if-only (nonzero ret-nonptr-lo)
			(ld-gcptr ret-ptr-lo Z+)
			(ld-gcptr ret-ptr-hi Z))))

(export-asm-func search-list-symbol-callback)

(defun-asm get-dynamic-binding (r16 r17) ; symbol to look for
	                           (r18 r19  ; arg to search-list (callback arg)
								r20 r21  ; arg to search-list (predicate function)
								X)       ; holds ptrs
	(clear-reg ret-ptr-hi ret-ptr-lo ret-nonptr-hi ret-nonptr-lo)

	(!! "Put the symbol to search for into the callback arg position")
	(movw r19 r18 r17 r16)

	(!! "Put *DYNAMIC-VARS-PTR into the list arg position")
	(load-imm X dynamic-vars-ptr)
	(ld r16 X+)
	(ld r17 X+)

	(!! "Put the callback into position")
	(load-imm (list r21 r20) search-list-symbol-callback)
	(call search-list))

(defun-asm asm-eval-to-self (r16 r17) ; the object to evaluate
	                        ()
	(!!! "Return the argument")
	(movw ret-ptr-hi ret-ptr-lo r17 r16))

(defun-asm assert-type (r16 r17 ; the object
					    r18)    ; the type
	                   (Z)      ; temps
	(!!! "Fatal error if object is not of the specified type or is null;"
		 "returns a pointer to after the header byte")

	(if-only (zero (r17 r16))
		(call fatal-error))
	
	(movw Z-hi Z-lo r17 r16)
	(ld-gcptr r16 Z+)
	(if-only (!= r18 r16)
		(call fatal-error))
	(movw ret-ptr-hi ret-ptr-lo Z-hi Z-lo))

(defun-a7rl asm-execute-func-with-lexical-frame (r16 r17  ; the function object to execute
							                     r18 r19) ; the lexical frame to use
	                                            (Z        ; memory access
												 r20 r21  ; preserve the current lexical frame
												 r22)     ; the function type

	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Set the current lexical frame to this one (preserving the old one in r21:r20)")
	(load-imm Z current-lexical-frame)
	(ld r20 Z+)
	(ld r21 Z)
	(load-imm Z current-lexical-frame)
	(st Z+ r18)
	(st Z r19)

	(movw Z-hi Z-lo r17 r16)

	(! "Skip the header, and # of arguments")
	(adiw (list Z-hi Z-lo) 2)

	(! "load the function type into r22")
	(ld-gcptr r22 Z+)

	(!! "Load the pointer to the function body into Z")
	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z)

	(movw Z-hi Z-lo r17 r16)

	(!! "If this is a special form, invoke it with ICALL; else, skip the arg forms.")
	(if-else (i= r22 function-type-special-form)
		(call-gcptr)
		(progn
			(! "Skip the header and CAR")
			(adiw (list Z-hi Z-lo) 3)

			(!! "Load the CDR (function body) into r17:16")
			(ld-gcptr r16 Z+)
			(ld-gcptr r17 Z)

			(!! "Evaluate each of the body forms if this is a function or macro")
			(asm-mapc r17 r16
				(progn
					(movw r17 r16 Z-hi Z-lo)
					(call asm-eval))
				(call fatal-error)))) ;; we error if the function body is an improper list

	(!! "Restore the old lexical frame")
	(load-imm Z current-lexical-frame)
	(st Z+ r20)
	(st Z r21))

(defun-a7rl asm-store-func-args (r16 r17  ; Argument list
								 r18 r19  ; lexical frame to fill
								 r20      ; arg count
								 r21)     ; function type
	                            (X)       ; Lexical frame iterator
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(movw X-hi X-lo r19 r18)
	(adiw (list X-hi X-lo) 1) ; skip the arg count since it's an argument

	(!! "Evaluate each argument in succession, storing each result in "
		"the lexical frame (fatal error on improper lists)")
	(asm-mapc r17 r16
		(progn
			(if-only (i= r20 0)
				(break-while))
			(call asm-store-func-args-mapc-body)) ;; this is a function call because it's too big for breq
		(call fatal-error)))

(defun-asm asm-store-func-args-mapc-body () ()
	(dec r20)
	(! "Skip the symbol pointers")
	(adiw (list X-hi X-lo) 2)
	(movw r17 r16 Z-hi Z-lo)

	(!! "Evaluate the argument if this is a normal function")
	(if-only (i= r21 function-type-func)
		(call asm-eval)
		(movw r17 r16 ret-ptr-hi ret-ptr-lo))

	(st X+ r16)
	(st X+ r17))

(defun-a7rl asm-eval-cons (r16 r17) ; the cons to evaluate
	                      (Y        ; preserve registers
						   Z        ; memory access
						   r18 r19  ; arguments
						   r20      ; temps
						   r21 r22 r23) ; preserve registers
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!!! "Evaluate a cons")

	(!! "Skip the header byte")
	(movw Z-hi Z-lo r17 r16)
	(adiw (list Z-hi Z-lo) 1)

	(!! "Evaluate the CAR and ensure it's a function")
	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z+)

	(call asm-eval)

	(!! "Assert that we got a function")
	(movw r17 r16 ret-ptr-hi ret-ptr-lo)
	(ldi r18 function-object-id)
	(call assert-type)

	(!! "Save the pointer to the function")
	;;(movw r22 r21 r17 r16)
	(mov r22 r17)
	(mov r21 r16)

	(!! "Create a stack frame with the given size and container stack frame")
	(! "Save Z")
	(movw Y-hi Y-lo Z-hi Z-lo)
	;;(movw Z-hi Z-lo r22 r21)
	(mov Z-hi r22)
	(mov Z-lo r21)
	(adiw (list Z-hi Z-lo) 1) ; skip header
	(ld-gcptr r20 Z+) ; # of arguments
	(ld-gcptr r23 Z+) ; type of function
	(ld-gcptr r16 Z+) ; pointer to function list (lo)
	(ld-gcptr r17 Z+) ; pointer to function list (hi)
	(ld-gcptr r18 Z+) ; pointer to lexical closure (lo)
	(ld-gcptr r19 Z)  ; pointer to lexical closure (hi)

	(!! "If this is a special form, we don't create a clean scope for it.")
	(if-only (i= r23 function-type-special-form)
		(load-imm Z current-lexical-frame)
		(ld-gcptr r18 Z+)
		(ld-gcptr r19 Z))
	
	(movw Z-hi Z-lo Y-hi Y-lo)

	(with-registers (r21)
		(mov r21 r23)
		(call gc-make-lexical-frame))

	(movw r19 r18 ret-ptr-hi ret-ptr-lo)

	(!! "At this point: r19:18 = ptr to new lexical frame (is an argument to ASM-EXECUTE-FUNC-WITH-LEXICAL-FRAME)"
		"                                                  and ASM-STORE-FUNC-ARGS)"
		"               r17:16 = ptr to function definition (CAR has arg list)"
		"               r22:21 = pointer to the function object"
		"               r20    = number of arguments (from the definition) (is argument to ASM-EXECUTE...)"
		"               Z      = ptr to first cons of the list of arguments in the CALL"
		"               r23    = function type")

	(!! "Fill the lexical frame with arguments")
	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z)
	(!! "r19:18 already has lexical frame, r20 already has the arg count in it")
	(mov Y-lo r21) ; preserve Y-lo
	(mov r21 r23)  ; function type argument
	(call asm-store-func-args)
	(mov r21 Y-lo)

	(! "Arg to execution: pointer to function object")
	;;(movw r17 r16 r22 r21)
	(mov r17 r22)
	(mov r16 r21)

	(!! "r19:18 already contains the pointer to the lexical frame; r20 has arg count")
	(call asm-execute-func-with-lexical-frame))

(defun-a7rl asm-eval-symbol (r16 r17)       ; symbol to evaluate
                            (Z r18 r19 r20) ; temps, args, counter
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)
	(!!! "Searches lexical bindings, then dynamic bindings, for a given symbol.")
	(load-imm Z current-lexical-frame)

	(!! "Load the pointer to the current lexical frame into Z")
	(ld r18 Z+)
	(ld r19 Z)

	(movw Z-hi Z-lo r19 r18)

	(!! "Start iteration of the linked list of frames")
	(with-labels (done-searching)
		(loop-while (:test (nonzero (Z-hi Z-lo)))
	    	(ld-gcptr r20 Z+)

			(loop-while (:test (nonzero r20))
				(ld-gcptr r18 Z+)
				(ld-gcptr r19 Z+)

				(call symbol-compare)

				(if-only (nonzero ret-nonptr-lo)
					(ld-gcptr ret-ptr-lo Z+)
					(ld-gcptr ret-ptr-hi Z+)
					(rjmp done-searching))

				(adiw (list Z-hi Z-lo) 2)
				(dec r20))

			(!! "Load the next pointer into Z")
			(ld-gcptr r18 Z+)
			(ld-gcptr r19 Z)
			(movw Z-hi Z-lo r19 r18))

		(label done-searching))

	(if-only (zero r20)
		(call get-dynamic-binding)))

(export-asm-func asm-eval-symbol)
(export-asm-func asm-eval-cons)
(export-asm-func asm-eval-to-self)

(defconstant-ptr-array eval-type-dispatch-table
	(asm-eval-to-self   ; number
	 asm-eval-cons
	 asm-eval-symbol
	 asm-eval-to-self   ; string
	 asm-eval-to-self   ; readtable
	 asm-eval-to-self   ; array
	 asm-eval-to-self)) ; function

(defun-a7rl asm-eval (r16 r17) ; the form to evaluate
	                 (r18 r19 X Z)   ; temp, args, memory access
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Check the type and dispatch on it")
	(movw X-hi X-lo r17 r16)
	(load-imm (list r17 r16) eval-type-dispatch-table-gcptr)
	(ld r18 X)
	(clear-reg r19)
	(call array-index)

	(movw r17 r16 X-hi X-lo)
	(movw Z-hi Z-lo ret-ptr-hi ret-ptr-lo)
	(call-gcptr))
