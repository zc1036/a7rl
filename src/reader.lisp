
;;;; a7rl reader

(in-package :reader)

(defconstant-bytes program " (SET-LEDS (LET (X 2) (LET (Y 2) (+ X Y)))) ")

(defglobal-word read-ptr) ;; READ-PTR is the address of a pointer, so it's like a CHAR**
(defglobal-byte unput-char)
(defglobal-byte use-unput-char)

(defun-asm reader-init () (X ;; stores a pointer to READ-PTR
						   r16 ;; temporary that holds the bytes of PROGRAM and temporaries, and arguments
						   r17)
	(!! "Initialize READ-PTR with PROGRAM (shifted over one because program memory stores two bytes per address)")
	(load-imm X read-ptr)

	(ldi r16 (lobyte (lshifti program 1)))
	(st X+ r16)
	(ldi r16 (hibyte (lshifti program 1)))
	(st X r16)

	(!! "Set USE-UNPUT-CHAR to false")
	(sts use-unput-char zero))

(push reader-init a7rl:init-funcs)

(defun-asm asm-read-char () (r16 ;; temporaries
							 X   ;; holds READ-PTR
							 Z)  ;; holds *READ-PTR
	(!!! "Reads a character and returns it in RET-NONPTR-LO; RET-NONPTR-HI is 0 if EOF, nonzero otherwise")

	(clr ret-nonptr-hi)

	(!! "Load USE-UNPUT-CHAR to check if we should return an unputted character")
	(lds r16 use-unput-char)

	(!! "If we should, load UNPUT-CHAR into the lo return byte and 1 into the hi byte"
		"to indicate success")
	(if-only (nonzero r16)
		(sts use-unput-char zero)
		(lds ret-nonptr-lo unput-char)
		(inc ret-nonptr-hi)
		(ret))

	(!!! "TODO: Check if we are at the end of PROGRAM")

	(!! "Load a pointer to a pointer to the current byte in PROGRAM to X")
	(load-imm X read-ptr)

	(!! "Load a pointer to the current byte in PROGRAM to Z")
	(ld Z-lo X+)
	(ld Z-hi X)

	(!! "Load the current byte in PROGRAM to the lo-byte of the return registers,"
		"and put 1 into the hi-byte to indicate success")

	(!! "Increment Z so it points to the next byte")
	(lpm ret-nonptr-lo Z+)
	(inc ret-nonptr-hi)

	(!! "Store Z, a pointer to the next byte, to X, which points to READ-PTR")
	(st X Z-hi)
	(st -X Z-lo))

(defun-asm asm-peek-char () (r16 r17) ;; temps
	(call asm-read-char)

	(!! "Preserve the return value of asm-read-char and unput the char if needed")
	(movw r17 r16 ret-nonptr-hi ret-nonptr-lo)
	(if-only (nonzero ret-nonptr-hi)
			 (call asm-unput-char))
	(movw ret-nonptr-hi ret-nonptr-lo r17 r16))

(defun-asm asm-unput-char (r16) ()
	(!!! "Like C's unputc; unputs the character in r16.")
	(sts unput-char r16)

	(ldi r16 1)
	(sts use-unput-char r16))

(defun-asm is-char-numeric (r16) ()
	(!!! "Returns nonzero in RET-NONPTR-HI if r16 is a numeric character, 0 otherwise."
		 "If RET-NONPTR-HI is nonzero, RET-NONPTR-LO contains the character as a number.")
	(clr ret-nonptr-hi)
	
	(!! "Subtract 0x30 because 0x30 is ASCII 0; if r16 ends up less than 0, it wasn't in the range [0x30, 0x39)")
	(subi r16 #x30)

	(if-only (negative r16)
		(ret))

	(!! "Subtract 10 and if the number is now negative, it was in the range [0x30, 0x3A) and is a number")
	(subi r16 10)

	(if-only (negative r16)
		(inc ret-nonptr-hi)
		(mov ret-nonptr-lo r16)
		(!! "add back the 10 we subtracted above")
		(ldi tmp-reg 10)
		(add ret-nonptr-lo tmp-reg)))

(defun-asm is-char-symchar (r16) ;; the char to test
	                       (r17) ;; temporary
	(!!! "Returns nonzero in RET-NONPTR-HI if r16 is a symbol-char, 0 otherwise.")
	(clr ret-nonptr-hi)

	(with-labels (is-sym is-not-sym)
		(!! "Test for closing parentheses")
		(if-only (i= r16 (char-code #\)))
			(rjmp is-not-sym))

		(!! "Test for ASCII range [33, 97)")
		(subi r16 33)
		(! "If r16 is negative, it's too low")
		(brmi is-not-sym)
		(subi r16 (- 97 33))
		(! "If r16 is negative, it's good")
		(brmi is-sym)

		(!! "Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)")
		(subi r16 (- 123 97))
		(brmi is-not-sym)
		(subi r16 4)
		(brmi is-sym)

		(rjmp is-not-sym)

		(label is-sym)
		(inc ret-nonptr-hi)
		(label is-not-sym)))

(defun-asm get-char-readtable-index (r16) ;; the char to test
	                                (r17 r18) ;; temporary
	(!!! "Returns nonzero in RET-NONPTR-HI if r16 is a character in the readtable, 0 otherwise."
		 "If RET-NONPTR-HI is nonzero, RET-NONPTR-LO contains the readtable index at which"
		 "this character's read-function resides.")
	(clr ret-nonptr-hi)

	(with-labels (is-sym is-not-sym)
		(!! "Test for tab")
		(ldi r17 9) 
		(if-only (= r16 r17)
				 (ldi r17 0)
				 (rjmp is-sym))

		(!! "Test for carriage return")
		(ldi r17 10)
		(if-only (= r16 r17)
				 (ldi r17 1)
				 (rjmp is-sym))
		
		(!! "Test for line feed")
		(ldi r17 13)
		(if-only (= r16 r17)
				 (ldi r17 1)
				 (rjmp is-sym))

		(!! "Test for ASCII range [32, 97) (save r16 - 30 in r17 in case it's in this range)")
		(subi r16 30)
		(mov r17 r16)
		(subi r16 (- 32 30)) ;; subi does affect the N and Z registers
		(! "If r16 is negative, it's too low")
		(brmi is-not-sym)
		(subi r16 65)
		(! "If r16 is negative, it's good")
		(brmi is-sym)

		(!! "Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)")
		(subi r16 (- 123 97))
		(brmi is-not-sym)
		(ldi r17 67)
		(add r17 r16)
		(subi r16 4)
		(brmi is-sym)
		(rjmp is-not-sym)

		(label is-sym)
		(inc ret-nonptr-hi)
		(mov ret-nonptr-lo r17)
		(label is-not-sym)))

(defun-a7rl asm-read-number () (r16 r17  ;; holds arguments and read bytes
								r18 r19  ;; holds the return value while we build it
								r20 r21) ;; holds the multiplier
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!!! "Reads a number from PROGRAM and returns a GC pointer to it in the pointer return registers."
		 "RET-NONPTR-LO is nonzero for success, zero for failure.")

	(clr r18)
	(clr r19)
	(ldi r20 1)
	(clr r21)

	(!! "We mustn't call any GC-calling functions in this loop because we modify the stack in it.")
	(loop-while (:call (call asm-read-char)
				 :test (nonzero ret-nonptr-hi))
	   (! "argument to is-char-numeric")
	   (mov r16 ret-nonptr-lo)
	   (call is-char-numeric)
	   (if-else (nonzero ret-nonptr-hi)
		   (progn
			   (!! "The character in r16 is numeric; multiply its decimal value by 10 raised to the digit position")
			   (with-registers (r18 r19)
				   (mov r16 r20)
				   (mov r17 r21)
				   (mov r18 ret-nonptr-lo)
				   (clr r19)
				   (call mul16))

			   (add r18 ret-nonptr-lo)
			   (adc r19 ret-nonptr-hi)

			   (!! "Increment the power of ten (i.e. r21:r20 *= 10)")
			   (with-registers (r18 r19)
				   (mov r16 r20)
				   (mov r17 r21)
				   (ldi r18 10)
				   (clr r19)

				   (call mul16))

			   (mov r20 ret-nonptr-lo)
			   (mov r21 ret-nonptr-hi))
		   (progn
			   (!! "The character in r16 is non-numeric; unput it")
			   (call asm-unput-char)
			   (break-while))))

	(!! "Allocate the integer on the heap and return it")
	(movw r17 r16 r19 r18)
	(call gc-make-fixnum)
	(clear-reg ret-nonptr-hi ret-nonptr-lo)
	(inc ret-nonptr-lo))

(export-asm-func asm-read-number)

(defun-a7rl asm-read-symbol () (r16 r22  ;; temporaries, args
								r17 r18  ;; holds the length of the symbol being read, args
								X-lo X-hi r20 r21) ;; Holds the pointer to the text of the symbol being read on the stack
	(declare-stack-usage :non-pointer-bytes (+ 16 lavrock:preserved-register-byte-count))

	(!!! "Read a symbol from STDIN and return a pointer to it on the pointer return registers."
		 "Symbols can be a max of 16 bytes long.")

	(clr r17)
	(with-stack-space X-hi X-lo 16
		(!! "Preserve the pointer to the stack space")
		(movw r21 r20 X-hi X-lo)

		(!! "Read the symbol")
		(loop-while (:call (call asm-read-char)
					 :test (nonzero ret-nonptr-hi))
		   (! "argument to is-char-symchar")
		   (mov r16 ret-nonptr-lo)
		   (call is-char-symchar)
		   (if-else (nonzero ret-nonptr-hi)
			   (progn
				   (!! "The character that was read is a symchar; add it to the accumulating string.")
				   (inc r17)
				   (st X+ r16))
			   (progn
				   (!! "The char in r16 is non-numeric, unput it")
				   (call asm-unput-char)
				   (break-while))))

		(!! "Allocate GC space for the symbol and return that value.")
		(mov r18 r17)
		(movw r17 r16 r21 r20)
		(clear-reg X) ;; clear X so the GC won't think it points to memory
		(call gc-make-symbol)
		(clear-reg ret-nonptr-hi ret-nonptr-lo)
		(inc ret-nonptr-lo)))

(export-asm-func asm-read-symbol)

(defun-asm asm-discard-char () ()
	(call asm-read-char)
	(!! "We return 0 in the nonptr returns to indicate we didn't read an object")
	(clear-reg ret-nonptr-hi ret-nonptr-lo))

(export-asm-func asm-discard-char)

(defun-a7rl asm-read-list ()
	                      (r16 r17 ;; temporary pointer for read objects
						   r18 r19 ;; arguments
						   r20     ;; boolean: did we hit a closing paren?
						   X Y)    ;; holds tail and head of list, respectively
	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!! "Dispose of the leading parenthesis")
	(call asm-read-char)

	(!! "Set the head of the list to NIL")
	(clear-reg X Y r20)

	(loop-while (:call (call asm-read)
				 :test (nonzero ret-nonptr-lo))
	   (!! "Call GC-ALLOC-CONS with the new read object and the old head of the list (X) as args"
		   "Then store it back into X")
	   (movw r17 r16 ret-ptr-hi ret-ptr-lo)
	   (clear-reg r19 r18)
	   (call gc-make-cons)

	   (if-else (zero (Y-hi Y-lo))
		   (movw Y-hi Y-lo ret-ptr-hi ret-ptr-lo)
		   (progn
			   (!! "Update the CDR of the current tail")
			   (adiw (list X-hi X-lo) 3)
			   (st X+ ret-ptr-lo)
			   (st X+ ret-ptr-hi)))

	   (movw X-hi X-lo ret-ptr-hi ret-ptr-lo)

	   (call asm-peek-char)

	   (!! "If we run into a ), then discard it and return. (We let asm-read handle breaking the loop on EOF)")
	   (if-only (nonzero ret-nonptr-hi)
		   (mov r16 ret-nonptr-lo)
		   (if-only (i= r16 (char-code #\)))
			   (call asm-read-char)
			   (inc r20)
			   (break-while))))

	(if-only (zero r20)
		(call fatal-error))

	(movw ret-ptr-hi ret-ptr-lo Y-hi Y-lo)
	(clear-reg ret-nonptr-hi ret-nonptr-lo)
	(inc ret-nonptr-lo))

(export-asm-func asm-read-list)

(defun-a7rl asm-read () (r16          ; holds the read character
						 r17 r18 r19  ; args
						 r20 r21      ; store the current readtable
						 Z)           ; used to call the dispatch func

	(declare-stack-usage :non-pointer-bytes lavrock:preserved-register-byte-count)

	(!!! "Reads an object from STDIN and returns a pointer to it in RET-PTR-HI:LO."
		 "If no object was read, RET-NONPTR-LO is zero. Otherwise, it is nonzero.")

	(!! "Get the current readtable")
	(load-imm (list r17 r16) readtable-sym-gcptr)
	(call get-dynamic-binding)
	(movw r21 r20 ret-ptr-hi ret-ptr-lo)	

	(with-labels (try-read-again)
		(label try-read-again)

		(!! "We don't want to take the char out of the stream, so we peek at it")
		(call asm-peek-char)

		(!! "If we got a char, then look it up in the readtable, else indicate failure.")
		(if-else (nonzero ret-nonptr-hi)
			(progn
				(mov r16 ret-nonptr-lo)

				(call get-char-readtable-index)

				(!! "If we got a char in the readtable, then load the current readtable,"
					"get the dispatch function, and call it. Else, indicate failure.")
				(if-else (nonzero ret-nonptr-hi)
					(progn
						(! "Arg 2/2 to READTABLE-INDEX: the index into the readtable")
						(mov r18 ret-nonptr-lo)
						(clr r19)

						(! "Arg 1/2 to READTABLE-INDEX: The readtable itself (we asume we'll always get one from GET-DYNAMIC-BINDING)")
						(movw r17 r16 r21 r20)

						(!! "Index the readtable to get the dispatch function and invoke it,"
							"conveying the return values transparently.")
						(call readtable-index)
						(movw Z-hi Z-lo ret-ptr-hi ret-ptr-lo)
						(call-gcptr)

						(if-only (zero ret-nonptr-lo)
							(jmp try-read-again)))
					(clear-reg ret-nonptr-lo)))
			(clear-reg ret-nonptr-lo))))
