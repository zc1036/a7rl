
;;;; Array functions

(in-package :asm-arrays)

(defun array-index-impl ()
	(left-shift-n (r19 r18) 1)

	(!! "Multiply the index by two to turn bytes into words and add it to the array pointer")
	(add Z-lo r18)
	(adc Z-hi r19)

	(ld-gcptr ret-ptr-lo Z+)
	(ld-gcptr ret-ptr-hi Z))

(defun-asm readtable-index (r16 r17  ; the array to index
							r18 r19) ; the element (word) index
	                       (Z)       ; temp for mem access
	(!!! "Indexes the given lisp array (of pointers) at the given position")
	
	(!! "Ensure this is a readtable")
	(movw Z-hi Z-lo r17 r16)
	(ld-gcptr r16 Z+)

	(if-only (i!= r16 readtable-object-id)
		(call fatal-error))

	(!! "Skip the length")
	(adiw (list Z-hi Z-lo) 2)

	(array-index-impl))

(defun-asm get-lexical-variable-by-index (r16 r17  ; lexical frame
										  r18)     ; index
	                                     (Z        ; memory access
										  r19)
	(movw Z-hi Z-lo r17 r16)

	(ld-gcptr r16 Z+)

	(!! "Make sure the access isn't out of bounds")
	(if-only (>= r18 r16)
		(call fatal-error))

	(clear-reg r19)
	(left-shift-n (r19 r18) 2)

	(!! "Add the index * 4 to the pointer to get the address of the symbol/value pair")
	(add Z-lo r18)
	(adc Z-hi r19)

	(!! "add two to get the address of the value")
	(ldi r16 2)
	(add Z-lo r16)
	(adc Z-hi zero)

	(ld-gcptr ret-ptr-lo Z+)
	(ld-gcptr ret-ptr-hi Z))

(defun-asm array-index (r16 r17
					    r18 r19)
	                   (Z)
	(movw Z-hi Z-lo r17 r16)
	(ld-gcptr r16 Z+)

	(if-only (i!= r16 array-object-id)
		(call fatal-error))

	(ld-gcptr r16 Z+)
	(ld-gcptr r17 Z+)

	(if-only (>= (r19 r18) (r17 r16))
		(call fatal-error))

	(array-index-impl))
