
;;;; List functions

(in-package :asm-lists)

(defun-asm search-list (r16 r17  ; a pointer to the list to search
						r18 r19  ; a pointer to the callback argument
						r20 r21) ; a pointer to the callback
	                   (r22 r23 Z) ; temps, iterator
	(!!! "Searches a linked list for a value. When calling the given predicate returns nonzero in"
		 "RET-NONPTR-LO, this function will return the values for which the predicate did so in"
		 "The predicate will be called with the CAR of each list node as the"
		 "first argument, and the given callback argument as the second.")

	(!! "Load Z with the pointer to the list")
	(movw Z-hi Z-lo r17 r16)

	(loop-while (:test (nonzero (Z-hi Z-lo)))
	   (!! "Increment Z to skip the header byte")
	   (adiw (list Z-hi Z-lo) 1)

	   (!! "Load the CAR of the list into r17:r16")
	   (ld-gcptr r16 Z+)
	   (ld-gcptr r17 Z+)

	   (!! "Preserve Z in r23:r22")
	   (movw r23 r22 Z-hi Z-lo)

	   (!! "load the callback into Z")
	   (movw Z-hi Z-lo r21 r20)

	   (!! "Call the callback to see if this is the return value")
	   (icall)

	   (!! "If it returned nonzero, stop and return")
	   (if-only (nonzero ret-nonptr-lo)
		   (break-while))

	   (!! "Restore Z to the pointer to the list to search")
	   (movw Z-hi Z-lo r23 r22)

	   (!! "If it's not the one, put the CDR into Z and try again")
	   (ld-gcptr r16 Z+)
	   (ld-gcptr r17 Z)

	   (movw Z-hi Z-lo r17 r16))

	(!! "We don't need to fix the return value because RET-NONPTR-LO will be zero if"
		"we get to the end of the list and haven't found it (i.e. Z is NIL)."))
