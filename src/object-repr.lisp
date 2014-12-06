
;;;; Global definition functions

(in-package :object-repr)

(defconstant-asm fixnum-object-id 0)
(defconstant-asm cons-object-id 1)
(defconstant-asm symbol-object-id 2)
(defconstant-asm readtable-object-id 4)
(defconstant-asm array-object-id 5)
(defconstant-asm function-object-id 6)

(defconstant-asm function-type-func 0)
(defconstant-asm function-type-macro 1)
(defconstant-asm function-type-special-form 2)

(defconstant-asm object-type-mask 7)

(defconstant-asm readtable-size-bytes 142)

(defmacro defconstant-symbol (name sym-name)
	`(progn
		 (defconstant-bytes ,name ',(append (list symbol-object-id (length sym-name))
											(map 'list #'char-int sym-name)))
		 
		 (defexpr-asm ,(intern (format nil "~a-GCPTR" name)) (bitor (lshifti ,name 1) (lshifti 1 15)))))

(defmacro defconstant-special-form (name func num-args)
	`(progn
		 (defconstant-bytes ,name (list function-object-id ; object header
										,num-args ; number of args
										function-type-special-form ; function type
										(lobyte ,func) (bitor (hibyte ,func) (lshifti 1 7)) ; function pointer
										0 0)) ; lexical frame pointer

		 (defexpr-asm ,(intern (format nil "~a-GCPTR" name)) (bitor (lshifti ,name 1) (lshifti 1 15)))))

(defmacro defconstant-ptr-array (name ptrs &optional (type-id 'array-object-id))
	(let ((ptrs-len (length ptrs)))
		(labels ((lo-hi-pptr-bytes (ptr)
					 (values
					  `(lobyte ,ptr)
					  `(bitor (hibyte ,ptr) (lshifti 1 7)))))
			`(progn
				 (defconstant-bytes ,name (list ,type-id
												(lobyte ,ptrs-len)
												(hibyte ,ptrs-len)
												,@(multiple-value-mapcar* #'lo-hi-pptr-bytes ptrs)))
				 (defexpr-asm ,(intern (format nil "~a-GCPTR" name)) (bitor (lshifti ,name 1) (lshifti 1 15)))))))

(defmacro defconstant-readtable (name)
	`(progn
		 (defconstant-ptr-array ,name
			 (reader:asm-discard-char	   ; Tab
			  reader:asm-discard-char	   ; cr/lf
			  reader:asm-discard-char	   ; (space)
			  reader:asm-read-symbol	   ; !
			  reader:asm-read-symbol	   ; "
			  reader:asm-read-symbol	   ; #
			  reader:asm-read-symbol	   ; $
			  reader:asm-read-symbol	   ; %
			  reader:asm-read-symbol	   ; &
			  reader:asm-read-symbol	   ; '
			  reader:asm-read-list		   ; (
			  reader:asm-read-symbol	   ; )
			  reader:asm-read-symbol	   ; *
			  reader:asm-read-symbol	   ; +
			  reader:asm-read-symbol	   ; ,
			  reader:asm-read-symbol	   ; -
			  reader:asm-read-symbol	   ; .
			  reader:asm-read-symbol	   ; /
			  reader:asm-read-number	   ; 0
			  reader:asm-read-number	   ; 1
			  reader:asm-read-number	   ; 2
			  reader:asm-read-number	   ; 3
			  reader:asm-read-number	   ; 4
			  reader:asm-read-number	   ; 5
			  reader:asm-read-number	   ; 6
			  reader:asm-read-number	   ; 7
			  reader:asm-read-number	   ; 8
			  reader:asm-read-number	   ; 9
			  reader:asm-read-symbol	   ; :
			  reader:asm-read-symbol	   ; ;
			  reader:asm-read-symbol	   ; <
			  reader:asm-read-symbol	   ; =
			  reader:asm-read-symbol	   ; >
			  reader:asm-read-symbol	   ; ?
			  reader:asm-read-symbol	   ; @
			  reader:asm-read-symbol	   ; A
			  reader:asm-read-symbol	   ; B
			  reader:asm-read-symbol	   ; C
			  reader:asm-read-symbol	   ; D
			  reader:asm-read-symbol	   ; E
			  reader:asm-read-symbol	   ; F
			  reader:asm-read-symbol	   ; G
			  reader:asm-read-symbol	   ; H
			  reader:asm-read-symbol	   ; I
			  reader:asm-read-symbol	   ; J
			  reader:asm-read-symbol	   ; K
			  reader:asm-read-symbol	   ; L
			  reader:asm-read-symbol	   ; M
			  reader:asm-read-symbol	   ; N
			  reader:asm-read-symbol	   ; O
			  reader:asm-read-symbol	   ; P
			  reader:asm-read-symbol	   ; Q
			  reader:asm-read-symbol	   ; R
			  reader:asm-read-symbol	   ; S
			  reader:asm-read-symbol	   ; T
			  reader:asm-read-symbol	   ; U
			  reader:asm-read-symbol	   ; V
			  reader:asm-read-symbol	   ; W
			  reader:asm-read-symbol	   ; X
			  reader:asm-read-symbol	   ; Y
			  reader:asm-read-symbol	   ; Z
			  reader:asm-read-symbol	   ; [
			  reader:asm-read-symbol	   ; \
			  reader:asm-read-symbol	   ; ]
			  reader:asm-read-symbol	   ; ^
			  reader:asm-read-symbol	   ; _
			  reader:asm-read-symbol	   ; `
			  reader:asm-read-symbol	   ; {
			  reader:asm-read-symbol	   ; |
			  reader:asm-read-symbol	   ; }
			  reader:asm-read-symbol)	   ; ~
			 readtable-object-id)))

(defmacro asm-mapc (reghi reglo body &optional dotted)
	"Evaluates BODY on each CAR of the list given in REGHI:REGLO. If
     the list is an improper list, DOTTED is evaluated on the CDR of
     the last CONS. Users need not preserve Z in their function stack frame."
	(let ((reghi-sym (gensym)) (reglo-sym (gensym)))
		`(let ((,reghi-sym ,reghi) (,reglo-sym ,reglo))
			 (with-registers (Z)
				 (movw Z-hi Z-lo ,reghi-sym ,reglo-sym)
				 (loop-while (:test (nonzero (Z-hi Z-lo)))
					(ld-gcptr a7rl::tmp-reg-lo Z)
					(if-only (i!= a7rl::tmp-reg-lo cons-object-id)
						,dotted
						(break-while))
					(adiw (list Z-hi Z-lo) 1)
					(ld-gcptr a7rl::tmp-reg-lo Z+)
					(ld-gcptr a7rl::tmp-reg-hi Z+)

					(with-registers (Z)
						(movw Z-hi Z-lo a7rl::tmp-reg-hi a7rl::tmp-reg-lo)
						(flet ((break-while ()
								   (with-registers-early-restore)
								   (break-while)))
							,body))

					(ld-gcptr a7rl::tmp-reg-lo Z+)
					(ld-gcptr a7rl::tmp-reg-hi Z)
					(movw Z-hi Z-lo a7rl::tmp-reg-hi a7rl::tmp-reg-lo))))))
