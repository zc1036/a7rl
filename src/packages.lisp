
(eval-when (:load-toplevel :compile-toplevel :execute)
	(asdf:load-system :lavrock))

(defpackage :a7rl
  (:use :cl :lavrock :instructions :codegen :asm-funcs :lavrock-util)
  (:export
   :init-funcs
   :defun-a7rl
   :declare-stack-usage

   :with-stack-space
   :call-gcptr
   :ld-gcptr

   :ret-nonptr-lo
   :ret-nonptr-hi

   :ret-ptr-lo
   :ret-ptr-hi
   :ret-nonptr
   :ret-ptr

   :ld-gcptr
   :call-gcptr

   :tmp-reg

   :init-funcs))

(defpackage :asm-lib
  (:use :cl :lavrock :instructions :codegen :asm-funcs :lavrock-util :a7rl)
  (:export :mul16 :subi16 :memcpy))

(defpackage :a7rl-asm-file
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib))

(defpackage :asm-lists
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib)
  (:export :search-list))

(defpackage :object-repr
  (:use :cl :lavrock :lavrock-util :instructions :codegen :asm-funcs :a7rl :asm-lib)
  (:export
   :defconstant-symbol
   :defconstant-readtable
   :defconstant-ptr-array
   :defconstant-special-form

   :symbol-object-id
   :cons-object-id
   :readtable-object-id
   :fixnum-object-id
   :array-object-id
   :function-object-id

   :object-type-mask

   :function-type-func
   :function-type-macro
   :function-type-special-form

   :asm-mapc

   :readtable-size-bytes))

(defpackage :asm-arrays
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib :object-repr)
  (:export :array-index :readtable-index :get-lexical-variable-by-index))

(defpackage :gc
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib :object-repr)
  (:export
   :defglobal-word
   :defglobal-byte

   :gc-alloc
   :gc-make-symbol
   :gc-make-cons
   :gc-make-fixnum
   :gc-make-lexical-frame))

(defpackage :eval
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib :gc :object-repr :asm-lists :asm-arrays)
  (:export
   :readtable-sym-gcptr
   :assert-type

   :asm-eval

   :get-dynamic-binding))

(defpackage :reader
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib :gc :asm-arrays :eval)
  (:export
   :asm-read-char
   :asm-peek-char
   :asm-read-number
   :asm-read-symbol
   :asm-read-list
   :asm-read
   :asm-discard-char))

(defpackage :main
  (:use :cl :lavrock :instructions :codegen :asm-funcs :a7rl :asm-lib :reader :eval))
