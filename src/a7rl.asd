
(in-package :asdf-user)

(defsystem "a7rl"
	:description "A lisp for the ATMega128"
	:version "0.1"
	:author "Zach"
	:licence "GPL 3.0"
	:components ((:file "packages")
				 (:file "asm-lib")
				 (:file "a7rl"))
	;; :depends-on ("hunchentoot" "cl-json" "ironclad" "sqlite")
	)
