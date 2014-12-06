
.include "../m128def.inc"

.CSEG
.ORG 71

.CSEG
.ORG 0x0
;; Interrupt table
		nop
		nop
		nop
		nop
		nop
jmp FMAN ; Reset

.cseg
.org 20
FMAN:
		nop
		nop
		nop
		ret

.cseg
.org 500
FJE:
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		ret

;; .cseg
;; .org 20
;; FMAIN:
;; 	nop
;; 	nop
;; 	nop
;;     reti

