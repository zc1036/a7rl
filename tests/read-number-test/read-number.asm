.include "../m128def.inc"


.CSEG
.ORG 0x46
.DEF ZERO=R6
.DEF RET_NONPTR_LO=R8
.DEF RET_NONPTR_HI=R9
.DEF RET_PTR_LO=R10
.DEF RET_PTR_HI=R11
.DEF GC_HEAP_PTR_LO=R0
.DEF GC_HEAP_PTR_HI=R1
.EQU HEAP_START = 256
.EQU HEAP_END = 2304
.EQU RELOCATION_BUCKET_COUNT = 512
.EQU HEAP_RELOCATION_BUCKETS_BEGIN = 2304
.EQU HEAP_RELOCATION_BUCKETS_END = 2880
.EQU GLOBALS_BEGIN = 2880

PROGRAM: .DB 0x37, 0x34, 0x20, 0x20, 0x20, 
.EQU READ_PTR = 2880
.EQU UNPUT_CHAR = 2882
.EQU USE_UNPUT_CHAR = 2883

MAIN:
    clr ZERO
    out SREG, ZERO
    ;; Set up the stack
    ldi R16, low(RAMEND)
    out SPL, R16
    ldi R16, high(RAMEND)
    out SPH, R16
    ;; Call initialization functions
    call READER_INIT
    call GC_INIT
    call ASM_READ_NUMBER
    reti

ASM_READ_NUMBER:
    ;; Declare stack frame size (6 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 6
    push R25
    ldi R25, 0
    push R25
    ;;
    push R21
    push R20
    push R19
    push R18
    push R17
    push R16
 ;;; Reads a number from PROGRAM and returns it in the non-pointer return registers.
 ;;; The pointer return registers are left alone.
    clr R18
    clr R19
    ldi R20, 1
    clr R21
BEGIN_LOOP_8:
    call ASM_READ_CHAR
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_10
    mov R16, RET_NONPTR_LO
    call IS_CHAR_NUMERIC
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_11
    ;; The character that was in r16 is numeric; multiply its decimal value by 10 raised to the digit position
    push R18
    push R19
    mov R16, R20
    mov R17, R21
    mov R18, RET_NONPTR_LO
    clr R19
    call MUL16
    pop R19
    pop R18
    add R18, RET_NONPTR_LO
    adc R19, RET_NONPTR_HI
    ;; Increment the power of ten (i.e. r21:r20 *= 10)
    push R18
    push R19
    mov R16, R20
    mov R17, R21
    ldi R18, 10
    clr R19
    call MUL16
    pop R19
    pop R18
    mov R20, RET_NONPTR_LO
    mov R21, RET_NONPTR_HI
    rjmp FAILURE_CONDITION_END_12
FAILURE_CONDITION_START_11:
    ;; The character in r17 is non-numeric
    mov R16, R17
    call ASM_UNPUT_CHAR
    jmp END_LOOP_9
FAILURE_CONDITION_END_12:
    jmp BEGIN_LOOP_8
SUCCESS_CONDITION_END_10:
END_LOOP_9:
    movw RET_NONPTR_HI:RET_NONPTR_LO, R19:R18
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R21
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

GC_INIT:
    push R16
    ldi R16, low(HEAP_START)
    mov GC_HEAP_PTR_LO, R16
    ldi R16, high(HEAP_START)
    mov GC_HEAP_PTR_HI, R16
    pop R16
    ret

READER_INIT:
    push R16
    push R26
    push R27
    ;; Initialize READ-PTR with PROGRAM (shifted over one because program memory stores two bytes per address)
    ldi R26, low(READ_PTR)
    ldi R27, high(READ_PTR)
    ldi R16, low((PROGRAM<<1))
    st X+, R16
    ldi R16, high((PROGRAM<<1))
    st X, R16
    ;; Set USE-UNPUT-CHAR to false
    ldi R16, 0
    sts USE_UNPUT_CHAR, R16
    pop R27
    pop R26
    pop R16
    ret

ASM_UNPUT_CHAR:
 ;;; Like C's unputc; unputs the character in r16.
    sts UNPUT_CHAR, R16
    ldi R16, 1
    sts USE_UNPUT_CHAR, R16
    ret

MUL16:
    push R20
 ;;; Multiply two 16-bit numbers (r17:16 and r19:r18 and get a 16-bit result
 ;;; using the Peasant's multiplication method. The pointer-return register is left alone.
    ;; r17:r16 is divided in half repeatedly and r19:r18 is doubled until r17:r16 is zero.
    clr RET_NONPTR_LO
    clr RET_NONPTR_HI
START_1:
    cp R16, ZERO
    cpc R17, ZERO
    lds R20, SREG + 0x20
    sbrc R20, SREG_Z
    rjmp DONE_3
    ;; if LHS is odd, add RHS to the result
    sbrs R16, 0
    rjmp CONTINUE_2
    add RET_NONPTR_LO, R18
    adc RET_NONPTR_HI, R19
CONTINUE_2:
    lsr R16
    ror R17
    lsl R18
    rol R19
    rjmp START_1
DONE_3:
    pop R20
    ret

IS_CHAR_NUMERIC:
 ;;; Returns nonzero in RET-NONPTR-HI if r16 is a numeric character, 0 otherwise.
 ;;; If RET-NONPTR-HI is nonzero, RET-NONPTR-LO contains the character as a number.
    clr RET_NONPTR_HI
    ;; Subtract 0x30 because 0x30 is ASCII 0; if r16 ends up less than 0, it wasn't in the range [0x30, 0x39)
    subi R16, 48
    tst R16
    brpl SUCCESS_CONDITION_END_6
    ret
SUCCESS_CONDITION_END_6:
    ;; Subtract 10 and if the number is now negative, it was in the range [0x30, 0x3A) and is a number
    subi R16, 10
    tst R16
    brpl SUCCESS_CONDITION_END_7
    inc RET_NONPTR_HI
    mov RET_NONPTR_LO, R16
    ;; add back the 10 we subtracted above
    ldi R25, 10
    add RET_NONPTR_LO, R25
SUCCESS_CONDITION_END_7:
    ret

ASM_READ_CHAR:
    push R30
    push R31
    push R26
    push R27
    push R16
 ;;; Reads a character and returns it in RET-NONPTR-LO; RET-NONPTR-HI is 0 if EOF, nonzero otherwise
    clr RET_NONPTR_HI
    ;; Load USE-UNPUT-CHAR to check if we should return an unputted character
    lds R16, USE_UNPUT_CHAR
    ;; If we should, load UNPUT-CHAR into the lo return byte and 1 into the hi byte
    ;; to indicate success
    tst R16
    breq SUCCESS_CONDITION_END_5
    sts USE_UNPUT_CHAR, ZERO
    lds RET_NONPTR_LO, UNPUT_CHAR
    inc RET_NONPTR_HI
    pop R16
    pop R27
    pop R26
    pop R31
    pop R30
    ret
SUCCESS_CONDITION_END_5:
 ;;; TODO: Check if we are at the end of PROGRAM
    ;; Load a pointer to a pointer to the current byte in PROGRAM to X
    ldi R26, low(READ_PTR)
    ldi R27, high(READ_PTR)
    ;; Load a pointer to the current byte in PROGRAM to Z
    ld R30, X+
    ld R31, X
    ;; Load the current byte in PROGRAM to the lo-byte of the return registers,
    ;; and put 1 into the hi-byte to indicate success
    ;; Increment Z so it points to the next byte
    lpm RET_NONPTR_LO, Z+
    inc RET_NONPTR_HI
    ;; Store Z, a pointer to the next byte, to X, which points to READ-PTR
    st X, R31
    st -X, R30
    pop R16
    pop R27
    pop R26
    pop R31
    pop R30
    ret

.CSEG
.ORG 0x0
;; Interrupt table
jmp MAIN ; Reset
reti ; External Interrupt 0
nop
reti ; External Interrupt 1
nop
reti ; External Interrupt 2
nop
reti ; External Interrupt 3
nop
reti ; External Interrupt 4
nop
reti ; External Interrupt 5
nop
reti ; External Interrupt 6
nop
reti ; External Interrupt 7
nop
reti ; Timer/Counter2 Compare Match
nop
reti ; Timer/Counter2 Overflow
nop
reti ; Timer/Counter1 Capture Event
nop
reti ; Timer/Counter1 Compare Match A
nop
reti ; Timer/Counter1 Compare Match B
nop
reti ; Timer/Counter1 Overflow
nop
reti ; Timer/Counter0 Compare Match
nop
reti ; Timer/Counter0 Overflow
nop
reti ; Serial Transfer Complete
nop
reti ; USART 0 Rx Complete
nop
reti ; USART 0 Data Register Empty
nop
reti ; USART 0 Tx Complete
nop
reti ; ADC Conversion Complete
nop
reti ; EEPROM Ready
nop
reti ; Analog Comparator
nop
reti ; Timer/Counter1 Compare Match C
nop
reti ; Timer/Counter3 Capture Event
nop
reti ; Timer/Counter3 Compare Match A
nop
reti ; Timer/Counter3 Compare Match B
nop
reti ; Timer/Counter3 Compare Match C
nop
reti ; Timer/Counter3 Overflow
nop
reti ; USART 1 Rx Complete
nop
reti ; USART 1 Data Register Empty
nop
reti ; USART 1 Tx Complete
nop
reti ; Two-wire Serial Interface
nop
reti ; Store Program Memory Ready
nop
