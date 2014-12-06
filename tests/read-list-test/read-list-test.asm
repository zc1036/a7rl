.include "../m128def.inc"

.CSEG
.ORG 0x46
.DEF ZERO=R6
.DEF RET_NONPTR_LO=R8
.DEF RET_NONPTR_HI=R9
.DEF RET_PTR_LO=R10
.DEF RET_PTR_HI=R11
.EQU READTABLE_OBJECT_ID = 4
.EQU FIXNUM_OBJECT_ID = 0
.EQU SYMBOL_OBJECT_ID = 2
.EQU CONS_OBJECT_ID = 1
.EQU OBJECT_TYPE_MASK = 7
.EQU READTABLE_SIZE_BYTES = 142
.DEF GC_HEAP_PTR_LO=R0
.DEF GC_HEAP_PTR_HI=R1
.EQU HEAP_START = 256
.EQU HEAP_END = 2304
.EQU RELOCATION_BUCKET_COUNT = 512
.EQU HEAP_RELOCATION_BUCKETS_BEGIN = 2304
.EQU HEAP_RELOCATION_BUCKETS_END = 2880
.EQU GLOBALS_BEGIN = 2880

PROGRAM: .DB 0x20, 0x28, 0x4C, 0x41, 0x37, 0x34, 0x20, 0x35, 0x29, 0x20, 0x20, 0x20, 
.EQU READ_PTR = 2880
.EQU UNPUT_CHAR = 2882
.EQU USE_UNPUT_CHAR = 2883
.EQU DYNAMIC_VARS_PTR = 2884

READTABLE_SYM: .DB SYMBOL_OBJECT_ID, 0xB, 
               .DB 0x2A, 0x72, 
               .DB 0x65, 0x61, 
               .DB 0x64, 0x74, 
               .DB 0x61, 0x62, 
               .DB 0x6C, 0x65, 
               .DB 0x2A, 0x0

.EQU READTABLE_SYM_GCPTR = ((READTABLE_SYM)<<(1))|((1)<<(15))

DEFAULT_READTABLE: .DB READTABLE_OBJECT_ID, low(ASM_DISCARD_CHAR), 
                   .DB (high(ASM_DISCARD_CHAR))|((1)<<(7)), low(ASM_DISCARD_CHAR), 
                   .DB (high(ASM_DISCARD_CHAR))|((1)<<(7)), low(ASM_DISCARD_CHAR), 
                   .DB (high(ASM_DISCARD_CHAR))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_LIST), 
                   .DB (high(ASM_READ_LIST))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_NUMBER), 
                   .DB (high(ASM_READ_NUMBER))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), low(ASM_READ_SYMBOL), 
                   .DB (high(ASM_READ_SYMBOL))|((1)<<(7)), 0x0

.EQU DEFAULT_READTABLE_GCPTR = ((DEFAULT_READTABLE)<<(1))|((1)<<(15))

MAIN:
    clr ZERO
    out SREG, ZERO
    ;; Set up the stack
    ldi R16, low(RAMEND)
    out SPL, R16
    ldi R16, high(RAMEND)
    out SPH, R16
    ;; Call initialization functions
    call GC_INIT
    call READER_INIT
    call EVAL_INIT
    call ASM_READ
    reti

SEARCH_LIST_SYMBOL_CALLBACK:
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
    push R20
    ;; Load the CAR of the CONS into r17:r16 (we increment the pointer first to skip the header byte)
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_95
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_96
LOAD_SRAM_GCPTR_95:
    ld R16, Z+
DONE_LOAD_GCPTR_96:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_97
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_98
LOAD_SRAM_GCPTR_97:
    ld R17, Z+
DONE_LOAD_GCPTR_98:
    ldi R20, SYMBOL_OBJECT_ID
    ;; If CHECK-OBJ-TYPES-EQUAL and SYMBOL-COMPARE return nonzero
    ;; then load the CDR of the CONS into RET-PTR-HI:LO and return
    call CHECK_OBJ_TYPES_EQUAL
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_99
    call SYMBOL_COMPARE
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_100
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_101
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_102
LOAD_SRAM_GCPTR_101:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_102:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_103
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_104
LOAD_SRAM_GCPTR_103:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_104:
SUCCESS_CONDITION_END_100:
SUCCESS_CONDITION_END_99:
    pop R20
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    ret

ASM_READ_LIST:
    ;; Declare stack frame size (9 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 9
    push R25
    ldi R25, 0
    push R25
    ;;
    push R28
    push R29
    push R26
    push R27
    push R20
    push R19
    push R18
    push R17
    push R16
    ;; Dispose of the leading parenthesis
    call ASM_READ_CHAR
    ;; Set the head of the list to NIL
    clr R27
    clr R26
    clr R29
    clr R28
    clr R20
BEGIN_LOOP_105:
    call ASM_READ
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_107
    ;; Call GC-ALLOC-CONS with the new read object and the old head of the list (X) as args
    ;; Then store it back into X
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    clr R19
    clr R18
    call GC_MAKE_CONS
    cp R28, ZERO
    cpc R29, ZERO
    brne FAILURE_CONDITION_START_108
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    rjmp FAILURE_CONDITION_END_109
FAILURE_CONDITION_START_108:
    ;; Update the CDR of the current tail
    adiw R27:R26, 3
    st X+, RET_PTR_LO
    st X+, RET_PTR_HI
FAILURE_CONDITION_END_109:
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    call ASM_PEEK_CHAR
    ;; If we run into a ), then discard it and return. (We let asm-read handle breaking the loop on EOF)
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_110
    mov R16, RET_NONPTR_LO
    cpi R16, 41
    brne SUCCESS_CONDITION_END_111
    call ASM_READ_CHAR
    inc R20
    jmp END_LOOP_106
SUCCESS_CONDITION_END_111:
SUCCESS_CONDITION_END_110:
    jmp BEGIN_LOOP_105
SUCCESS_CONDITION_END_107:
END_LOOP_106:
    tst R20
    brne SUCCESS_CONDITION_END_112
    call FATAL_ERROR
SUCCESS_CONDITION_END_112:
    movw RET_PTR_HI:RET_PTR_LO, R29:R28
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    inc RET_NONPTR_LO
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R27
    pop R26
    pop R29
    pop R28
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_DISCARD_CHAR:
    call ASM_READ_CHAR
    ;; We return 0 in the nonptr returns to indicate we didn't read an object
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    ret

ASM_READ_SYMBOL:
    ;; Declare stack frame size (24 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 24
    push R25
    ldi R25, 0
    push R25
    ;;
    push R21
    push R20
    push R27
    push R26
    push R18
    push R17
    push R22
    push R16
 ;;; Read a symbol from STDIN and return a pointer to it on the pointer return registers.
 ;;; Symbols can be a max of 16 bytes long.
    clr R17
    in R24, SPL
    in R25, SPH
    subi R24, 16
    sbc R25, ZERO
    mov R26, R24
    mov R27, R25
    subi R24, 1
    sbc R25, ZERO
    out SPL, R24
    out SPH, R25
    ;; Preserve the pointer to the stack space
    movw R21:R20, R27:R26
    ;; Read the symbol
BEGIN_LOOP_113:
    call ASM_READ_CHAR
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_115
 ; argument to is-char-symchar
    mov R16, RET_NONPTR_LO
    call IS_CHAR_SYMCHAR
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_116
    ;; The character that was read is a symchar; add it to the accumulating string.
    inc R17
    st X+, R16
    rjmp FAILURE_CONDITION_END_117
FAILURE_CONDITION_START_116:
    ;; The char in r16 is non-numeric, unput it
    call ASM_UNPUT_CHAR
    jmp END_LOOP_114
FAILURE_CONDITION_END_117:
    jmp BEGIN_LOOP_113
SUCCESS_CONDITION_END_115:
END_LOOP_114:
    ;; Allocate GC space for the symbol and return that value.
    mov R18, R17
    movw R17:R16, R21:R20
    clr R27
    clr R26
    call GC_MAKE_SYMBOL
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    inc RET_NONPTR_LO
    ldi R25, 16
    inc R25
    in R24, SPL
    add R24, R25
    out SPL, R24
    in R24, SPH
    adc R24, ZERO
    out SPH, R24
    pop R16
    pop R22
    pop R17
    pop R18
    pop R26
    pop R27
    pop R20
    pop R21
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

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
 ;;; Reads a number from PROGRAM and returns a GC pointer to it in the pointer return registers.
 ;;; RET-NONPTR-LO is nonzero for success, zero for failure.
    clr R18
    clr R19
    ldi R20, 1
    clr R21
    ;; We mustn't call any GC-calling functions in this loop because we modify the stack in it.
BEGIN_LOOP_118:
    call ASM_READ_CHAR
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_120
 ; argument to is-char-numeric
    mov R16, RET_NONPTR_LO
    call IS_CHAR_NUMERIC
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_121
    ;; The character in r16 is numeric; multiply its decimal value by 10 raised to the digit position
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
    rjmp FAILURE_CONDITION_END_122
FAILURE_CONDITION_START_121:
    ;; The character in r16 is non-numeric; unput it
    call ASM_UNPUT_CHAR
    jmp END_LOOP_119
FAILURE_CONDITION_END_122:
    jmp BEGIN_LOOP_118
SUCCESS_CONDITION_END_120:
END_LOOP_119:
    ;; Allocate the integer on the heap and return it
    movw R17:R16, R19:R18
    call GC_MAKE_FIXNUM
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    inc RET_NONPTR_LO
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

GC_MAKE_FIXNUM:
    ;; Declare stack frame size (5 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 5
    push R25
    ldi R25, 0
    push R25
    ;;
    push R17
    push R16
    push R26
    push R27
    push R18
    push R16
    push R17
 ; We want one word of memory (we only use three bytes)
    ldi R16, 1
    clr R17
    call GC_ALLOC
    pop R17
    pop R16
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    ldi R18, FIXNUM_OBJECT_ID
    st X+, R18
    st X+, R16
    st X+, R17
    subi R26, 3
    ldi R25, 0
    sbc R27, R25
    movw RET_PTR_HI:RET_PTR_LO, R27:R26
    pop R18
    pop R27
    pop R26
    pop R16
    pop R17
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

MUL16:
    push R19
    push R18
    push R17
    push R16
    push R20
 ;;; Multiply two 16-bit numbers (r17:16 and r19:r18 and get a 16-bit result
 ;;; using the Peasant's multiplication method. The pointer-return register is left alone.
    ;; r17:r16 is divided in half repeatedly and r19:r18 is doubled until r17:r16 is zero.
    clr RET_NONPTR_LO
    clr RET_NONPTR_HI
START_123:
    cp R16, ZERO
    cpc R17, ZERO
    lds R20, SREG + 0x20
    sbrc R20, SREG_Z
    rjmp DONE_125
    ;; if LHS is odd, add RHS to the result
    sbrs R16, 0
    rjmp CONTINUE_124
    add RET_NONPTR_LO, R18
    adc RET_NONPTR_HI, R19
CONTINUE_124:
    lsr R16
    ror R17
    lsl R18
    rol R19
    rjmp START_123
DONE_125:
    pop R20
    pop R16
    pop R17
    pop R18
    pop R19
    ret

IS_CHAR_NUMERIC:
    push R16
 ;;; Returns nonzero in RET-NONPTR-HI if r16 is a numeric character, 0 otherwise.
 ;;; If RET-NONPTR-HI is nonzero, RET-NONPTR-LO contains the character as a number.
    clr RET_NONPTR_HI
    ;; Subtract 0x30 because 0x30 is ASCII 0; if r16 ends up less than 0, it wasn't in the range [0x30, 0x39)
    subi R16, 48
    tst R16
    brpl SUCCESS_CONDITION_END_126
    pop R16
    ret
SUCCESS_CONDITION_END_126:
    ;; Subtract 10 and if the number is now negative, it was in the range [0x30, 0x3A) and is a number
    subi R16, 10
    tst R16
    brpl SUCCESS_CONDITION_END_127
    inc RET_NONPTR_HI
    mov RET_NONPTR_LO, R16
    ;; add back the 10 we subtracted above
    ldi R25, 10
    add RET_NONPTR_LO, R25
SUCCESS_CONDITION_END_127:
    pop R16
    ret

GC_MAKE_SYMBOL:
    ;; Declare stack frame size (8 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 8
    push R25
    ldi R25, 0
    push R25
    ;;
    push R18
    push R17
    push R16
    push R26
    push R27
    push R21
    push R20
    push R19
    push R16
    push R17
    ;; Round the length up to the nearest word (w/ at least 2 extra bytes) by adding five and
    ;; dividing by 4 (gc-alloc wants count in blocks of 4 bytes) and allocate that sucker
    clr R17
    ldi R16, 5
    add R16, R18
    adc R17, ZERO
    lsr R17
    ror R16
    lsr R17
    ror R16
    call GC_ALLOC
    pop R17
    pop R16
    ;; Store the object header and length in the allocated space
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    ldi R19, SYMBOL_OBJECT_ID
    st X+, R19
    st X+, R18
    ;; memcpy the contents over
 ; arg 3/3 to memcpy: length
    mov R20, R18
    clr R21
 ; arg 2/3 to memcpy: source
    movw R19:R18, R17:R16
 ; arg 1/3 to memcpy: destination
    movw R17:R16, R27:R26
    call MEMCPY
    ;; Undo the previous adding of 2 bytes
    subi R26, 2
    ldi R25, 0
    sbc R27, R25
    movw RET_PTR_HI:RET_PTR_LO, R27:R26
    pop R19
    pop R20
    pop R21
    pop R27
    pop R26
    pop R16
    pop R17
    pop R18
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_UNPUT_CHAR:
    push R16
 ;;; Like C's unputc; unputs the character in r16.
    sts UNPUT_CHAR, R16
    ldi R16, 1
    sts USE_UNPUT_CHAR, R16
    pop R16
    ret

IS_CHAR_SYMCHAR:
    push R16
    push R17
 ;;; Returns nonzero in RET-NONPTR-HI if r16 is a symbol-char, 0 otherwise.
    clr RET_NONPTR_HI
    ;; Test for closing parentheses
    cpi R16, 41
    brne SUCCESS_CONDITION_END_130
    rjmp IS_NOT_SYM_129
SUCCESS_CONDITION_END_130:
    ;; Test for ASCII range [33, 97)
    subi R16, 33
 ; If r16 is negative, it's too low
    brmi IS_NOT_SYM_129
    subi R16, 64
 ; If r16 is negative, it's good
    brmi IS_SYM_128
    ;; Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)
    subi R16, 26
    brmi IS_NOT_SYM_129
    subi R16, 4
    brmi IS_SYM_128
    rjmp IS_NOT_SYM_129
IS_SYM_128:
    inc RET_NONPTR_HI
IS_NOT_SYM_129:
    pop R17
    pop R16
    ret

FATAL_ERROR:
    sleep
    rjmp FATAL_ERROR
    ret

ASM_PEEK_CHAR:
    push R17
    push R16
    call ASM_READ_CHAR
    ;; Preserve the return value of asm-read-char and unput the char if needed
    movw R17:R16, RET_NONPTR_HI:RET_NONPTR_LO
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_131
    call ASM_UNPUT_CHAR
SUCCESS_CONDITION_END_131:
    movw RET_NONPTR_HI:RET_NONPTR_LO, R17:R16
    pop R16
    pop R17
    ret

GC_MAKE_CONS:
    ;; Declare stack frame size (7 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 7
    push R25
    ldi R25, 0
    push R25
    ;;
    push R19
    push R18
    push R17
    push R16
    push R20
    push R26
    push R27
    push R16
    push R17
 ; We want two words of memory (we only use five bytes)
    ldi R16, 2
    clr R17
    call GC_ALLOC
    pop R17
    pop R16
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    ldi R20, CONS_OBJECT_ID
    st X+, R20
    st X+, R16
    st X+, R17
    st X+, R18
    st X+, R19
    subi R26, 5
    ldi R25, 0
    sbc R27, R25
    movw RET_PTR_HI:RET_PTR_LO, R27:R26
    pop R27
    pop R26
    pop R20
    pop R16
    pop R17
    pop R18
    pop R19
    ;; Pop stack frame size markers
    pop R25
    pop R25
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
    breq SUCCESS_CONDITION_END_132
    sts USE_UNPUT_CHAR, ZERO
    lds RET_NONPTR_LO, UNPUT_CHAR
    inc RET_NONPTR_HI
    pop R16
    pop R27
    pop R26
    pop R31
    pop R30
    ret
SUCCESS_CONDITION_END_132:
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

SYMBOL_COMPARE:
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
    push R22
    push R21
    push R20
 ;;; Compares two symbols for equality (doesn't check type); if they are equal, RET-NONPTR-LO
 ;;; is nonzero; else, it is zero.
    clr RET_NONPTR_LO
    clr RET_NONPTR_HI
    ;; Check that lengths are equal
    movw R31:R30, R17:R16
 ; Skip the header byte
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_133
    andi R31, 127
    lpm R20, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_134
LOAD_SRAM_GCPTR_133:
    ld R20, Z+
DONE_LOAD_GCPTR_134:
    movw R17:R16, R31:R30
    movw R31:R30, R19:R18
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_135
    andi R31, 127
    lpm R21, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_136
LOAD_SRAM_GCPTR_135:
    ld R21, Z+
DONE_LOAD_GCPTR_136:
    movw R19:R18, R31:R30
 ; Ensure the lengths are equal, and if so, compare each byte in the representation for equality
    cp R20, R21
    brne SUCCESS_CONDITION_END_137
BEGIN_LOOP_138:
    tst R20
    breq SUCCESS_CONDITION_END_140
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_141
    andi R31, 127
    lpm R21, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_142
LOAD_SRAM_GCPTR_141:
    ld R21, Z+
DONE_LOAD_GCPTR_142:
    movw R17:R16, R31:R30
    movw R31:R30, R19:R18
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_143
    andi R31, 127
    lpm R22, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_144
LOAD_SRAM_GCPTR_143:
    ld R22, Z+
DONE_LOAD_GCPTR_144:
    movw R19:R18, R31:R30
    cp R21, R22
    breq SUCCESS_CONDITION_END_145
    jmp END_LOOP_139
SUCCESS_CONDITION_END_145:
    dec R20
    jmp BEGIN_LOOP_138
SUCCESS_CONDITION_END_140:
END_LOOP_139:
SUCCESS_CONDITION_END_137:
    tst R20
    brne SUCCESS_CONDITION_END_146
    inc RET_NONPTR_LO
SUCCESS_CONDITION_END_146:
    pop R20
    pop R21
    pop R22
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    ret

CHECK_OBJ_TYPES_EQUAL:
    push R20
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
 ;;; If the arguments have the same type and that type is r20, RET-NONPTR-LO will be nonzero; otherwise, it will be zero.
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    ;; Load the headers of the objects into r16 and r17
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_147
    andi R31, 127
    lpm R16, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_148
LOAD_SRAM_GCPTR_147:
    ld R16, Z
DONE_LOAD_GCPTR_148:
    movw R31:R30, R19:R18
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_149
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_150
LOAD_SRAM_GCPTR_149:
    ld R17, Z
DONE_LOAD_GCPTR_150:
    ;; Make sure it's the same type of object, and that they're symbols.
 ; Mask off the program/sram tag
    andi R16, OBJECT_TYPE_MASK
    andi R17, OBJECT_TYPE_MASK
    cp R16, R17
    brne SUCCESS_CONDITION_END_151
    cp R16, R20
    brne SUCCESS_CONDITION_END_152
    inc RET_NONPTR_LO
SUCCESS_CONDITION_END_152:
SUCCESS_CONDITION_END_151:
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    ret

ASM_READ:
    ;; Declare stack frame size (8 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 8
    push R25
    ldi R25, 0
    push R25
    ;;
    push R30
    push R31
    push R21
    push R20
    push R19
    push R18
    push R17
    push R16
 ;;; Reads an object from STDIN and returns a pointer to it in RET-PTR-HI:LO.
 ;;; If no object was read, RET-NONPTR-LO is zero. Otherwise, it is nonzero.
    ;; Get the current readtable
    ldi R17, high(((READTABLE_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((READTABLE_SYM)<<(1))|((1)<<(15)))
    call GET_DYNAMIC_BINDING
    movw R21:R20, RET_PTR_HI:RET_PTR_LO
TRY_READ_AGAIN_153:
    ;; We don't want to take the char out of the stream, so we peek at it
    call ASM_PEEK_CHAR
    ;; If we got a char, then look it up in the readtable, else indicate failure.
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_154
    mov R16, RET_NONPTR_LO
    call GET_CHAR_READTABLE_INDEX
    ;; If we got a char in the readtable, then load the current readtable,
    ;; get the dispatch function, and call it. Else, indicate failure.
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_156
 ; Arg 2/2 to ARRAY-INDEX: the index into the readtable
    mov R18, RET_NONPTR_LO
    clr R19
 ; Arg 1/2 to ARRAY-INDEX: The readtable itself (we asume we'll always get one from GET-DYNAMIC-BINDING)
    movw R17:R16, R21:R20
    ;; Index the readtable to get the dispatch function and invoke it,
    ;; conveying the return values transparently.
    call ARRAY_INDEX
    movw R31:R30, RET_PTR_HI:RET_PTR_LO
    sbrs R31, 7
    rjmp CALL_LISP_158
    andi R31, 127
    icall
    ori R31, 128
    rjmp CALL_DONE_159
CALL_LISP_158:
    call FATAL_ERROR
CALL_DONE_159:
    tst RET_NONPTR_LO
    brne SUCCESS_CONDITION_END_160
    jmp TRY_READ_AGAIN_153
SUCCESS_CONDITION_END_160:
    rjmp FAILURE_CONDITION_END_157
FAILURE_CONDITION_START_156:
    clr RET_NONPTR_LO
FAILURE_CONDITION_END_157:
    rjmp FAILURE_CONDITION_END_155
FAILURE_CONDITION_START_154:
    clr RET_NONPTR_LO
FAILURE_CONDITION_END_155:
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R21
    pop R31
    pop R30
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

EVAL_INIT:
    ;; Declare stack frame size (8 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 8
    push R25
    ldi R25, 0
    push R25
    ;;
    push R28
    push R29
    push R26
    push R27
    push R19
    push R18
    push R17
    push R16
    ;; Create the list of initial bindings
    ldi R17, high(((READTABLE_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((READTABLE_SYM)<<(1))|((1)<<(15)))
    ldi R19, high(((DEFAULT_READTABLE)<<(1))|((1)<<(15)))
    ldi R18, low(((DEFAULT_READTABLE)<<(1))|((1)<<(15)))
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    clr R18
    clr R19
    call GC_MAKE_CONS
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    ldi R28, low(DYNAMIC_VARS_PTR)
    ldi R29, high(DYNAMIC_VARS_PTR)
    st Y+, R26
    st Y, R27
    pop R16
    pop R17
    pop R18
    pop R19
    pop R27
    pop R26
    pop R29
    pop R28
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

READER_INIT:
    push R17
    push R16
    push R26
    push R27
    ;; Initialize READ-PTR with PROGRAM (shifted over one because program memory stores two bytes per address)
    ldi R26, low(READ_PTR)
    ldi R27, high(READ_PTR)
    ldi R16, low((PROGRAM)<<(1))
    st X+, R16
    ldi R16, high((PROGRAM)<<(1))
    st X, R16
    ;; Set USE-UNPUT-CHAR to false
    sts USE_UNPUT_CHAR, ZERO
    pop R27
    pop R26
    pop R16
    pop R17
    ret

GC_INIT:
    push R16
    ldi R16, low(HEAP_START)
    mov GC_HEAP_PTR_LO, R16
    ldi R16, high(HEAP_START)
    mov GC_HEAP_PTR_HI, R16
    pop R16
    ret

ARRAY_INDEX:
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
 ;;; Indexes the given lisp array (of pointers) at the given position
    ;; Ensure this is an array
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_161
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_162
LOAD_SRAM_GCPTR_161:
    ld R16, Z+
DONE_LOAD_GCPTR_162:
    cpi R16, READTABLE_OBJECT_ID
    breq SUCCESS_CONDITION_END_163
    call FATAL_ERROR
SUCCESS_CONDITION_END_163:
    ;; Multiply the index by two to turn bytes into words and add it to the array pointer
    lsl R18
    rol R19
    add R30, R18
    adc R31, R19
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_164
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_165
LOAD_SRAM_GCPTR_164:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_165:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_166
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_167
LOAD_SRAM_GCPTR_166:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_167:
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    ret

GET_CHAR_READTABLE_INDEX:
    push R16
    push R18
    push R17
 ;;; Returns nonzero in RET-NONPTR-HI if r16 is a character in the readtable, 0 otherwise.
 ;;; If RET-NONPTR-HI is nonzero, RET-NONPTR-LO contains the readtable index at which
 ;;; this character's read-function resides.
    clr RET_NONPTR_HI
    ;; Test for tab
    ldi R17, 9
    cp R16, R17
    brne SUCCESS_CONDITION_END_170
    ldi R17, 0
    rjmp IS_SYM_168
SUCCESS_CONDITION_END_170:
    ;; Test for carriage return
    ldi R17, 10
    cp R16, R17
    brne SUCCESS_CONDITION_END_171
    ldi R17, 1
    rjmp IS_SYM_168
SUCCESS_CONDITION_END_171:
    ;; Test for line feed
    ldi R17, 13
    cp R16, R17
    brne SUCCESS_CONDITION_END_172
    ldi R17, 1
    rjmp IS_SYM_168
SUCCESS_CONDITION_END_172:
    ;; Test for ASCII range [32, 97) (save r16 - 30 in r17 in case it's in this range)
    subi R16, 30
    mov R17, R16
    subi R16, 2
 ; If r16 is negative, it's too low
    brmi IS_NOT_SYM_169
    subi R16, 65
 ; If r16 is negative, it's good
    brmi IS_SYM_168
    ;; Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)
    subi R16, 26
    brmi IS_NOT_SYM_169
    ldi R17, 67
    add R17, R16
    subi R16, 4
    brmi IS_SYM_168
    rjmp IS_NOT_SYM_169
IS_SYM_168:
    inc RET_NONPTR_HI
    mov RET_NONPTR_LO, R17
IS_NOT_SYM_169:
    pop R17
    pop R18
    pop R16
    ret

GET_DYNAMIC_BINDING:
    push R17
    push R16
    push R26
    push R27
    push R21
    push R20
    push R19
    push R18
    ;; Put the symbol to search for into the callback arg position
    movw R19:R18, R17:R16
    ;; Put *DYNAMIC-VARS-PTR into the list arg position
    ldi R26, low(DYNAMIC_VARS_PTR)
    ldi R27, high(DYNAMIC_VARS_PTR)
    ld R16, X+
    ld R17, X+
    ;; Put the callback into position
    ldi R21, high(SEARCH_LIST_SYMBOL_CALLBACK)
    ldi R20, low(SEARCH_LIST_SYMBOL_CALLBACK)
    call SEARCH_LIST
    pop R18
    pop R19
    pop R20
    pop R21
    pop R27
    pop R26
    pop R16
    pop R17
    ret

MEMCPY:
    push R21
    push R20
    push R19
    push R18
    push R17
    push R16
    push R28
    push R29
    push R26
    push R27
    push R22
    ;; X is the destination and Y is the source
    movw R27:R26, R17:R16
    movw R29:R28, R19:R18
BEGIN_LOOP_173:
    cp R20, ZERO
    cpc R21, ZERO
    breq SUCCESS_CONDITION_END_175
    subi R20, 1
    ldi R25, 0
    sbc R21, R25
    ld R22, Y+
    st X+, R22
    jmp BEGIN_LOOP_173
SUCCESS_CONDITION_END_175:
END_LOOP_174:
    pop R22
    pop R27
    pop R26
    pop R29
    pop R28
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R21
    ret

GC_ALLOC:
    ;; Declare stack frame size (6 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 6
    push R25
    ldi R25, 0
    push R25
    ;;
    push R17
    push R16
    push R21
    push R20
    push R19
    push R18
    ;; Multiply the number by four to get our byte count
    lsl R16
    rol R17
    lsl R16
    rol R17
    ;; Preserve the old value of the heap pointer in r20:r21 so we can return it
    movw R21:R20, GC_HEAP_PTR_HI:GC_HEAP_PTR_LO
    ;; Add the byte count to the heap pointer
    add R16, GC_HEAP_PTR_LO
    adc R17, GC_HEAP_PTR_HI
    ;; load a pointer to the end of the heap into registers
    ldi R18, low(HEAP_END)
    ldi R19, high(HEAP_END)
    ;; stall if we're out of memory
    cp R16, R18
    cpc R17, R19
    brlo SUCCESS_CONDITION_END_176
    jmp FATAL_ERROR
SUCCESS_CONDITION_END_176:
    ;; Make the heap pointer point to the new end of the heap
    movw GC_HEAP_PTR_HI:GC_HEAP_PTR_LO, R17:R16
    ;; Return a pointer to the allocated memory
    movw RET_PTR_HI:RET_PTR_LO, R21:R20
    pop R18
    pop R19
    pop R20
    pop R21
    pop R16
    pop R17
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

SEARCH_LIST:
    push R21
    push R20
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
    push R23
    push R22
 ;;; Searches a linked list for a value. When calling the given predicate returns nonzero in
 ;;; RET-NONPTR-LO, this function will return the values for which the predicate did so in
 ;;; The predicate will be called with the CAR of each list node as the
 ;;; first argument, and the given callback argument as the second.
    ;; Load Z with the pointer to the list
    movw R31:R30, R17:R16
BEGIN_LOOP_177:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_179
    ;; Increment Z to skip the header byte
    adiw R31:R30, 1
    ;; Load the CAR of the list into r17:r16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_180
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_181
LOAD_SRAM_GCPTR_180:
    ld R16, Z+
DONE_LOAD_GCPTR_181:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_182
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_183
LOAD_SRAM_GCPTR_182:
    ld R17, Z+
DONE_LOAD_GCPTR_183:
    ;; Preserve Z in r23:r22
    movw R23:R22, R31:R30
    ;; load the callback into Z
    movw R31:R30, R21:R20
    ;; Call the callback to see if this is the return value
    icall
    ;; If it returned nonzero, stop and return
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_184
    jmp END_LOOP_178
SUCCESS_CONDITION_END_184:
    ;; Restore Z to the pointer to the list to search
    movw R31:R30, R23:R22
    ;; If it's not the one, put the CDR into Z and try again
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_185
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_186
LOAD_SRAM_GCPTR_185:
    ld R16, Z+
DONE_LOAD_GCPTR_186:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_187
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_188
LOAD_SRAM_GCPTR_187:
    ld R17, Z
DONE_LOAD_GCPTR_188:
    movw R31:R30, R17:R16
    jmp BEGIN_LOOP_177
SUCCESS_CONDITION_END_179:
END_LOOP_178:
    ;; We don't need to fix the return value because RET-NONPTR-LO will be zero if
    ;; we get to the end of the list and haven't found it (i.e. Z is NIL).
    pop R22
    pop R23
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R21
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
