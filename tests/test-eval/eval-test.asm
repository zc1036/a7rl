
.include "../m128def.inc"

.CSEG
.ORG 0x46
.DEF ZERO=R6
.DEF RET_NONPTR_LO=R8
.DEF RET_NONPTR_HI=R9
.DEF RET_PTR_LO=R10
.DEF RET_PTR_HI=R11
.EQU FIXNUM_OBJECT_ID = 0
.EQU CONS_OBJECT_ID = 1
.EQU SYMBOL_OBJECT_ID = 2
.EQU READTABLE_OBJECT_ID = 4
.EQU ARRAY_OBJECT_ID = 5
.EQU FUNCTION_OBJECT_ID = 6
.EQU FUNCTION_TYPE_FUNC = 0
.EQU FUNCTION_TYPE_MACRO = 1
.EQU FUNCTION_TYPE_SPECIAL_FORM = 2
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

PROGRAM: .DB 0x20, 0x28, 0x53, 0x45, 0x54, 0x2D, 0x4C, 0x45, 0x44, 0x53, 0x20, 0x28,
	.Db 0x4C, 0x45, 0x54, 0x20, 0x28, 0x58, 0x20, 0x32, 0x29, 0x20, 0x28, 0x4C, 0x45, 0x54, 0x20, 0x28, 0x59, 0x20, 0x32, 0x29, 0x20, 0x28, 0x2B, 0x20, 0x58, 0x20, 0x59, 0x29, 0x29, 0x29, 0x29, 0x20, 
.EQU READ_PTR = 2880
.EQU UNPUT_CHAR = 2882
.EQU USE_UNPUT_CHAR = 2883
.EQU DYNAMIC_VARS_PTR = 2884
.EQU CURRENT_LEXICAL_FRAME = 2886

READTABLE_SYM: .DB SYMBOL_OBJECT_ID, 0xB, 
               .DB 0x2A, 0x52, 
               .DB 0x45, 0x41, 
               .DB 0x44, 0x54, 
               .DB 0x41, 0x42, 
               .DB 0x4C, 0x45, 
               .DB 0x2A, 0x0

.EQU READTABLE_SYM_GCPTR = ((READTABLE_SYM)<<(1))|((1)<<(15))

PLUS_SYM: .DB SYMBOL_OBJECT_ID, 0x1, 
          .DB 0x2B, 0x0

.EQU PLUS_SYM_GCPTR = ((PLUS_SYM)<<(1))|((1)<<(15))

LET_SYM: .DB SYMBOL_OBJECT_ID, 0x3, 
         .DB 0x4C, 0x45, 
         .DB 0x54, 0x0

.EQU LET_SYM_GCPTR = ((LET_SYM)<<(1))|((1)<<(15))

SET_LEDS_SYM: .DB SYMBOL_OBJECT_ID, 0x8, 
              .DB 0x53, 0x45, 
              .DB 0x54, 0x2D, 
              .DB 0x4C, 0x45, 
              .DB 0x44, 0x53, 
              
.EQU SET_LEDS_SYM_GCPTR = ((SET_LEDS_SYM)<<(1))|((1)<<(15))

DEFAULT_READTABLE: .DB READTABLE_OBJECT_ID, 0x47, 
                   .DB 0x0, low(ASM_DISCARD_CHAR), 
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

ASM_LET: .DB FUNCTION_OBJECT_ID, 0x2, 
         .DB FUNCTION_TYPE_SPECIAL_FORM, low(ASM_LET_SPECIAL_FORM), 
         .DB (high(ASM_LET_SPECIAL_FORM))|((1)<<(7)), 0x0, 
         .DB 0x0, 0x0

.EQU ASM_LET_GCPTR = ((ASM_LET)<<(1))|((1)<<(15))

ASM_PLUS: .DB FUNCTION_OBJECT_ID, 0x2, 
          .DB FUNCTION_TYPE_SPECIAL_FORM, low(ASM_PLUS_SPECIAL_FORM), 
          .DB (high(ASM_PLUS_SPECIAL_FORM))|((1)<<(7)), 0x0, 
          .DB 0x0, 0x0

.EQU ASM_PLUS_GCPTR = ((ASM_PLUS)<<(1))|((1)<<(15))

ASM_SET_LEDS: .DB FUNCTION_OBJECT_ID, 0x1, 
              .DB FUNCTION_TYPE_SPECIAL_FORM, low(ASM_SET_LEDS_SPECIAL_FORM), 
              .DB (high(ASM_SET_LEDS_SPECIAL_FORM))|((1)<<(7)), 0x0, 
              .DB 0x0, 0x0

.EQU ASM_SET_LEDS_GCPTR = ((ASM_SET_LEDS)<<(1))|((1)<<(15))

EVAL_TYPE_DISPATCH_TABLE: .DB ARRAY_OBJECT_ID, 0x7, 
                          .DB 0x0, low(ASM_EVAL_TO_SELF), 
                          .DB (high(ASM_EVAL_TO_SELF))|((1)<<(7)), low(ASM_EVAL_CONS), 
                          .DB (high(ASM_EVAL_CONS))|((1)<<(7)), low(ASM_EVAL_SYMBOL), 
                          .DB (high(ASM_EVAL_SYMBOL))|((1)<<(7)), low(ASM_EVAL_TO_SELF), 
                          .DB (high(ASM_EVAL_TO_SELF))|((1)<<(7)), low(ASM_EVAL_TO_SELF), 
                          .DB (high(ASM_EVAL_TO_SELF))|((1)<<(7)), low(ASM_EVAL_TO_SELF), 
                          .DB (high(ASM_EVAL_TO_SELF))|((1)<<(7)), low(ASM_EVAL_TO_SELF), 
                          .DB (high(ASM_EVAL_TO_SELF))|((1)<<(7)), 0x0

.EQU EVAL_TYPE_DISPATCH_TABLE_GCPTR = ((EVAL_TYPE_DISPATCH_TABLE)<<(1))|((1)<<(15))

MAIN:
    clr ZERO
    out SREG, ZERO
    ;; Enable sleep
    ldi R16, 32
    out MCUCR, R16
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
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    call ASM_EVAL
    call FATAL_ERROR
    reti

ASM_EVAL_TO_SELF:
    push R17
    push R16
 ;;; Return the argument
    movw RET_PTR_HI:RET_PTR_LO, R17:R16
    pop R16
    pop R17
    ret

ASM_EVAL_CONS:
    ;; Declare stack frame size (12 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 12
    push R25
    ldi R25, 0
    push R25
    ;;
    push R17
    push R16
    push R23
    push R22
    push R21
    push R20
    push R19
    push R18
    push R30
    push R31
    push R28
    push R29
 ;;; Evaluate a cons
    ;; Skip the header byte
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    ;; Evaluate the CAR and ensure it's a function
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_527
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_528
LOAD_SRAM_GCPTR_527:
    ld R16, Z+
DONE_LOAD_GCPTR_528:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_529
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_530
LOAD_SRAM_GCPTR_529:
    ld R17, Z+
DONE_LOAD_GCPTR_530:
    call ASM_EVAL
    ;; Assert that we got a function
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ldi R18, FUNCTION_OBJECT_ID
    call ASSERT_TYPE
    ;; Save the pointer to the function
    mov R22, R17
    mov R21, R16
    ;; Create a stack frame with the given size and container stack frame
 ; Save Z
    movw R29:R28, R31:R30
    mov R31, R22
    mov R30, R21
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_531
    andi R31, 127
    lpm R20, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_532
LOAD_SRAM_GCPTR_531:
    ld R20, Z+
DONE_LOAD_GCPTR_532:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_533
    andi R31, 127
    lpm R23, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_534
LOAD_SRAM_GCPTR_533:
    ld R23, Z+
DONE_LOAD_GCPTR_534:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_535
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_536
LOAD_SRAM_GCPTR_535:
    ld R16, Z+
DONE_LOAD_GCPTR_536:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_537
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_538
LOAD_SRAM_GCPTR_537:
    ld R17, Z+
DONE_LOAD_GCPTR_538:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_539
    andi R31, 127
    lpm R18, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_540
LOAD_SRAM_GCPTR_539:
    ld R18, Z+
DONE_LOAD_GCPTR_540:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_541
    andi R31, 127
    lpm R19, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_542
LOAD_SRAM_GCPTR_541:
    ld R19, Z
DONE_LOAD_GCPTR_542:
    ;; If this is a special form, we don't create a clean scope for it.
    cpi R23, FUNCTION_TYPE_SPECIAL_FORM
    brne SUCCESS_CONDITION_END_543
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_544
    andi R31, 127
    lpm R18, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_545
LOAD_SRAM_GCPTR_544:
    ld R18, Z+
DONE_LOAD_GCPTR_545:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_546
    andi R31, 127
    lpm R19, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_547
LOAD_SRAM_GCPTR_546:
    ld R19, Z
DONE_LOAD_GCPTR_547:
SUCCESS_CONDITION_END_543:
    movw R31:R30, R29:R28
    push R21
    mov R21, R23
    call GC_MAKE_LEXICAL_FRAME
    pop R21
    movw R19:R18, RET_PTR_HI:RET_PTR_LO
    ;; At this point: r19:18 = ptr to new lexical frame (is an argument to ASM-EXECUTE-FUNC-WITH-LEXICAL-FRAME)
    ;;                                                   and ASM-STORE-FUNC-ARGS)
    ;;                r17:16 = ptr to function definition (CAR has arg list)
    ;;                r22:21 = pointer to the function object
    ;;                r20    = number of arguments (from the definition) (is argument to ASM-EXECUTE...)
    ;;                Z      = ptr to first cons of the list of arguments in the CALL
    ;;                r23    = function type
    ;; Fill the lexical frame with arguments
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_548
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_549
LOAD_SRAM_GCPTR_548:
    ld R16, Z+
DONE_LOAD_GCPTR_549:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_550
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_551
LOAD_SRAM_GCPTR_550:
    ld R17, Z
DONE_LOAD_GCPTR_551:
    ;; r19:18 already has lexical frame, r20 already has the arg count in it
    mov R28, R21
    mov R21, R23
    call ASM_STORE_FUNC_ARGS
    mov R21, R28
 ; Arg to execution: pointer to function object
    mov R17, R22
    mov R16, R21
    ;; r19:18 already contains the pointer to the lexical frame; r20 has arg count
    call ASM_EXECUTE_FUNC_WITH_LEXICAL_FRAME
    pop R29
    pop R28
    pop R31
    pop R30
    pop R18
    pop R19
    pop R20
    pop R21
    pop R22
    pop R23
    pop R16
    pop R17
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_EVAL_SYMBOL:
    ;; Declare stack frame size (7 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 7
    push R25
    ldi R25, 0
    push R25
    ;;
    push R17
    push R16
    push R20
    push R19
    push R18
    push R30
    push R31
 ;;; Searches lexical bindings, then dynamic bindings, for a given symbol.
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    ;; Load the pointer to the current lexical frame into Z
    ld R18, Z+
    ld R19, Z
    movw R31:R30, R19:R18
    ;; Start iteration of the linked list of frames
BEGIN_LOOP_553:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_555
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_556
    andi R31, 127
    lpm R20, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_557
LOAD_SRAM_GCPTR_556:
    ld R20, Z+
DONE_LOAD_GCPTR_557:
BEGIN_LOOP_558:
    tst R20
    breq SUCCESS_CONDITION_END_560
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_561
    andi R31, 127
    lpm R18, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_562
LOAD_SRAM_GCPTR_561:
    ld R18, Z+
DONE_LOAD_GCPTR_562:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_563
    andi R31, 127
    lpm R19, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_564
LOAD_SRAM_GCPTR_563:
    ld R19, Z+
DONE_LOAD_GCPTR_564:
    call SYMBOL_COMPARE
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_565
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_566
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_567
LOAD_SRAM_GCPTR_566:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_567:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_568
    andi R31, 127
    lpm RET_PTR_HI, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_569
LOAD_SRAM_GCPTR_568:
    ld RET_PTR_HI, Z+
DONE_LOAD_GCPTR_569:
    rjmp DONE_SEARCHING_552
SUCCESS_CONDITION_END_565:
    adiw R31:R30, 2
    dec R20
    jmp BEGIN_LOOP_558
SUCCESS_CONDITION_END_560:
END_LOOP_559:
    ;; Load the next pointer into Z
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_570
    andi R31, 127
    lpm R18, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_571
LOAD_SRAM_GCPTR_570:
    ld R18, Z+
DONE_LOAD_GCPTR_571:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_572
    andi R31, 127
    lpm R19, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_573
LOAD_SRAM_GCPTR_572:
    ld R19, Z
DONE_LOAD_GCPTR_573:
    movw R31:R30, R19:R18
    jmp BEGIN_LOOP_553
SUCCESS_CONDITION_END_555:
END_LOOP_554:
DONE_SEARCHING_552:
    tst R20
    brne SUCCESS_CONDITION_END_574
    call GET_DYNAMIC_BINDING
SUCCESS_CONDITION_END_574:
    pop R31
    pop R30
    pop R18
    pop R19
    pop R20
    pop R16
    pop R17
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

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
    rjmp LOAD_SRAM_GCPTR_575
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_576
LOAD_SRAM_GCPTR_575:
    ld R16, Z+
DONE_LOAD_GCPTR_576:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_577
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_578
LOAD_SRAM_GCPTR_577:
    ld R17, Z+
DONE_LOAD_GCPTR_578:
    ldi R20, SYMBOL_OBJECT_ID
    ;; If CHECK-OBJ-TYPES-EQUAL and SYMBOL-COMPARE return nonzero
    ;; then load the CDR of the CONS into RET-PTR-HI:LO and return
    call CHECK_OBJ_TYPES_EQUAL
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_579
    call SYMBOL_COMPARE
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_580
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_581
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_582
LOAD_SRAM_GCPTR_581:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_582:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_583
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_584
LOAD_SRAM_GCPTR_583:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_584:
SUCCESS_CONDITION_END_580:
SUCCESS_CONDITION_END_579:
    pop R20
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    ret

ASM_SET_LEDS_SPECIAL_FORM:
    ;; Declare stack frame size (5 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 5
    push R25
    ldi R25, 0
    push R25
    ;;
    push R18
    push R17
    push R16
    push R30
    push R31
    ;; Load the lexical frame pointer into r16:17
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    ld R16, Z+
    ld R17, Z
    ldi R18, 0
    ;; Get the zeroth argument
    call GET_LEXICAL_VARIABLE_BY_INDEX
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ;; Evaluate the zeroth argument
    call ASM_EVAL
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ;; Check the type of the zeroth argument
    ldi R18, FIXNUM_OBJECT_ID
    call ASSERT_TYPE
    ;; Load the RHS into r17:16
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_585
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_586
LOAD_SRAM_GCPTR_585:
    ld R16, Z+
DONE_LOAD_GCPTR_586:
    out PORTA, R16
    break
    clr RET_PTR_HI
    clr RET_PTR_LO
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_PLUS_SPECIAL_FORM:
    ;; Declare stack frame size (9 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 9
    push R25
    ldi R25, 0
    push R25
    ;;
    push R23
    push R22
    push R21
    push R20
    push R18
    push R17
    push R16
    push R30
    push R31
    ;; Load the lexical frame pointer into r16:17
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    ld R16, Z+
    ld R17, Z
    ;; Get the zeroth argument
    ldi R18, 0
    call GET_LEXICAL_VARIABLE_BY_INDEX
    movw R23:R22, RET_PTR_HI:RET_PTR_LO
    ;; Get the first argument
    ldi R18, 1
    call GET_LEXICAL_VARIABLE_BY_INDEX
    ;; Evaluate the first argument
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    call ASM_EVAL
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ;; Check type of LHS
    ldi R18, FIXNUM_OBJECT_ID
    call ASSERT_TYPE
    ;; Load LHS into r21:20
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_587
    andi R31, 127
    lpm R20, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_588
LOAD_SRAM_GCPTR_587:
    ld R20, Z+
DONE_LOAD_GCPTR_588:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_589
    andi R31, 127
    lpm R21, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_590
LOAD_SRAM_GCPTR_589:
    ld R21, Z
DONE_LOAD_GCPTR_590:
    ;; Evaluate the zeroth argument
    movw R17:R16, R23:R22
    call ASM_EVAL
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ;; Check the type of the zeroth argument
    call ASSERT_TYPE
    ;; Load the RHS into r17:16
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_591
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_592
LOAD_SRAM_GCPTR_591:
    ld R16, Z+
DONE_LOAD_GCPTR_592:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_593
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_594
LOAD_SRAM_GCPTR_593:
    ld R17, Z
DONE_LOAD_GCPTR_594:
    add R16, R20
    adc R17, R21
    call GC_MAKE_FIXNUM
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R20
    pop R21
    pop R22
    pop R23
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_LET_SPECIAL_FORM:
    ;; Declare stack frame size (14 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 14
    push R25
    ldi R25, 0
    push R25
    ;;
    push R28
    push R29
    push R26
    push R27
    push R23
    push R22
    push R21
    push R20
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    ld R16, Z+
    ld R17, Z
    movw R31:R30, R17:R16
    ;; Get the zeroth argument (bindings) into r21:20
    ldi R18, 0
    call GET_LEXICAL_VARIABLE_BY_INDEX
    movw R21:R20, RET_PTR_HI:RET_PTR_LO
    ;; Get the first argument (body) into r23:22
    ldi R18, 1
    call GET_LEXICAL_VARIABLE_BY_INDEX
    movw R23:R22, RET_PTR_HI:RET_PTR_LO
    push R20
    push R21
    ;; r16 and r17 are unused by gc-make-lexical-frame if r21 is FUNCTION-TYPE-SPECIAL-FORM
    mov R18, R16
    mov R19, R17
    ldi R20, 1
    ldi R21, FUNCTION_TYPE_SPECIAL_FORM
    call GC_MAKE_LEXICAL_FRAME
    pop R21
    pop R20
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    movw R27:R26, R29:R28
    adiw R29:R28, 1
    ldi R18, 2
    push R30
    push R31
    movw R31:R30, R21:R20
BEGIN_LOOP_595:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_597
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_598
    andi R31, 127
    lpm R24, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_599
LOAD_SRAM_GCPTR_598:
    ld R24, Z
DONE_LOAD_GCPTR_599:
    cpi R24, CONS_OBJECT_ID
    breq SUCCESS_CONDITION_END_600
    call FATAL_ERROR
    jmp END_LOOP_596
SUCCESS_CONDITION_END_600:
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_601
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_602
LOAD_SRAM_GCPTR_601:
    ld R24, Z+
DONE_LOAD_GCPTR_602:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_603
    andi R31, 127
    lpm R25, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_604
LOAD_SRAM_GCPTR_603:
    ld R25, Z+
DONE_LOAD_GCPTR_604:
    push R30
    push R31
    movw R31:R30, R25:R24
    tst R18
    brne SUCCESS_CONDITION_END_605
    pop R31
    pop R30
    jmp END_LOOP_596
SUCCESS_CONDITION_END_605:
    call ASM_LET_MAPC_BODY
    pop R31
    pop R30
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_606
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_607
LOAD_SRAM_GCPTR_606:
    ld R24, Z+
DONE_LOAD_GCPTR_607:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_608
    andi R31, 127
    lpm R25, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_609
LOAD_SRAM_GCPTR_608:
    ld R25, Z
DONE_LOAD_GCPTR_609:
    movw R31:R30, R25:R24
    jmp BEGIN_LOOP_595
SUCCESS_CONDITION_END_597:
END_LOOP_596:
    pop R31
    pop R30
    ldi R28, low(CURRENT_LEXICAL_FRAME)
    ldi R29, high(CURRENT_LEXICAL_FRAME)
    st Y+, R26
    st Y, R27
    movw R17:R16, R23:R22
    call ASM_EVAL
    ldi R28, low(CURRENT_LEXICAL_FRAME)
    ldi R29, high(CURRENT_LEXICAL_FRAME)
    st Y+, R30
    st Y, R31
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
    pop R21
    pop R22
    pop R23
    pop R27
    pop R26
    pop R29
    pop R28
    ;; Pop stack frame size markers
    pop R25
    pop R25
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
BEGIN_LOOP_610:
    call ASM_READ
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_612
    ;; Call GC-ALLOC-CONS with the new read object and the old head of the list (X) as args
    ;; Then store it back into X
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    clr R19
    clr R18
    call GC_MAKE_CONS
    cp R28, ZERO
    cpc R29, ZERO
    brne FAILURE_CONDITION_START_613
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    jmp FAILURE_CONDITION_END_614
FAILURE_CONDITION_START_613:
    ;; Update the CDR of the current tail
    adiw R27:R26, 3
    st X+, RET_PTR_LO
    st X+, RET_PTR_HI
FAILURE_CONDITION_END_614:
    movw R27:R26, RET_PTR_HI:RET_PTR_LO
    call ASM_PEEK_CHAR
    ;; If we run into a ), then discard it and return. (We let asm-read handle breaking the loop on EOF)
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_615
    mov R16, RET_NONPTR_LO
    cpi R16, 41
    brne SUCCESS_CONDITION_END_616
    call ASM_READ_CHAR
    inc R20
    jmp END_LOOP_611
SUCCESS_CONDITION_END_616:
SUCCESS_CONDITION_END_615:
    jmp BEGIN_LOOP_610
SUCCESS_CONDITION_END_612:
END_LOOP_611:
    tst R20
    brne SUCCESS_CONDITION_END_617
    call FATAL_ERROR
SUCCESS_CONDITION_END_617:
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
BEGIN_LOOP_618:
    call ASM_READ_CHAR
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_620
 ; argument to is-char-symchar
    mov R16, RET_NONPTR_LO
    call IS_CHAR_SYMCHAR
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_621
    ;; The character that was read is a symchar; add it to the accumulating string.
    inc R17
    st X+, R16
    jmp FAILURE_CONDITION_END_622
FAILURE_CONDITION_START_621:
    ;; The char in r16 is non-numeric, unput it
    call ASM_UNPUT_CHAR
    jmp END_LOOP_619
FAILURE_CONDITION_END_622:
    jmp BEGIN_LOOP_618
SUCCESS_CONDITION_END_620:
END_LOOP_619:
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
BEGIN_LOOP_623:
    call ASM_READ_CHAR
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_625
 ; argument to is-char-numeric
    mov R16, RET_NONPTR_LO
    call IS_CHAR_NUMERIC
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_626
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
    jmp FAILURE_CONDITION_END_627
FAILURE_CONDITION_START_626:
    ;; The character in r16 is non-numeric; unput it
    call ASM_UNPUT_CHAR
    jmp END_LOOP_624
FAILURE_CONDITION_END_627:
    jmp BEGIN_LOOP_623
SUCCESS_CONDITION_END_625:
END_LOOP_624:
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
START_628:
    cp R16, ZERO
    cpc R17, ZERO
    lds R20, SREG + 0x20
    sbrc R20, SREG_Z
    rjmp DONE_630
    ;; if LHS is odd, add RHS to the result
    sbrs R16, 0
    rjmp CONTINUE_629
    add RET_NONPTR_LO, R18
    adc RET_NONPTR_HI, R19
CONTINUE_629:
    lsr R16
    ror R17
    lsl R18
    rol R19
    rjmp START_628
DONE_630:
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
    brpl SUCCESS_CONDITION_END_631
    pop R16
    ret
SUCCESS_CONDITION_END_631:
    ;; Subtract 10 and if the number is now negative, it was in the range [0x30, 0x3A) and is a number
    subi R16, 10
    tst R16
    brpl SUCCESS_CONDITION_END_632
    inc RET_NONPTR_HI
    mov RET_NONPTR_LO, R16
    ;; add back the 10 we subtracted above
    ldi R25, 10
    add RET_NONPTR_LO, R25
SUCCESS_CONDITION_END_632:
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
    brne SUCCESS_CONDITION_END_635
    rjmp IS_NOT_SYM_634
SUCCESS_CONDITION_END_635:
    ;; Test for ASCII range [33, 97)
    subi R16, 33
 ; If r16 is negative, it's too low
    brmi IS_NOT_SYM_634
    subi R16, 64
 ; If r16 is negative, it's good
    brmi IS_SYM_633
    ;; Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)
    subi R16, 26
    brmi IS_NOT_SYM_634
    subi R16, 4
    brmi IS_SYM_633
    rjmp IS_NOT_SYM_634
IS_SYM_633:
    inc RET_NONPTR_HI
IS_NOT_SYM_634:
    pop R17
    pop R16
    ret

ASM_PEEK_CHAR:
    push R17
    push R16
    call ASM_READ_CHAR
    ;; Preserve the return value of asm-read-char and unput the char if needed
    movw R17:R16, RET_NONPTR_HI:RET_NONPTR_LO
    tst RET_NONPTR_HI
    breq SUCCESS_CONDITION_END_636
    call ASM_UNPUT_CHAR
SUCCESS_CONDITION_END_636:
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
    breq SUCCESS_CONDITION_END_637
    sts USE_UNPUT_CHAR, ZERO
    lds RET_NONPTR_LO, UNPUT_CHAR
    inc RET_NONPTR_HI
    pop R16
    pop R27
    pop R26
    pop R31
    pop R30
    ret
SUCCESS_CONDITION_END_637:
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

ASM_LET_MAPC_BODY:
    cpi R18, 1
    brne SUCCESS_CONDITION_END_638
    movw R17:R16, R31:R30
    call ASM_EVAL
    movw R31:R30, RET_PTR_HI:RET_PTR_LO
SUCCESS_CONDITION_END_638:
    st Y+, R30
    st Y+, R31
    dec R18
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

GET_LEXICAL_VARIABLE_BY_INDEX:
    push R18
    push R17
    push R16
    push R19
    push R30
    push R31
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_639
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_640
LOAD_SRAM_GCPTR_639:
    ld R16, Z+
DONE_LOAD_GCPTR_640:
    ;; Make sure the access isn't out of bounds
    cp R18, R16
    brlo SUCCESS_CONDITION_END_641
    call FATAL_ERROR
SUCCESS_CONDITION_END_641:
    clr R19
    lsl R18
    rol R19
    lsl R18
    rol R19
    ;; Add the index * 4 to the pointer to get the address of the symbol/value pair
    add R30, R18
    adc R31, R19
    ;; add two to get the address of the value
    ldi R16, 2
    add R30, R16
    adc R31, ZERO
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_642
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_643
LOAD_SRAM_GCPTR_642:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_643:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_644
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_645
LOAD_SRAM_GCPTR_644:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_645:
    pop R31
    pop R30
    pop R19
    pop R16
    pop R17
    pop R18
    ret

CHECK_OBJ_TYPES_EQUAL:
    push R20
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
 ;;; If the arguments have the same type and that type is r20,
 ;;; RET-NONPTR-LO will be nonzero; otherwise, it will be zero.
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
    ;; Load the headers of the objects into r16 and r17
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_646
    andi R31, 127
    lpm R16, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_647
LOAD_SRAM_GCPTR_646:
    ld R16, Z
DONE_LOAD_GCPTR_647:
    movw R31:R30, R19:R18
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_648
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_649
LOAD_SRAM_GCPTR_648:
    ld R17, Z
DONE_LOAD_GCPTR_649:
    ;; Make sure it's the same type of object, and that they're symbols.
 ; Mask off the program/sram tag
    andi R16, OBJECT_TYPE_MASK
    andi R17, OBJECT_TYPE_MASK
    cp R16, R17
    brne SUCCESS_CONDITION_END_650
    cp R16, R20
    brne SUCCESS_CONDITION_END_651
    inc RET_NONPTR_LO
SUCCESS_CONDITION_END_651:
SUCCESS_CONDITION_END_650:
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    pop R20
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
    clr RET_PTR_HI
    clr RET_PTR_LO
    clr RET_NONPTR_HI
    clr RET_NONPTR_LO
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
    cp R16, ZERO
    cpc R17, ZERO
    brne SUCCESS_CONDITION_END_652
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
SUCCESS_CONDITION_END_652:
    cp R18, ZERO
    cpc R19, ZERO
    brne SUCCESS_CONDITION_END_653
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
SUCCESS_CONDITION_END_653:
    ;; Check that lengths are equal
    movw R31:R30, R17:R16
 ; Skip the header byte
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_654
    andi R31, 127
    lpm R20, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_655
LOAD_SRAM_GCPTR_654:
    ld R20, Z+
DONE_LOAD_GCPTR_655:
    movw R17:R16, R31:R30
    movw R31:R30, R19:R18
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_656
    andi R31, 127
    lpm R21, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_657
LOAD_SRAM_GCPTR_656:
    ld R21, Z+
DONE_LOAD_GCPTR_657:
    movw R19:R18, R31:R30
 ; Ensure the lengths are equal, and if so, compare each byte in the representation for equality
    cp R20, R21
    brne SUCCESS_CONDITION_END_658
BEGIN_LOOP_659:
    tst R20
    breq SUCCESS_CONDITION_END_661
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_662
    andi R31, 127
    lpm R21, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_663
LOAD_SRAM_GCPTR_662:
    ld R21, Z+
DONE_LOAD_GCPTR_663:
    movw R17:R16, R31:R30
    movw R31:R30, R19:R18
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_664
    andi R31, 127
    lpm R22, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_665
LOAD_SRAM_GCPTR_664:
    ld R22, Z+
DONE_LOAD_GCPTR_665:
    movw R19:R18, R31:R30
    cp R21, R22
    breq SUCCESS_CONDITION_END_666
    jmp END_LOOP_660
SUCCESS_CONDITION_END_666:
    dec R20
    jmp BEGIN_LOOP_659
SUCCESS_CONDITION_END_661:
END_LOOP_660:
SUCCESS_CONDITION_END_658:
    tst R20
    brne SUCCESS_CONDITION_END_667
    inc RET_NONPTR_LO
SUCCESS_CONDITION_END_667:
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

ASM_EXECUTE_FUNC_WITH_LEXICAL_FRAME:
    ;; Declare stack frame size (9 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 9
    push R25
    ldi R25, 0
    push R25
    ;;
    push R19
    push R18
    push R17
    push R16
    push R22
    push R21
    push R20
    push R30
    push R31
    ;; Set the current lexical frame to this one (preserving the old one in r21:r20)
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    ld R20, Z+
    ld R21, Z
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    st Z+, R18
    st Z, R19
    movw R31:R30, R17:R16
 ; Skip the header, and # of arguments
    adiw R31:R30, 2
 ; load the function type into r22
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_668
    andi R31, 127
    lpm R22, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_669
LOAD_SRAM_GCPTR_668:
    ld R22, Z+
DONE_LOAD_GCPTR_669:
    ;; Load the pointer to the function body into Z
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_670
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_671
LOAD_SRAM_GCPTR_670:
    ld R16, Z+
DONE_LOAD_GCPTR_671:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_672
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_673
LOAD_SRAM_GCPTR_672:
    ld R17, Z
DONE_LOAD_GCPTR_673:
    movw R31:R30, R17:R16
    ;; If this is a special form, invoke it with ICALL; else, skip the arg forms.
    cpi R22, FUNCTION_TYPE_SPECIAL_FORM
    brne FAILURE_CONDITION_START_674
    sbrs R31, 7
    rjmp CALL_LISP_676
    andi R31, 127
    icall
    ori R31, 128
    rjmp CALL_DONE_677
CALL_LISP_676:
    call FATAL_ERROR
CALL_DONE_677:
    jmp FAILURE_CONDITION_END_675
FAILURE_CONDITION_START_674:
 ; Skip the header and CAR
    adiw R31:R30, 3
    ;; Load the CDR (function body) into r17:16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_678
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_679
LOAD_SRAM_GCPTR_678:
    ld R16, Z+
DONE_LOAD_GCPTR_679:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_680
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_681
LOAD_SRAM_GCPTR_680:
    ld R17, Z
DONE_LOAD_GCPTR_681:
    ;; Evaluate each of the body forms if this is a function or macro
    push R30
    push R31
    movw R31:R30, R17:R16
BEGIN_LOOP_682:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_684
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_685
    andi R31, 127
    lpm R24, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_686
LOAD_SRAM_GCPTR_685:
    ld R24, Z
DONE_LOAD_GCPTR_686:
    cpi R24, CONS_OBJECT_ID
    breq SUCCESS_CONDITION_END_687
    call FATAL_ERROR
    jmp END_LOOP_683
SUCCESS_CONDITION_END_687:
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_688
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_689
LOAD_SRAM_GCPTR_688:
    ld R24, Z+
DONE_LOAD_GCPTR_689:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_690
    andi R31, 127
    lpm R25, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_691
LOAD_SRAM_GCPTR_690:
    ld R25, Z+
DONE_LOAD_GCPTR_691:
    push R30
    push R31
    movw R31:R30, R25:R24
    movw R17:R16, R31:R30
    call ASM_EVAL
    pop R31
    pop R30
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_692
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_693
LOAD_SRAM_GCPTR_692:
    ld R24, Z+
DONE_LOAD_GCPTR_693:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_694
    andi R31, 127
    lpm R25, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_695
LOAD_SRAM_GCPTR_694:
    ld R25, Z
DONE_LOAD_GCPTR_695:
    movw R31:R30, R25:R24
    jmp BEGIN_LOOP_682
SUCCESS_CONDITION_END_684:
END_LOOP_683:
    pop R31
    pop R30
FAILURE_CONDITION_END_675:
    ;; Restore the old lexical frame
    ldi R30, low(CURRENT_LEXICAL_FRAME)
    ldi R31, high(CURRENT_LEXICAL_FRAME)
    st Z+, R20
    st Z, R21
    pop R31
    pop R30
    pop R20
    pop R21
    pop R22
    pop R16
    pop R17
    pop R18
    pop R19
    ;; Pop stack frame size markers
    pop R25
    pop R25
    ret

ASM_STORE_FUNC_ARGS:
    ;; Declare stack frame size (8 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 8
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
    push R26
    push R27
    movw R27:R26, R19:R18
    adiw R27:R26, 1
    ;; Evaluate each argument in succession, storing each result in 
    ;; the lexical frame (fatal error on improper lists)
    push R30
    push R31
    movw R31:R30, R17:R16
BEGIN_LOOP_696:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_698
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_699
    andi R31, 127
    lpm R24, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_700
LOAD_SRAM_GCPTR_699:
    ld R24, Z
DONE_LOAD_GCPTR_700:
    cpi R24, CONS_OBJECT_ID
    breq SUCCESS_CONDITION_END_701
    call FATAL_ERROR
    jmp END_LOOP_697
SUCCESS_CONDITION_END_701:
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_702
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_703
LOAD_SRAM_GCPTR_702:
    ld R24, Z+
DONE_LOAD_GCPTR_703:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_704
    andi R31, 127
    lpm R25, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_705
LOAD_SRAM_GCPTR_704:
    ld R25, Z+
DONE_LOAD_GCPTR_705:
    push R30
    push R31
    movw R31:R30, R25:R24
    cpi R20, 0
    brne SUCCESS_CONDITION_END_706
    pop R31
    pop R30
    jmp END_LOOP_697
SUCCESS_CONDITION_END_706:
    call ASM_STORE_FUNC_ARGS_MAPC_BODY
    pop R31
    pop R30
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_707
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_708
LOAD_SRAM_GCPTR_707:
    ld R24, Z+
DONE_LOAD_GCPTR_708:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_709
    andi R31, 127
    lpm R25, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_710
LOAD_SRAM_GCPTR_709:
    ld R25, Z
DONE_LOAD_GCPTR_710:
    movw R31:R30, R25:R24
    jmp BEGIN_LOOP_696
SUCCESS_CONDITION_END_698:
END_LOOP_697:
    pop R31
    pop R30
    pop R27
    pop R26
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

GC_MAKE_LEXICAL_FRAME:
    ;; Declare stack frame size (12 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 12
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
    push R23
    push R22
    push R30
    push R31
    push R28
    push R29
    cpi R21, FUNCTION_TYPE_SPECIAL_FORM
    breq SUCCESS_CONDITION_END_711
    ;; Put the CAR of the given function body (the argument list) into r17:16
    movw R31:R30, R17:R16
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_712
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_713
LOAD_SRAM_GCPTR_712:
    ld R16, Z+
DONE_LOAD_GCPTR_713:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_714
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_715
LOAD_SRAM_GCPTR_714:
    ld R17, Z+
DONE_LOAD_GCPTR_715:
SUCCESS_CONDITION_END_711:
    push R16
    push R17
    push R20
    ;; We want at least 4 * n + 3 bytes
    clr R17
    lsl R20
    rol R17
    lsl R20
    rol R17
    mov R23, R17
    mov R22, R20
    ldi R16, 8
    add R16, R20
    adc R17, ZERO
    lsr R17
    ror R16
    lsr R17
    ror R16
    call GC_ALLOC
    pop R20
    pop R17
    pop R16
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    push R28
    push R29
    push R18
    ;; Store the length
    st Y+, R20
    ;; If this is a special form, just zero-out the memory;
    ;; else, fill it with the argument names.
    cpi R21, FUNCTION_TYPE_SPECIAL_FORM
    brne FAILURE_CONDITION_START_716
BEGIN_LOOP_718:
    cp R22, ZERO
    cpc R23, ZERO
    breq SUCCESS_CONDITION_END_720
    st Y+, ZERO
    subi R22, 1
    sbc R23, ZERO
    jmp BEGIN_LOOP_718
SUCCESS_CONDITION_END_720:
END_LOOP_719:
    jmp FAILURE_CONDITION_END_717
FAILURE_CONDITION_START_716:
    ldi R18, SYMBOL_OBJECT_ID
    ;; Store the argument names
    push R30
    push R31
    movw R31:R30, R17:R16
BEGIN_LOOP_721:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_723
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_724
    andi R31, 127
    lpm R24, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_725
LOAD_SRAM_GCPTR_724:
    ld R24, Z
DONE_LOAD_GCPTR_725:
    cpi R24, CONS_OBJECT_ID
    breq SUCCESS_CONDITION_END_726
    jmp END_LOOP_722
SUCCESS_CONDITION_END_726:
    adiw R31:R30, 1
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_727
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_728
LOAD_SRAM_GCPTR_727:
    ld R24, Z+
DONE_LOAD_GCPTR_728:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_729
    andi R31, 127
    lpm R25, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_730
LOAD_SRAM_GCPTR_729:
    ld R25, Z+
DONE_LOAD_GCPTR_730:
    push R30
    push R31
    movw R31:R30, R25:R24
    movw R17:R16, R31:R30
    call ASSERT_TYPE
    st Y+, R30
    st Y+, R31
    st Y+, ZERO
    st Y+, ZERO
    pop R31
    pop R30
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_731
    andi R31, 127
    lpm R24, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_732
LOAD_SRAM_GCPTR_731:
    ld R24, Z+
DONE_LOAD_GCPTR_732:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_733
    andi R31, 127
    lpm R25, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_734
LOAD_SRAM_GCPTR_733:
    ld R25, Z
DONE_LOAD_GCPTR_734:
    movw R31:R30, R25:R24
    jmp BEGIN_LOOP_721
SUCCESS_CONDITION_END_723:
END_LOOP_722:
    pop R31
    pop R30
FAILURE_CONDITION_END_717:
    pop R18
    ;; Store the pointer to the next frame
    st Y+, R18
    st Y+, R19
    pop R29
    pop R28
    movw RET_PTR_HI:RET_PTR_LO, R29:R28
    pop R29
    pop R28
    pop R31
    pop R30
    pop R22
    pop R23
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

ASSERT_TYPE:
    push R18
    push R17
    push R16
    push R30
    push R31
 ;;; Fatal error if object is not of the specified type or is null;
 ;;; returns a pointer to after the header byte
    cp R16, ZERO
    cpc R17, ZERO
    brne SUCCESS_CONDITION_END_735
    call FATAL_ERROR
SUCCESS_CONDITION_END_735:
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_736
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_737
LOAD_SRAM_GCPTR_736:
    ld R16, Z+
DONE_LOAD_GCPTR_737:
    cp R18, R16
    breq SUCCESS_CONDITION_END_738
    call FATAL_ERROR
SUCCESS_CONDITION_END_738:
    movw RET_PTR_HI:RET_PTR_LO, R31:R30
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    ret

FATAL_ERROR:
    break
    sleep
    rjmp FATAL_ERROR
    ret

ASM_EVAL:
    ;; Declare stack frame size (8 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 8
    push R25
    ldi R25, 0
    push R25
    ;;
    push R17
    push R16
    push R30
    push R31
    push R26
    push R27
    push R19
    push R18
    ;; Check the type and dispatch on it
    movw R27:R26, R17:R16
    ldi R17, high(((EVAL_TYPE_DISPATCH_TABLE)<<(1))|((1)<<(15)))
    ldi R16, low(((EVAL_TYPE_DISPATCH_TABLE)<<(1))|((1)<<(15)))
    ld R18, X
    clr R19
    call ARRAY_INDEX
    movw R17:R16, R27:R26
    movw R31:R30, RET_PTR_HI:RET_PTR_LO
    sbrs R31, 7
    rjmp CALL_LISP_739
    andi R31, 127
    icall
    ori R31, 128
    rjmp CALL_DONE_740
CALL_LISP_739:
    call FATAL_ERROR
CALL_DONE_740:
    pop R18
    pop R19
    pop R27
    pop R26
    pop R31
    pop R30
    pop R16
    pop R17
    ;; Pop stack frame size markers
    pop R25
    pop R25
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
TRY_READ_AGAIN_741:
    ;; We don't want to take the char out of the stream, so we peek at it
    call ASM_PEEK_CHAR
    ;; If we got a char, then look it up in the readtable, else indicate failure.
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_742
    mov R16, RET_NONPTR_LO
    call GET_CHAR_READTABLE_INDEX
    ;; If we got a char in the readtable, then load the current readtable,
    ;; get the dispatch function, and call it. Else, indicate failure.
    tst RET_NONPTR_HI
    breq FAILURE_CONDITION_START_744
 ; Arg 2/2 to READTABLE-INDEX: the index into the readtable
    mov R18, RET_NONPTR_LO
    clr R19
 ; Arg 1/2 to READTABLE-INDEX: The readtable itself (we asume we'll always get one from GET-DYNAMIC-BINDING)
    movw R17:R16, R21:R20
    ;; Index the readtable to get the dispatch function and invoke it,
    ;; conveying the return values transparently.
    call READTABLE_INDEX
    movw R31:R30, RET_PTR_HI:RET_PTR_LO
    sbrs R31, 7
    rjmp CALL_LISP_746
    andi R31, 127
    icall
    ori R31, 128
    rjmp CALL_DONE_747
CALL_LISP_746:
    call FATAL_ERROR
CALL_DONE_747:
    tst RET_NONPTR_LO
    brne SUCCESS_CONDITION_END_748
    jmp TRY_READ_AGAIN_741
SUCCESS_CONDITION_END_748:
    jmp FAILURE_CONDITION_END_745
FAILURE_CONDITION_START_744:
    clr RET_NONPTR_LO
FAILURE_CONDITION_END_745:
    jmp FAILURE_CONDITION_END_743
FAILURE_CONDITION_START_742:
    clr RET_NONPTR_LO
FAILURE_CONDITION_END_743:
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
    ;; Declare stack frame size (6 bytes for non-lisp-pointers and 0 for lisp pointers)
    ldi R25, 6
    push R25
    ldi R25, 0
    push R25
    ;;
    push R28
    push R29
    push R19
    push R18
    push R17
    push R16
    ;; Configure LEDs
    ldi R16, 7
    out DDRA, R16
    ;; Create the list of initial bindings
 ; *readtable*
    ldi R17, high(((READTABLE_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((READTABLE_SYM)<<(1))|((1)<<(15)))
    ldi R19, high(((DEFAULT_READTABLE)<<(1))|((1)<<(15)))
    ldi R18, low(((DEFAULT_READTABLE)<<(1))|((1)<<(15)))
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    clr R18
    clr R19
    call GC_MAKE_CONS
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    ldi R17, high(((SET_LEDS_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((SET_LEDS_SYM)<<(1))|((1)<<(15)))
    ldi R19, high(((ASM_SET_LEDS)<<(1))|((1)<<(15)))
    ldi R18, low(((ASM_SET_LEDS)<<(1))|((1)<<(15)))
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    movw R19:R18, R29:R28
    call GC_MAKE_CONS
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    ldi R17, high(((PLUS_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((PLUS_SYM)<<(1))|((1)<<(15)))
    ldi R19, high(((ASM_PLUS)<<(1))|((1)<<(15)))
    ldi R18, low(((ASM_PLUS)<<(1))|((1)<<(15)))
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    movw R19:R18, R29:R28
    call GC_MAKE_CONS
    movw R29:R28, RET_PTR_HI:RET_PTR_LO
    ldi R17, high(((LET_SYM)<<(1))|((1)<<(15)))
    ldi R16, low(((LET_SYM)<<(1))|((1)<<(15)))
    ldi R19, high(((ASM_LET)<<(1))|((1)<<(15)))
    ldi R18, low(((ASM_LET)<<(1))|((1)<<(15)))
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    movw R19:R18, R29:R28
    call GC_MAKE_CONS
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
    ldi R28, low(DYNAMIC_VARS_PTR)
    ldi R29, high(DYNAMIC_VARS_PTR)
    st Y+, R16
    st Y, R17
    ;; Zero out the current lexical frame
    ldi R28, low(CURRENT_LEXICAL_FRAME)
    ldi R29, high(CURRENT_LEXICAL_FRAME)
    st Y+, ZERO
    st Y, ZERO
    pop R16
    pop R17
    pop R18
    pop R19
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

READTABLE_INDEX:
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
 ;;; Indexes the given lisp array (of pointers) at the given position
    ;; Ensure this is a readtable
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_749
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_750
LOAD_SRAM_GCPTR_749:
    ld R16, Z+
DONE_LOAD_GCPTR_750:
    cpi R16, READTABLE_OBJECT_ID
    breq SUCCESS_CONDITION_END_751
    call FATAL_ERROR
SUCCESS_CONDITION_END_751:
    ;; Skip the length
    adiw R31:R30, 2
    lsl R18
    rol R19
    ;; Multiply the index by two to turn bytes into words and add it to the array pointer
    add R30, R18
    adc R31, R19
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_752
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_753
LOAD_SRAM_GCPTR_752:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_753:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_754
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_755
LOAD_SRAM_GCPTR_754:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_755:
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
    brne SUCCESS_CONDITION_END_758
    ldi R17, 0
    rjmp IS_SYM_756
SUCCESS_CONDITION_END_758:
    ;; Test for carriage return
    ldi R17, 10
    cp R16, R17
    brne SUCCESS_CONDITION_END_759
    ldi R17, 1
    rjmp IS_SYM_756
SUCCESS_CONDITION_END_759:
    ;; Test for line feed
    ldi R17, 13
    cp R16, R17
    brne SUCCESS_CONDITION_END_760
    ldi R17, 1
    rjmp IS_SYM_756
SUCCESS_CONDITION_END_760:
    ;; Test for ASCII range [32, 97) (save r16 - 30 in r17 in case it's in this range)
    subi R16, 30
    mov R17, R16
    subi R16, 2
 ; If r16 is negative, it's too low
    brmi IS_NOT_SYM_757
    subi R16, 65
 ; If r16 is negative, it's good
    brmi IS_SYM_756
    ;; Test for ASCII range [123, 127) in a similar manner (but keep in mind we've already sub'd 97)
    subi R16, 26
    brmi IS_NOT_SYM_757
    ldi R17, 67
    add R17, R16
    subi R16, 4
    brmi IS_SYM_756
    rjmp IS_NOT_SYM_757
IS_SYM_756:
    inc RET_NONPTR_HI
    mov RET_NONPTR_LO, R17
IS_NOT_SYM_757:
    pop R17
    pop R18
    pop R16
    ret

ARRAY_INDEX:
    push R19
    push R18
    push R17
    push R16
    push R30
    push R31
    movw R31:R30, R17:R16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_761
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_762
LOAD_SRAM_GCPTR_761:
    ld R16, Z+
DONE_LOAD_GCPTR_762:
    cpi R16, ARRAY_OBJECT_ID
    breq SUCCESS_CONDITION_END_763
    call FATAL_ERROR
SUCCESS_CONDITION_END_763:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_764
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_765
LOAD_SRAM_GCPTR_764:
    ld R16, Z+
DONE_LOAD_GCPTR_765:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_766
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_767
LOAD_SRAM_GCPTR_766:
    ld R17, Z+
DONE_LOAD_GCPTR_767:
    cp R18, R16
    cpc R19, R17
    brlo SUCCESS_CONDITION_END_768
    call FATAL_ERROR
SUCCESS_CONDITION_END_768:
    lsl R18
    rol R19
    ;; Multiply the index by two to turn bytes into words and add it to the array pointer
    add R30, R18
    adc R31, R19
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_769
    andi R31, 127
    lpm RET_PTR_LO, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_770
LOAD_SRAM_GCPTR_769:
    ld RET_PTR_LO, Z+
DONE_LOAD_GCPTR_770:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_771
    andi R31, 127
    lpm RET_PTR_HI, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_772
LOAD_SRAM_GCPTR_771:
    ld RET_PTR_HI, Z
DONE_LOAD_GCPTR_772:
    pop R31
    pop R30
    pop R16
    pop R17
    pop R18
    pop R19
    ret

ASM_STORE_FUNC_ARGS_MAPC_BODY:
    dec R20
 ; Skip the symbol pointers
    adiw R27:R26, 2
    movw R17:R16, R31:R30
    ;; Evaluate the argument if this is a normal function
    cpi R21, FUNCTION_TYPE_FUNC
    brne SUCCESS_CONDITION_END_773
    call ASM_EVAL
    movw R17:R16, RET_PTR_HI:RET_PTR_LO
SUCCESS_CONDITION_END_773:
    st X+, R16
    st X+, R17
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
BEGIN_LOOP_774:
    cp R30, ZERO
    cpc R31, ZERO
    breq SUCCESS_CONDITION_END_776
    ;; Increment Z to skip the header byte
    adiw R31:R30, 1
    ;; Load the CAR of the list into r17:r16
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_777
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_778
LOAD_SRAM_GCPTR_777:
    ld R16, Z+
DONE_LOAD_GCPTR_778:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_779
    andi R31, 127
    lpm R17, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_780
LOAD_SRAM_GCPTR_779:
    ld R17, Z+
DONE_LOAD_GCPTR_780:
    ;; Preserve Z in r23:r22
    movw R23:R22, R31:R30
    ;; load the callback into Z
    movw R31:R30, R21:R20
    ;; Call the callback to see if this is the return value
    icall
    ;; If it returned nonzero, stop and return
    tst RET_NONPTR_LO
    breq SUCCESS_CONDITION_END_781
    jmp END_LOOP_775
SUCCESS_CONDITION_END_781:
    ;; Restore Z to the pointer to the list to search
    movw R31:R30, R23:R22
    ;; If it's not the one, put the CDR into Z and try again
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_782
    andi R31, 127
    lpm R16, Z+
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_783
LOAD_SRAM_GCPTR_782:
    ld R16, Z+
DONE_LOAD_GCPTR_783:
    sbrs R31, 7
    rjmp LOAD_SRAM_GCPTR_784
    andi R31, 127
    lpm R17, Z
    ori R31, 128
    rjmp DONE_LOAD_GCPTR_785
LOAD_SRAM_GCPTR_784:
    ld R17, Z
DONE_LOAD_GCPTR_785:
    movw R31:R30, R17:R16
    jmp BEGIN_LOOP_774
SUCCESS_CONDITION_END_776:
END_LOOP_775:
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
BEGIN_LOOP_786:
    cp R20, ZERO
    cpc R21, ZERO
    breq SUCCESS_CONDITION_END_788
    subi R20, 1
    ldi R25, 0
    sbc R21, R25
    ld R22, Y+
    st X+, R22
    jmp BEGIN_LOOP_786
SUCCESS_CONDITION_END_788:
END_LOOP_787:
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
    brlo SUCCESS_CONDITION_END_789
    jmp FATAL_ERROR
SUCCESS_CONDITION_END_789:
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
