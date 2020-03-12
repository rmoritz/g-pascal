; -*- asm -*-
;***********************************************
; PASCAL COMPILER
; for Commodore 64
; PART 0 - REU functions
; Authors: Ralph Moeritz
;***********************************************

        .org $7F00

;***********************************************
; G-PASCAL POINTERS & ADDRESSES
;***********************************************

        P       = $26
        TS      = $8009
	P1	= $8013
	P3	= $992E

;***********************************************
; G-PASCAL VECTORS
;***********************************************

        FND_END	= P1+102
        CHK_VAL = P3+403
        INITIO  = P3+649

;***********************************************
; REU CONSTANTS & ADDRESSES
;***********************************************

        WRITE_CMD    = $90
        READ_CMD     = $91
        REU_ARGS_LEN = 9
        REU_ADDR     = $DF01

;***********************************************
; TXT2REU - Backup Pascal sources to REU
;***********************************************

TXT2REU:
        ;; TS is the address of the Pascal source.
        ;; Set it as the source/destination
        ;; address for the copy operation.
        LDX TS
        LDY TS+1
        STX REU_ARGS+1
        STY REU_ARGS+2

        ;; Determine the end address of the sources.
        JSR FND_END
        ;; P contains the text end address. Subtract
        ;; TS from P to determine the transfer length.
        SEC
        LDA P
        SBC TS
        STA REU_ARGS+6
        LDA P+1
        SBC TS+1
        STA REU_ARGS+7
        CLC

        LDA #WRITE_CMD
        JMP INIT_REU

;***********************************************
; REU2TXT - Restore Pascal sources from REU
;***********************************************

REU2TXT:
        LDA #READ_CMD

INIT_REU:
        STA REU_ARGS            ; set command
        LDX #REU_ARGS_LEN

REU_LOOP:
        ;; Store copy operation arguments in REU
        ;; command register to initiate copy.
        LDA REU_ARGS,X
        STA REU_ADDR,X
        DEX
        BPL REU_LOOP

        ;; Return to G-Pascal
        LDA REU_ARGS
        CMP #READ_CMD
        BCC WRITING
READING:
        JSR INITIO
        JMP EXIT
WRITING:
        JSR CHK_VAL
EXIT:
        RTS

REU_ARGS:
        .byte 0                 ; REC command
        .word 0                 ; C64 base address
        .word 0                 ; REU base address
        .byte 0                 ; REU bank
        .word 0                 ; transfer length
        .byte 0                 ; IRQ mask
        .byte 0                 ; address control register
