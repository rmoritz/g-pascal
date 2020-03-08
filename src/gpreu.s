; -*- asm -*-
;***********************************************
; PASCAL COMPILER
; for Commodore 64
; PART 0 - REU functions
; Authors: Ralph Moeritz
;***********************************************

        .org $33C               ; tape buffer

;***********************************************
; G-PASCAL POINTERS & ADDRESSES
;***********************************************

        P       = $26
        TS      = $8009
	P1	= $8013
	P2	= $8DD4
	P3	= $992E
	P4	= $A380
	P5	= $B384
	P6	= $BCB8

;***********************************************
; G-PASCAL VECTORS
;***********************************************

        FND_END	= P1+102
        CHK_VAL = P3+403

;***********************************************
; TXT2REU - Backup Pascal sources to REU
;***********************************************
TXT2REU:
        JSR FND_END
        ;; P contains text end address
        ;; Subtract TS from P to determine xfer length
        SEC
        LDA P
        SBC TS
        STA XFER_LEN
        LDA P+1
        SBC TS+1
        STA XFER_LEN+1
        LDA #$90
        JMP INIT_REU
;***********************************************
; REU2TXT - Restore Pascal sources from REU
;***********************************************
REU2TXT:
        LDA #$91
INIT_REU:
        STA REU_ARGS            ; set command
        LDX #<TS                ; set C64 base address
        LDY #>TS
        STX REU_ARGS+1
        STY REU_ARGS+2
        LDX #<XFER_LEN          ; set xfer length
        LDY #>XFER_LEN
        STX REU_ARGS+6
        STY REU_ARGS+7
        LDX #9
REU_LOOP:
        LDA REU_ARGS,X
        STA $DF01,X
        DEX
        BPL REU_LOOP
        JSR CHK_VAL
        RTS

XFER_LEN:
        .word 0
REU_ARGS:
        .byte 0                 ; REC command
        .word 0                 ; C64 base address
        .word 0
        .byte 0
        .word 0                 ; xfer length
        .byte 0,0
