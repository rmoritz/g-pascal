; -*- asm -*-
;***********************************************
; pascal compiler
; for commodore 64
; part 0 - reu functions
; authors: ralph moeritz
;***********************************************

        .org $7f00

;***********************************************
; g-pascal pointers & addresses
;***********************************************

        p       = $26
        ts      = $8009
	p1	= $8013
	p3	= $992e

;***********************************************
; g-pascal vectors
;***********************************************

        fnd_end	= p1+102
        chk_val = p3+403
        initio  = p3+649

;***********************************************
; reu constants & addresses
;***********************************************

        write_cmd    = $90
        read_cmd     = $91
        reu_args_len = 9
        reu_addr     = $df01

;***********************************************
; txt2reu - backup pascal sources to reu
;***********************************************

txt2reu:
        ;; ts is the address of the pascal source.
        ;; set it as the source/destination
        ;; address for the copy operation.
        ldx ts
        ldy ts+1
        stx reu_args+1
        sty reu_args+2

        ;; determine the end address of the sources.
        jsr fnd_end
        ;; p contains the text end address. subtract
        ;; ts from p to determine the transfer length.
        sec
        lda p
        sbc ts
        sta reu_args+6
        lda p+1
        sbc ts+1
        sta reu_args+7
        clc

        lda #write_cmd
        jmp init_reu

;***********************************************
; reu2txt - restore pascal sources from reu
;***********************************************

reu2txt:
        lda #read_cmd

init_reu:
        sta reu_args            ; set command
        ldx #reu_args_len

reu_loop:
        ;; store copy operation arguments in reu
        ;; command register to initiate copy.
        lda reu_args,x
        sta reu_addr,x
        dex
        bpl reu_loop

        ;; return to g-pascal
        lda reu_args
        cmp #read_cmd
        bcc writing
reading:
        jsr initio
        jmp exit
writing:
        jsr chk_val
exit:
        rts

reu_args:
        .byte 0                 ; rec command
        .word 0                 ; c64 base address
        .word 0                 ; reu base address
        .byte 0                 ; reu bank
        .word 0                 ; transfer length
        .byte 0                 ; irq mask
        .byte 0                 ; address control register
