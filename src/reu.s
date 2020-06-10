; -*- asm -*-
; Use tab width of 20 chars to display correctly
;***********************************************
; pascal compiler for commodore 64
; part 0 - reu routines
; authors: ralph moeritz
;***********************************************

	.org $7f00

;***********************************************
; g-pascal pointers & addresses
;***********************************************

	p = $26
	ts = $8009
	p1 = $8013
	p3 = $992e

;***********************************************
; g-pascal vectors
;***********************************************

	fnd_end = p1+102
	chk_val = p3+403
	initio = p3+649

;***********************************************
; reu constants & addresses
;***********************************************

	write_cmd = $90
	read_cmd = $91
	reu_args_len = 9
	reu_addr = $df01

;***********************************************
; resetreu - reset reu args to default values
;***********************************************

resetreu:	
	lda #0
	ldx #reu_args_len
reset_loop:	
	sta reu_args,x
	dex
	bpl reset_loop		
	rts		

;**************************************************
; zp2reu - backup page 0 ($02-$ff) to reu (bank 0)
;**************************************************

zp2reu:
	jsr resetreu
	lda #$02	
	sta reu_args+1	; set c64 base address (lo)	
	lda #$fd
	sta reu_args+6	; set transfer length
	lda #write_cmd
	jmp init_zpcpy

;*****************************************************
; reu2zp - restore page 0 ($02-$ff) from reu (bank 0)
;
; remarks:
; - must be called after zp2reu
; - no other reu routines must be called between
;   calling zp2reu and reu2zp!
;*****************************************************

reu2zp:
	lda #read_cmd	
	
init_zpcpy:
	sta reu_args	; set rec command		
	ldx #reu_args_len
zpcpy_loop:
;; store copy operation arguments in reu
;; command register to initiate copy.
	lda reu_args,x
	sta reu_addr,x
	dex
	bpl zpcpy_loop	
	rts	

;*************************************************
; txt2reu - backup pascal sources to reu (bank 1),
;           call chk_val, and return.
;*************************************************

txt2reu:
	jsr resetreu
;; ts is the address of the pascal sources.
;; set it as the source/destination
;; address for the copy operation.
	ldx ts
	ldy ts+1
	stx reu_args+1	; set c64 base address (lo)
	sty reu_args+2	; set c64 base address (hi)

;; determine the end address of the sources.
	jsr fnd_end
;; p contains the text end address. subtract
;; ts from p to determine the transfer length.
	sec
	lda p
	sbc ts
	sta reu_args+6	; set transfer length (lo)
	lda p+1
	sbc ts+1
	sta reu_args+7	; set transfer length (hi)
	clc

	lda #01
	sta reu_args+5	; set reu bank
	lda #write_cmd
	jmp init_txtcpy

;****************************************************
; reu2txt - restore pascal sources from reu (bank 1),
;           call initio, and return.
;
; remarks:
; - must be called after txt2reu
; - no other reu routines must be called between
;   calling txt2reu and reu2txt!
;****************************************************

reu2txt:
	lda #read_cmd

init_txtcpy:
	sta reu_args	; set rec command
	ldx #reu_args_len

txtcpy_loop:
;; store copy operation arguments in reu
;; command register to initiate copy.
	lda reu_args,x
	sta reu_addr,x
	dex
	bpl txtcpy_loop

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
	.byte 0	; rec command
	.word 0	; c64 base address
	.word 0	; reu base address
	.byte 0	; reu bank
	.word 0	; transfer length
	.byte 0	; irq mask
	.byte 0	; address control register
