; -*- asm -*-
; Use tab width of 20 chars to display correctly
; G-Pascal C64 REU-enhanced 3.1 2020-06-05 rmoritz

;*******************************************************************
;
; Conversion to CC65 Assembler format and other mungings by Chris
; Baird <cjb@brushtail.apana.org.au> in 2012. Addition of REU routines
; by Ralph Moeritz in 2020.
;
; To build:
;	ca65 gpascal.a
; 	[...0.16 seconds later!]
;	ld65 -o gpascal.bin -t none -S 32768 gpascal.o
;
; Or as I have in a Makefile:
;
; all:
;	ca65 -l gpascal.lst gpascal.a
;	ld65 -t none -S 32768 gpascal.o
;	dd if=a.out bs=1 skip=88 of=0.
;	printf "\000\200" >gpascal.prg
;	cat 0. >>gpascal.prg
;	rm -f 0. a.out
;
; As this is a derivative work, you probably don't want to bother Nick
; about the hacks I've done to it... And I (cjb) don't really intend
; to maintain or develop this Commodore64 version of the software.
; (From now on, I'll be working on porting it to other 6502 systems in
; my collection that've begged for their own Pascal for years!)
;
; This file has a number of totally ugly assembler hacks left in, both
; mine and Nick's, but have been left in for the goal of making it
; byte-perfect with the original code.
;
;*******************************************************************

;********************************************************************
;
; COPYRIGHT
;
; GPascal is copyright 1986 to 2011 by Nick Gammon. All rights
; reserved worldwide.
;
; GPascal is not in the public domain and Nick Gammon keeps its
; copyright.
;
; PERMISSION TO DISTRIBUTE
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; LIMITATION OF LIABILITY
;
; The software is provided "as is", without warranty of any kind,
; express or implied, including but not limited to the warranties of
; merchantability, fitness for a particular purpose and
; noninfringement. In no event shall the authors or copyright holders
; be liable for any claim, damages or other liability, whether in an
; action of contract, tort or otherwise, arising from, out of or in
; connection with the software or the use or other dealings in the
; software.
;
;*******************************************************************

;*******************************************************************
;
; For further information on this software, including the history of
; how it came to be, visit
; http://www.supercoders.com.au/blog/nickgammongpascal.shtml and
; http://www.gammon.com.au/GPascal/source/
;
;*******************************************************************

;************************************************
; g-pascal compiler
; for commodore 64
; part 1
; authors_ nick gammon & sue gobbett
; copyright (c) 1986 - gambit games.
;***********************************************

	new_edi = $0b00
	
	p0 = $7f00
	p1 = $8013
	p2 = $8dd4
	p3 = $992e
	p4 = $a380
	p5 = $b384
	p6 = $bcb8
;
	stack = $100
	inbuf = $33c
	kbd_buf = $277
	himem = $283
	color = $286
	hibase = $288
	autodn = $292
	bitnum = $298
	baudof = $299
	rodbs = $29d
	rodbe = $29e
	enabrs = $2a1	; rs232 enables
	warm_str = $302	; basic warm start vector
	cinv = $314	; hardware interrupt vector
;
	space = $20
	cr = $d
	ff = $c
	lf = $a
	max_stk = 32
	new_stk = $ff
;
	vic = $d000
	sid = $d400
	cia1 = $dc00
	cia2 = $dd00
	datareg = $dd01
	ddrb = $dd03
	flag = $dd0d
	border = $d020
	bkgnd = $d021
;
	cout = $ffd2
	stop = $ffe1
	getin = $ffe4
	chkout = $ffc9
	clrchn = $ffcc
	unlsn = $ffae
	untkl = $ffab
	chrin = $ffcf
	chkin = $ffc6
	plot = $fff0
	chrout = $ffd2
	cint = $ff81
	ioinit = $ff84
	clall = $ffe7
	setmsg = $ff90
	setlfs = $ffba
	setnam = $ffbd
	open = $ffc0
	load = $ffd5
	readst = $ffb7
	save = $ffd8
	ramtas = $ff87
	restor = $ff8a
	memtop = $ff99
	untlk = $ffab
	close = $ffc3

;***********************************************
; pascal work areas
;***********************************************

	line_cnt = $2	; 2 bytes
	line_no = line_cnt
	reg = $4	; 2 bytes
	rowl = reg
	rowh = reg+1
	srce = reg
	reg2 = $6	; 2 bytes
	dest = reg2
	wx = $8	; 2 bytes
	err_rtn = $b	; 2 bytes
	symtbl = $d
	token = $16
	tknadr = $17	; 2 bytes
	tknlen = $19
	eof = $1a
	list = $1b
	nxtchr = $1c	; 2 bytes
	value = $1e	; 3 bytes
	digit = $21
	notrsv = $22
	frame = $23	; 2 bytes
	level = $25
	pcode = $26
	p = pcode
	pntr = pcode
	act_pcda = $28	; 2 bytes
	displ = $2a	; 2 bytes
	offset = $2c	; 2 bytes
	opnd = $2e	; 3 bytes
	dcode = $31
	endsym = $32	; 2 bytes
	arg = $34
	prompt = $35
	workd = $36	; 2 bytes
	errno = $38
	rtnadr = $39	; 2 bytes
	bsave = $3b
	work = $3c	; 2 bytes
	prcitm = $3e	; 2 bytes
	dspwrk = $40	; 2 bytes
	pflag = $42
	t = endsym	; stack pointer 2 bytes
	tmp_pntr =  t
	base = $45	; 2 bytes
	to = base
	data = $47	; 2 bytes
	running = $49
	upr_case = $4a
	sce_lim = $4b	; 2 bytes
	function = sce_lim
	spriteno = sce_lim+1
	stk_use = $4d
	voiceno = stk_use
	symitm = $4e	; 2 bytes
	from = symitm
	syntax = $50
	chk_ary = $51
	secret = $52
	val_cmp = $53
	ctrlc_rt = $54	; 2 bytes
	end_pcd = $56	; 2 bytes
	regb = $58
	reg2b = $59
	leftcol = $5a
	sign = $5b
	temp = $5c	; 2 bytes
	call = $5e	; 2 bytes
	count = $60
	lncnt = $61
	ls = $62
	pcsvd = $63	; 2 bytes
	first = $65
	dbgtyp = $66
	dbgflg = $67
	defp = $68	; 2 bytes
	defs = $6a	; 2 bytes
	dattyp = $6c
	dos_flg = dattyp
	a5 = $6d	; 2 bytes
	mask = a5
	coll_reg = a5+1
	st = $90
	dfltn = $99	; input device
	queue = $c6
	indx = $c8
	lxsp = $c9
	blnsw = $cc
	blnon = $cf
	crsw = $d0
	basl = $d1
	ch = $d3
;
	p_stack = $ced0	; p-code stack
	s_animct = $ced8	; count of frames
	s_animps = $cee0	; current position
	s_animcc = $cee8	; current frame count
	s_animfm = $cef0	; no. of frames
	s_pointr = $cef8	; pointers - 16 per sprite
	sid_img = $cf7c
	s_active = $cf98
	s_xpos = $cfa0	; 3 bytes each
	s_ypos = $cfb8	; 2 bytes each
	s_xinc = $cfc8	; 3 bytes each
	s_yinc = $cfe0	; 2 bytes each
	s_count = $cff0	; 2 bytes each
;
	.org $02a7

count1:	.res 1
count2:	.res 1
sym_use:	.res 2	; 2 bytes
savcur:	.res 6	; 6 bytes
bpoint:	.res 20

	call_p = bpoint
	call_a = bpoint+1
	call_x = bpoint+2
	call_y = bpoint+3
	fnc_val = bpoint+15
	remain = bpoint+4
	xposl = bpoint+15
	xposh = bpoint+16
	ypos = bpoint+17
	cntr = bpoint+10
	ecntr = bpoint
	epntr = $43
	rep_from = bpoint+2
	rep_to = bpoint+3
	rep_len = bpoint+4
	pntr_hi = bpoint+5
	in_lgth = bpoint+6
	length = bpoint+7
	from_st = bpoint+9
	num_lins = bpoint+11
	ed_com = bpoint+13
	to_line = bpoint+15
	fnd_from = bpoint+17
	fnd_to = bpoint+18
	fnd_pos = bpoint+19

lastp:	.res 2
inchar:	.res 1
io_a:	.res 1
io_y:	.res 1
io_x:	.res 1
curr_chr:	.res 1
hex_wk:	.res 1
	.res 2
stk_avl:	.res 1
stk_page:	.res 1
stk_wrk:	.res 1
stk_rt:	.res 2
beg_stk:	.res 1
xsave:	.res 1
res:	.res 3	; 3 bytes
mcand:	.res 3	; 3 bytes
	divisor = mcand
dvdn:	.res 3	; 3 bytes
rmndr:	.res 1
temp1:	.res 2
bin_wrk:	.res 3
asc_wrk:	.res 10
def_pcd:	.res 1
rep_size:	.res 1
nln_flag:	.res 1
q_flag:	.res 1
fnd_flg:	.res 1
fnd_len:	.res 1
uc_flag:	.res 1
trn_flag:	.res 1
glb_flag:	.res 1
int_rtn:	.res 2	; address to return to after a timer interrupt
int_temp:	.res 1	; for interrupt service routine
int_tmp1:	.res 1
int_tmp2:	.res 1
qt_tgl:	.res 1	; quote toggle
qt_size:	.res 1	; number of characters in reserved words

;***********************************************
; address constants etc.
;***********************************************

	.org $8000
	jmp start
	jmp restart

	.res 3
ts:	.word $4000
sym_size:	.byte 16
lhb:	.byte "["
rhb:	.byte "]"
quot_sym:	.byte 34	; quote symbol
delimit:	.byte "."	; find/replace delimiter
pr_chan:	.byte 4	; printer channel
disk_chn:	.byte 8	; disk channel
	.byte 0	; spare for now

;***********************************************
; symbol table stuff
;***********************************************

	symprv =  0
	symlvl =  2
	symtyp =  3
	symdsp =  4
	symarg =  6
	symsub = 6	; max subscript+1
	symdat = 8	; variable type
	symlen =  9
	symnam = 10	; name

;***********************************************
; part 3 start
;***********************************************

	start = p3
	restart = start+3
	.scope 
	exit_cmp = restart
	dsp_bin = p3+6
	st_cmp = p3+9
	st_syn = p3+12
	debug = p3+15
	bell1x = p3+24

;***********************************************
; vectors
;***********************************************

	jmp init
	jmp getnext
	jmp comstl
	jmp isithx
	jmp isital
	jmp isitnm
	jmp char
	jmp gen2_b
	jmp dishx
	jmp error
	jmp getchk
	jmp chktkn
	jmp gennop
	jmp genadr
	jmp gennjp
	jmp gennjm
	jmp tknwrk
	jmp prbyte
	jmp gtoken
	jmp wrktkn
	jmp fixad
	jmp pshwrk
	jmp pulwrk
	jmp pc
	jmp pt
	jmp pl
	jmp token1
	jmp getans
	jmp putsp
	jmp dispad
	jmp crout
	jmp shlval
	jmp get_num
	jmp get_hex
	jmp fnd_end
	jmp pause
	jmp home
	jmp rdkey
	jmp genjmp
	jmp genrjmp

; these are dict token values to produce the
; following..
; 5,14,"g-pASCAL",
; "COMPILER", ""vERSION 3.1 sER# 5001",13,
; "wRITTEN BY nICK gAMMON ","AND sUE gOBBETT",13
; "cOPYRIGHT 1983","gAMBIT",gAMES",
; "p.o. bOX 124 iVANHOE 3079 vIC aUSTRALIA",13
us:	.byte $d1,$ba,$d2,$d6,$d3,$ce,$cf,$d0


;***********************************************
; part 5 vectors
;***********************************************

	editor = p5
	put_line = p5+12

;***********************************************
; part 6 vectors
;***********************************************

	block = p6

;***********************************************
;
; initialize
;
;***********************************************

nosce:	.byte "NO",$bf,$0d	; "NO SOURCE\n"

init:	lda #0
	sta eof
	sta list
	sta level
	sta dcode
	sta running
	sta prcitm
	sta prcitm+1
	sta line_cnt
	sta line_cnt+1
	sta regb
	lda #<us
	ldx #>us
	ldy #8
	jsr pt
	lda symtbl+1
	sta sym_use+1
	sta endsym+1
	lda ts
	sec 
	sbc #1
	sta nxtchr
	lda ts+1
	sbc #0
	sta nxtchr+1
	jsr fnd_end
	lda p
	sta act_pcda
	lda p+1
	sta act_pcda+1
	jsr line
	lda eof
	beq got_end2
	lda #<nosce
	ldx #>nosce
	jsr pl
	jmp errrtn

;***********************************************
; find text end
;***********************************************

fnd_end:
	lda ts
	sta pcode
	lda ts+1
	sta pcode+1
	ldy #0
fnd_txte:
	lda (p),y
	beq got_ends
	inc p
	bne fnd_txte
	inc p+1
	bne fnd_txte
got_ends:
	inc p
	bne got_end2
	inc p+1
got_end2:
	rts 

;***********************************************
; get next char from input
;***********************************************

getnext:
	ldy #1
	lda (nxtchr),y
line4:	rts

;***********************************************
; input a line
;***********************************************

line:	jsr getnext
	bne line1	; null= eof
	inc eof
	rts 
line1:
	lda nxtchr
	clc 
	adc #1
	sta a5
	lda nxtchr+1
	adc #0
	sta a5+1
	inc line_cnt
	bne line3
	inc line_cnt+1
line3:
	lda list
	beq line2
	jsr disp
	rts 
line2:
	lda line_cnt
	and #15
	bne line4
	lda #'*'
	jmp pc

;***********************************************
; get a character
;***********************************************

char:
	inc nxtchr
	bne char2
	inc nxtchr+1
char2:
	ldy #0
	lda (nxtchr),y
	cmp #cr
	bne char1
	jsr line	; end of line
	lda eof
	beq char
	lda #0	; end of file marker
char1:
	sta curr_chr
	rts 

crout:	lda  #cr
	jmp pc

;***********************************************
; compare string
;***********************************************

comstl:	dey
	bmi coms8
	lda (srce),y
	cmp #$c1
	bcc coms1	; blt = "bcc"
	cmp #$db
	bcs coms1	; bge = "bcs"
	and #$7f	; convert to u/c
coms1:
	cmp (dest),y
	beq comstl
coms9:	rts	; not equal
coms8:	lda #0
	rts 	; equal

;***********************************************
; is it hex?
;***********************************************

isithx:	cmp #'0'
	bcc nothx
	cmp #'9'+1
	bcc ishx
	cmp #'A'
	bcc nothx
	cmp #'F'+1
	bcc ishx_a
	cmp #$c1
	bcc nothx
	cmp #$c7
	bcs nothx
	and #$7f	; convert to upper case
ishx_a:
	sec 
	sbc #7
ishx:
	sec 
	sbc #'0'
	clc 
	rts 
nothx:
	sec 
	rts 

;***********************************************
; is it alpha
;***********************************************

isital:	cmp #'A'
	bcc notal
	cmp #'Z'+1
	bcc isal
	cmp #$c1
	bcc notal
	cmp #$db
	bcc isal
notal:
	sec 
	rts 
isal:
	clc 
	rts 

;***********************************************
; is it numeric?
;***********************************************

isitnm:	cmp #'0'
	bcc notnum
	cmp #'9'+1
	bcc isnum
notnum:
	sec 
	rts 
isnum:
	clc 
	rts 

;***********************************************
;
; get token !!!
; *********
;
; reserved word table
;
;***********************************************

rsvwrd:	.byte 3, $81, "GET"
	.byte 5, $82, "CONST"
	.byte 3, $83, "VAR"
	.byte 5, $84, "ARRAY"
	.byte 2, $85, "OF"
	.byte 9, $86, "PROCEDURE"
	.byte 8, $87, "FUNCTION"
	.byte 5, $88, "BEGIN"
	.byte 3, $89, "END"
	.byte 2, $8a, "OR"
	.byte 3, $8b, "DIV"
	.byte 3, $8c, "MOD"
	.byte 3, $8d, "AND"
	.byte 3, $8e, "SHL"
	.byte 3, $8f, "SHR"
	.byte 3, $90, "NOT"
	.byte 3, $91, "MEM"
	.byte 2, $92, "IF"
	.byte 4, $93, "THEN"
	.byte 4, $94, "ELSE"
	.byte 4, $95, "CASE"
	.byte 5, $96, "WHILE"
	.byte 2, $97, "DO"
	.byte 6, $98, "REPEAT"
	.byte 5, $99, "UNTIL"
	.byte 3, $9a, "FOR"
	.byte 2, $9b, "TO"
	.byte 6, $9c, "DOWNTO"
	.byte 5, $9d, "WRITE"
	.byte 4, $9e, "READ"
	.byte 4, $9f, "CALL"
; $a0 not used because of clash with shift/space
	.byte 4, $a1, "CHAR"
	.byte 4, $a2, "MEMC"
	.byte 6, $a3, "CURSOR"
	.byte 3, $a4, "XOR"
	.byte 12,$a5, "DEFINESPRITE"
	.byte 4, $a6, "PLOT"
	.byte 6, $a7, "GETKEY"
	.byte 5, $a8, "CLEAR"
	.byte 7, $a9, "ADDRESS"
	.byte 4, $aa, "WAIT"
	.byte 3, $ab, "CHR"
	.byte 3, $ac, "HEX"
	.byte 12,$ad, "SPRITEFREEZE"
	.byte 5, $ae, "CLOSE"
	.byte 3, $af, "PUT"
	.byte 6, $df, "SPRITE"
	.byte 14,$e0, "POSITIONSPRITE"
	.byte 5, $e1, "VOICE"
	.byte 8, $e2, "GRAPHICS"
	.byte 5, $e3, "SOUND"
	.byte 8, $e4, "SETCLOCK"
	.byte 6, $e5, "SCROLL"
	.byte 13,$e6, "SPRITECOLLIDE"
	.byte 13,$e7, "GROUNDCOLLIDE"
	.byte 7, $e8, "CURSORX"
	.byte 7, $e9, "CURSORY"
	.byte 5, $ea, "CLOCK"
	.byte 6, $eb, "PADDLE"
	.byte 7, $ec, "SPRITEX"
	.byte 8, $ed, "JOYSTICK"
	.byte 7, $ee, "SPRITEY"
	.byte 6, $ef, "RANDOM"
	.byte 8, $f0, "ENVELOPE"
	.byte 7, $f1, "SCROLLX"
	.byte 7, $f2, "SCROLLY"
	.byte 12,$f3, "SPRITESTATUS"
	.byte 10,$f4, "MOVESPRITE"
	.byte 10,$f5, "STOPSPRITE"
	.byte 11,$f6, "STARTSPRITE"
	.byte 13,$f7, "ANIMATESPRITE"
	.byte 3, $f8, "ABS"
	.byte 7, $f9, "INVALID"
	.byte 4, $fa, "LOAD"
	.byte 4, $fb, "SAVE"
	.byte 4, $fc, "OPEN"
	.byte 12,$fd, "FREEZESTATUS"
	.byte 7, $fe, "INTEGER"
	.byte 7, $ff, "WRITELN"
rsvend:	.byte 0,0	; end of table


gettkn:
gtoken:	jsr char
	bne token1	; zero= eof
	sta token
	rts 
token1:
	cmp #32
	beq gtoken	; bypass space
	cmp #160	; also bypass shift/space
	beq gtoken
	cmp #$10	; dle?
	bne token1_a
	inc nxtchr
	bne dle_ok
	inc nxtchr+1
dle_ok:
	jmp gtoken
token1_a:
	cmp #'('
	beq token1_b
	jmp token2
token1_b:
	jsr getnext
	cmp #'*'
	beq token3	; comment
	jmp tkn26_a	; back to process '('
token3:		; bypass comments
	jsr char
	bne token4
	ldx #7	; no } found
	jsr error
token4:
	cmp #$10	; dle
	bne token4_a	; n
	inc nxtchr	; bypass count
	bne token3
	inc nxtchr+1
	bne token3
token4_a:
	cmp #'%'	; compiler directive?
	bne token4_b	; no such luck
	jsr char	; and what might the directive be?
	and #$7f	; convert case
	cmp #'A'	; address of p-codes?
	bne token4_d	; nope
	jsr gtoken	; re-call gtoken to find the address
	cmp #'N'	; number?
	beq token4_c	; yes
	ldx #2
	jsr error	; 'constant expected'
token4_c:
	lda value
	sta p
	sta act_pcda	; also save for run
	lda value+1
	sta p+1	; store new p-code address
	sta act_pcda+1
	cmp #$08	; too low?
	bcc token4_i	; yes - error
	cmp #$70	; too high?
	bcc token3j	; nope
	bne token4_i	; yes
;
; here if address is $70xx - check that xx is zero
;
	lda p
	beq token3j	; yes - thank goodnes

; here if address is outside range $0800 to $7000
;
token4_i:
	ldx #30
	jsr error	; crash it

token3j:
	jmp token3	; back again
token4_d:
	cmp #'L'	; commence listing?
	bne token4_e	; nope
token4_h:
	pha 
	lda list	; already listing the file?
	bne token4_f	; yep
	jsr disp	; no - so display this line then
token4_f:
	pla 
	sta list	; set listing flag
	jmp token3	; back for next comment
;
token4_e:
	cmp #'P'	; p-codes list?
	bne token4_g	; nope
	sta dcode	; set display p-code flag
	beq token4_h	; go back as if for list
;
token4_g:
	eor #'N'	; no-list?
	bne token4_b	; nope - forget it then
	sta dcode
	sta list	; (a) must be zero - clear both flags
	beq token3j	; back for next comment
;
;
token4_b:
	cmp #'*'
	bne token3j
	jsr getnext
	cmp #')'
	bne token3j
	jsr char	; bypass *
	jmp gtoken
;
; here if not comment or space
;
token2:
	ldx nxtchr
	stx tknadr
	ldx nxtchr+1
	stx tknadr+1
	ldx #1
	stx tknlen	; default length=1
	dex 
	stx sign
	stx value+1	; for strings
	stx value+2
	stx notrsv
	jsr isital
	bcs token5	; not alpha
	lda #'I'
	sta token	; identifier
token7:
	jsr getnext
	jsr isital
	bcc tkn18	; alpha
	cmp #'_'	; underscore?
	beq tkn18_a	; yep
	jsr isitnm
	bcs token6	; end of ident
tkn18_a:
	inc notrsv
tkn18:
	jsr char
	inc tknlen
	bne token7
token6:
	lda notrsv
	bne tkn19
	lda #<rsvwrd
	sta wx
	lda #>rsvwrd
	sta wx+1
token8:
	ldy #0
	lda (wx),y
	bne token9	; more to go
tkn19:
	lda token
	rts 

; search for reserved word
;
token9:
	lda (wx),y	; length of word
	cmp tknlen	; same?
	bne tkn10	; can't be it then
	tay 	; length
	lda tknadr
	sta srce
	lda tknadr+1
	sta srce+1
	lda wx
	clc 
	adc #2
	sta dest
	lda wx+1
	adc #0
	sta dest+1
	jsr comstl
	bne tkn10	; not found
	ldy #1
	lda (wx),y
	sta token
	rts 
tkn10:
	ldy #0
	lda (wx),y	; length
	clc 
	adc #2
	adc wx
	sta wx
	bcc token8
	inc wx+1
	bne token8

;***********************************************
; not identifier
;***********************************************

token5:
	jsr isitnm
	bcc get_num
	jmp tkn12
get_num:
	sec 
	sbc #'0'
	sta value
	lda #0
	sta value+1
	sta value+2
tkn13:		; next digit
	jsr getnext
	jsr isitnm
	bcc tkn14	; more digits
tkn13a:
	lda sign
	beq tkn13b
	sec 
	lda #0
	tax 
	sbc value
	sta value
	txa 
	sbc value+1
	sta value+1
	txa 
	sbc value+2
	sta value+2
tkn13b:
	lda #'N'
	sta token
	clc 
	rts 
tkn14:
	jsr char
	sec 
	sbc #'0'
	sta digit
	inc tknlen
	jsr shlval
	lda value
	ldx value+1
	ldy value+2
	jsr shlval
	jsr shlval
	adc value
	sta value
	txa 
	adc value+1
	sta value+1
	tya 
	adc value+2
	sta value+2
	bcs tkn16
	lda value
	adc digit
	sta value
	bcc tkn13
	inc value+1
	bne tkn13
	inc value+2
	beq tkn16
	bmi tkn16
	jmp tkn13
;
tkn16_b:
	pla 
	pla 	; cut stack
tkn16:
	lda running
	bpl tkn16_a
	sec 
	rts 
tkn16_a:
	ldx #30
	jsr error
;
shlval:
	asl value
	rol value+1
	rol value+2
	bcs tkn16_b
	rts 
;
;***********************************************
; not a number
;***********************************************
tkn12:
	cmp quot_sym
	bne tkn17
	inc tknadr
	bne tkn12_a
	inc tknadr+1
tkn12_a:
	dec tknlen
tkn30_a:
	jsr getnext
	cmp #cr
	bne tkn31
	ldx #8	; missig quote
	jsr error
tkn31:
	cmp quot_sym
	bne tkn20
	jsr char
	jsr getnext	; another?
	cmp quot_sym
	beq tkn20	; imbedded quote
	ldy #3
	cpy tknlen
	bcc tkn31_a
	ldy tknlen
tkn31_a:
	dey 
	bmi tkn31_b
	lda (tknadr),y
	sta value,y
	bne tkn31_a
tkn31_b:
	lda tknlen
	bne tkn21
	ldx #14	; bad string
	jsr error
tkn21:
	lda quot_sym
	sta token
	rts 
tkn20:
	jsr char
	inc tknlen
	bne tkn30_a
;***********************************************
; not a string
;***********************************************
tkn17:
	cmp #'$'
	bne tkn29
	jsr getnext
	jsr isithx
	bcc tkn22
	lda #'$'
	sta token
	rts 
tkn22:
get_hex:
	sta value
	lda #0
	sta value+1
	sta value+2
tkn24:
	jsr char
	jsr getnext
	jsr isithx
	bcc tkn23
	jmp tkn13a
tkn23:
	inc tknlen
	ldx #4
tkn15:
	jsr shlval
	dex 
	bne tkn15
	clc 
	adc value
	sta value
	bcc tkn24
	inc value+1
	bne tkn24
	inc value+2
	bne tkn24
tkn16j:	jmp tkn16
;
;***********************************************
; not $ or hex literal
;***********************************************
tkn29:
	cmp #':'
	bne tkn25
	jsr getnext
	cmp #'='
	bne tkn26_a
	jsr char
	inc tknlen
	lda #'A'
tkn26:
	sta token
	rts 
tkn26_a:
	ldy #0
	lda (nxtchr),y
	bne tkn26
tkn25:
	cmp #'<'
	bne tkn27
	jsr getnext
	cmp #'='
	bne tkn28
	jsr char
	inc tknlen
	lda #$80
	bne tkn26
tkn27:
	cmp #'>'
	bne tkn30
	jsr getnext
	cmp #'='
	bne tkn26_a
	jsr char
	inc tknlen
	lda #$81
	bne tkn26
tkn28:
	cmp #'>'
	bne tkn26_a
	jsr char
	inc tknlen
	lda #'U'
	bne tkn26
tkn30:	cmp  #'-'
	bne tkn31_c
	sta sign
tkn32:	jsr  getnext
	jsr isitnm
	bcs tkn26_a
	jsr char
	jmp token5
tkn31_c:	cmp  #'+'
	bne tkn26_a
	beq tkn32

;***********************************************
; display a line

disp:	jsr dispad1
	jsr put_line	; display right-justified line no.
	lda ch
	sta leftcol
	lda #$40
	sta running
	lda a5
	ldx a5+1
	jsr pl
	lda #0
	sta running
	rts 
;
; display in hex
;
dishx:	jsr  prbyte
	jmp putsp

;***********************************************
; display error
;***********************************************

errlit:	.byte 14	; switch to lower case
	.byte "***",$bd	; '*** error'

error:	stx errno
	lda running
	beq err7
	jmp err6
err7:
	lda list
	bne err1
	jsr crout
	jsr disp
err1:
	lda tknadr
	sec 
	sbc a5
	pha 	; chars up to error point
	ldy #5
	lda #<errlit
	ldx #>errlit
	jsr pt
	pla 
	sta temp	; bytes to error point
	clc 
	adc leftcol
	sbc #8
	tax 
;
; allow for space counts
;
	ldy #0
	sty qt_tgl
	sty qt_size
err1_a:
	cpy temp
	bcs err2	; done
	lda (a5),y
	bmi err1_d	; could be reserved word
	cmp #$10	; dle?
	beq err1_b	; yes
	cmp quot_sym	; quote?
	bne err1_c	; no
	lda qt_tgl
	eor #1
	sta qt_tgl	; flip flag
err1_c:	iny	; onto next
	bne err1_a
;
; here to allow for spaces in expanded reserved word
;
err1_d:	sty  io_y
	ldy qt_tgl
	bne err1_f	; ignore if in quotes
	cmp #$b0
	bcc err1_e
	cmp #$df
	bcc err1_f	; not in range
;
err1_e:	sta  io_a	; token
	jsr pc_look
	clc 
	adc qt_size
	sta qt_size
err1_f:	ldy  io_y
	iny 
	bne err1_a	; done
;
;
err1_b:
	iny 
	lda (a5),y	; space count
	and #$7f	; clear 8-bit
	sta temp+1
	txa 
	clc 
	adc temp+1
	tax 
	dex 
	dex 	; allow for dle/count
	bne err1_c
;
err2:
	txa 
	clc 
	adc qt_size
	tax 
err3:
	jsr putsp
	dex 
	bne err3
	lda #'^'
	jsr pc
	jsr crout
	ldx #9
err5:
	jsr putsp
	dex 
	bne err5
err6:
	dec errno
	lda errno
	asl 
	clc 
	adc #<errtbl
	sta reg
	lda #>errtbl
	adc #0
	sta reg+1
	ldy #1
	lda (reg),y
	tax 
	dey 
	lda (reg),y
	jsr pl	; display error
errrtn:
	lda #0
	sta queue
	jmp (err_rtn)

;***********************************************
; error table
;***********************************************

errtbl:
	.word err01,err02,err03,err04
	.word err05,err06,err07,err08
	.word err09,err10,err11,err12,err13,err14,err15,err16
	.word err17,err18,err19,err06,err21,err22,err23,err24
	.word err25,err26,err27,err28,err29,err30,err31
	.word err32,err33,err34,err35,err36,err37,err38

err01:	.byte "mEMORY",$b1,$0d
err02:	.byte $b2,$b4,$0d
err03:	.byte "=",$b4,$0d
err04:	.byte $b3,$b4,$0d
err05:	.byte ",",$c1," :",$b4,$0d
err06:	.byte "BUG",$0d
err07:	.byte "*)",$b4,$0d
err08:	.byte $b7,$b8,$0d
err09:	.byte ".",$b4,$0d
err10:	.byte ";",$b4,$0d
err11:	.byte "uNDECLARED",$b3,$0d
err12:	.byte $b6,$b3,$0d
err13:	.byte ":=",$b4,$0d
err14:	.byte $bb,$b8,$c0,$be," LENGTH",$0d
err15:	.byte $ba," LIMITS EXCEEDED",$0d
err16:	.byte "then",$b4,$0d
err17:	.byte ";",$c1," end",$b4,$0d
err18:	.byte "do",$b4,$0d
err19:	.byte $b7,$c4,$0d
err21:	.byte "uSE",$c0," PROCEDURE",$b3," IN EXPRESSION",$0d
err22:	.byte ")",$b4,$0d
err23:	.byte $b6," FACTOR",$0d
err24:	.byte $c9,$bc,$0d
err25:	.byte "begin",$b4,$0d
err26:	.byte $22,$c0,$22,$b4,$0d
err27:	.byte $c6,$b1,$0d
err28:	.byte $22,$c2,$22,$c1," downto",$b4,$0d
err29:	.byte $b8,$bb,$c2,"O BIG",$0d
err30:	.byte $cc," OUT",$c0,$d8,$0d
err31:	.byte "(",$b4,$0d
err32:	.byte ",",$b4,$0d
err33:	.byte "[",$b4,$0d
err34:	.byte "]",$b4,$0d
err35:	.byte $d9,"S",$bc,"ED",$0d
err36:	.byte "dATA",$c9," NOT RECOGNISED",$0d
err37:	.byte $c4,$c8,$b1,$0d
err38:	.byte "dUPLICATE",$b3,$0d

;***********************************************
; get a token - check that it
; is the same as in "a", if not
; call error "x"
;***********************************************

getchk:	sta bsave
	txa 
	pha 
	jsr gtoken
	cmp bsave
	beq chkok
	pla 
	tax 
chknok:	jsr error
chkok:	pla
	rts 

;***********************************************
; check token agrees with "a",
; if not, give error "x"
;***********************************************
chktkn:
	cmp token
	bne chknok
	rts 
;
;***********************************************
; generate p-codes - no operands
;***********************************************
gennop:
	ldy syntax
	bne gen1
	sta (pcode),y
	pha 
	jsr dispad
	pla 
	ldx dcode
	beq gen1
	jsr dishx
	jsr crout
gen1:
	lda #1
	bne gen2_b
;***********************************************
; generate p-codes - with address
;***********************************************
genadr:
	ldy syntax
	bne gen2
	sta (pcode),y
	pha 
	lda displ
	iny 
	sta (pcode),y
	lda offset
	iny 
	sta (pcode),y
	lda offset+1
	iny 
	sta (pcode),y
	jsr dispad
	pla 
	ldx dcode
	beq gen2
	jsr dishx
	lda displ
	jsr dishx
	lda offset
	jsr dishx
	lda offset+1
	jsr dishx
	jsr crout
gen2:
	lda #4
gen2_b:
	clc 
	adc pcode
	sta pcode
	bcc gen2_a
	inc pcode+1
gen2_a:
	lda syntax
	bne gen2_c
	lda pcode+1
	cmp #>(p1-$18)
	bcc gen2_c
	bne gen_full
	lda pcode
	cmp #<(p1-$18)
	bcc gen2_c
gen_full:
	ldx #1	; mem full
	jsr error
gen2_c:
disp9:	rts

;***********************************************
; generate p-codes - jump address
;***********************************************

genrjmp:
	pha 
	lda opnd
	sec 
	sbc pcode
	sta opnd
	lda opnd+1
	sbc pcode+1
	sta opnd+1
	pla 
	jmp genjmp
;
gennjp:
	lda #60	; jmp
gennjm:	ldx  #0
	stx opnd
	stx opnd+1
;
genjmp:
	ldy syntax
	bne gen3
	sta (pcode),y
	pha 
	lda opnd
	iny 
	sta (pcode),y
	lda opnd+1
	iny 
	sta (pcode),y
	jsr dispad
	pla 
	ldx dcode
	beq gen3
	jsr dishx
	lda opnd
	jsr dishx
	lda opnd+1
	jsr dishx
	jsr crout
gen3:
	lda #3
	jmp gen2_b

;***********************************************
; display pcode address
;***********************************************
dispad:
	lda dcode
	beq disp9
dispad1:
	lda #'('
	jsr pc
	lda pcode+1
	jsr prbyte
	lda pcode
	jsr prbyte
	lda #')'
	jsr pc
	jmp putsp

;***********************************************
; fixup addresses
;***********************************************

fixad:
	ldy syntax
	bne disp9
	ldy #1
	lda pcode
	sec 
	sbc work
	sta (work),y
	iny 
	lda pcode+1
	sbc work+1
	sta (work),y
	lda dcode
	beq psh9
	lda #<fixm1
	ldx #>fixm1
	ldy #8
	jsr pt
	lda work+1
	jsr prbyte
	lda work
	jsr dishx
	lda #<fixm2
	ldx #>fixm2
	ldy #9
	jsr pt
	lda pcode+1
	jsr prbyte
	lda pcode
	jsr dishx
	jmp crout

fixm1:	.byte "jUMP AT "
fixm2:	.byte "CHANGED",$c2
	.byte " "

;***********************************************
; push 'work' onto stack
;***********************************************

pshwrk:
	sta bsave
	pla 
	tax 
	pla 
	tay 
	lda work+1
	pha 
	lda work
	pha 
	tya 
	pha 
	txa 
	pha 
	lda bsave
psh9:	rts

;***********************************************
; pull 'work' from stack
;***********************************************

pulwrk:
	sta bsave
	pla 
	tax 
	pla 
	tay 
	pla 
	sta work
	pla 
	sta work+1
	tya 
	pha 
	txa 
	pha 
	lda bsave
	rts 

;***********************************************
; printing subroutines
;***********************************************

prchar:
	stx xsave
	pha 
	cmp quot_sym
	bne pr_ntqt
	pha 
	lda qt_tgl
	eor #1
	sta qt_tgl
	pla 
pr_ntqt:
	pha 
	jsr cout
	pla 
	ldx pflag	; printing?
	beq pr_nptr
	pha 
	ldx #4
	jsr chkout	; direct to printer
	pla 

; the fiddling around below is because
; the capital letters in our messages (which are really
; lowercase here) do not print properly. so if we are not
; running a program or in quote mode, then we will convert
; what we think is 'lower case' to the equivalent in
; 'shifted' upper case (8 bit on).

	ldx running	; running?
	cpx #12
	beq pr_pok	; yes - ignore
	ldx qt_tgl	; in quotes?
	bne pr_pok	; yes - ignore
	cmp #'a'	; upper case (on c64)?
	bcc pr_pok	; nope
	cmp #'z'+1
	bcs pr_pok	; nope
	clc 
	adc #$60	; 'a' (hex 61) now becomes hex c1
;
pr_pok:
	jsr cout
	jsr clrchn	; back to screen
pr_nptr:
	jsr stop
	beq abort	; abort list
	lda queue	; keys in kbd queue?
	beq pr_not	; nope
	lda kbd_buf	; get item in queue
	cmp #$20	; space
	bne pr_not
	pla 
pause:	pha
	tya 
	pha 
	jsr getin	; clear that entry
pr_wait:
	jsr getin	; wait for another
	beq pr_wait
	jsr stop	; stop key?
	bne pr_onwd
abort:
	jsr getin	; clear keyboard buffer
	jmp (ctrlc_rt)
pr_onwd:	pla
	tay 
pr_not:
	pla 
prchr_x:
	ldx xsave
	rts 
;
;
;
prbyte:	pha
	lsr 
	lsr 
	lsr 
	lsr 
	jsr prhexz
	pla 
prhex:
	and #$0f
prhexz:	ora  #$30
	cmp #$3a
	bcc prhex1
	adc #$26
prhex1:	jmp  prchar

putsp:
	lda #32
	bne prhex1

pc:
	pha 
	jsr iosave
	pla 
	bpl pc3
	ldx running
	cpx #12
	bne pc1	; interpreting
pc9:	lda  io_a
	bmi pc3
pc1:
	lda qt_tgl
	bne pc9
	ldy #0
	lda io_a
	cmp #$b0
	bcc pc_rsvd
	cmp #$df
	bcs pc_rsvd
	cpx #$40	; in editor?
	beq pc9
	jsr putsp
	ldx #<dict
	stx reg
	ldx #>dict
	stx reg+1
pc6:
	lda (reg),y
	cmp #$ff
	beq pc7
	lda io_a
	cmp (reg),y
	beq pc5
	iny 
	bne pc6
	inc reg+1
	bne pc6
pc5:
	iny 
	bne pc5_a
	inc reg+1
pc5_a:
	lda (reg),y
	bmi pc2
	jsr prchar
	jmp pc5
pc7:
pc3:
	jsr prchar
pc2:
	jmp iorest
;
pc_look:		; lookup reserved word for pc and error
	lda #<rsvwrd
	sta reg
	lda #>rsvwrd
	sta reg+1
pc_rsvd1:
	iny 
	lda (reg),y	; token
	beq pc_look9	; end
	cmp io_a
	beq pc_rsvd2	; found
	dey 
	lda (reg),y	; length
	clc 
	adc #2
	adc reg
	sta reg
	bcc pc_rsvd1
	inc reg+1
	bne pc_rsvd1
pc_rsvd2:
	dey 
	lda (reg),y
pc_look9:
	rts 
;
pc_rsvd:
	jsr pc_look
	beq pc7	; not found
	tax 
	iny 
	iny 
pc_rsvd3:
	lda (reg),y
	jsr prchar
	iny 
	dex 
	bne pc_rsvd3
	jsr putsp
	beq pc2	; done!
;
;
pt:
	sta reg2
	stx reg2+1
	tya 
	tax 
	ldy #0
	sty qt_tgl
pt6:
	lda (reg2),y
	jsr pc
	iny 
	dex 
	bne pt6
	rts 
;
pl:
	sta reg2
	stx reg2+1
	ldy #0
	sty qt_tgl
pl5:
	lda (reg2),y
	cmp #$10	; dle
	beq pl5a
	jsr pc
	iny 
	cmp #cr
	bne pl5
	rts 
pl5a:
	iny 
	lda (reg2),y
	and #$7f	; strip 8-bit
	tax 
pl5b:
	jsr putsp
	dex 
	bne pl5b
	iny 
	jmp pl5
;
;

dict:	.byte $b0, "p-CODES"
	.byte $b1, "FULL"
	.byte $b2, "cONSTANT"
	.byte $d0, "p.o. bOX 124 iVANHOE 3079 vIC aUSTRALIA",$0d
	.byte $b3, "iDENTIFIER"
	.byte $b4, "EXPECTED"
	.byte $b5, "MISSING"
	.byte $cf, "gAMES",$0d
	.byte $b6, "iLLEGAL"
	.byte $b7, "iNCORRECT"
	.byte $d2, "vERSION 3.1 sER# 5001",$0d
	.byte $b8, "STRING"
	.byte $b9, "dO YOU WANT"
	.byte $ba, "COMPILER"
	.byte $d4, "<c>OMPILE"
	.byte $bb, "LITERAL"
	.byte $bc, "MISMATCH"
	.byte $d5, "<s>YNTAX"
	.byte $bd, "eRROR"
	.byte $ce, "gAMBIT"
	.byte $be, "ZERO"
	.byte $d3, "cOPYRIGHT 1983"
	.byte $bf, "SOURCE FILE"
	.byte $d7, "<q>UIT"
	.byte $c0, "OF"
	.byte $c1, "OR"
	.byte $c2, "TO"
	.byte $d1, 5,14,"g-pASCAL"
	.byte $c3, "ENDED AT "
	.byte $c4, "sYMBOL"
	.byte $d6, "wRITTEN BY nICK gAMMON AND sUE gOBBETT",$0d
	.byte $c6, "sTACK"
	.byte $c7, "iNSTRUCTION"
	.byte $d8, "rANGE"
	.byte $c8, "TABLE"
	.byte $c9, "tYPE"
	.byte $ca, "LIST"
	.byte $cb, "? y/n "
	.byte $cc, "nUMBER"
	.byte $cd, "lINE"
	.byte $d9, "pARAMETER"
	.byte $da, "<e>DIT,"
	.byte $db, "<"
edict:	.byte $ff

;
getans:
	jsr pt
	jsr rdkey
	and #$7f
	pha 
	cmp #$20
	bcs get1
	lda #$20
get1:
	jsr pc
	jsr crout
	pla 
getan9:
	rts 
;
;
iosave:
	sta io_a
	stx io_x
	sty io_y
	rts 
;
iorest:
	ldy io_y
	ldx io_x
	lda io_a
	rts 
;
;---- tknadr --> work
;
tknwrk:
	pha 
	lda tknadr
	sta work
	lda tknadr+1
	sta work+1
	pla 
	rts 
;
;---- work --> tknadr
;
wrktkn:
	pha 
	lda work
	sta tknadr
	lda work+1
	sta tknadr+1
	pla 
	rts 
;
;
rdkey:
	lda #0
	sta blnsw	; blink cursor
	sta autodn	; scroll
	jsr getin
	cmp #0
	beq rdkey	; loop until got a character
	pha 
	lda #0
	sta blnon
	pla 
	sta blnsw	; stop blinking
	rts 
;
home:
	lda #147
	jmp cout
;
;
	.byte 0	; end of file

	.endscope 

;************************************************
; pascal compiler
; for commodore 64
; part 2
; authors_ nick gammon & sue gobbett
;  sym $9000
;***********************************************

	.scope 

;***********************************************
; part 1 vectors
;***********************************************

	v1 = p1
	init = v1
	getnext = v1+3
	comstl = v1+6
	isithx = v1+9
	isital = v1+12
	isitnm = v1+15
	char = v1+18
	gen2_b = v1+21
	dishx = v1+24
	error = v1+27
	getchk = v1+30
	chktkn = v1+33
	gennop = v1+36
	genadr = v1+39
	gennjp = v1+42
	gennjm = v1+45
	tknwrk = v1+48
	prbyte = v1+51
	gtoken = v1+54
	wrktkn = v1+57
	fixad = v1+60
	pshwrk = v1+63
	pulwrk = v1+66
	pc = v1+69
	pt = v1+72
	pl = v1+75
	pc8 = v1+78
	getans = v1+81
	putsp = v1+84
	dispad = v1+87
	crout = v1+90
	shlval = v1+93
	get_num = v1+96
	get_hex = v1+99
	fnd_enq = v1+102
	pause = v1+105
	home = v1+108
	rdkey = v1+111
	genjmp = v1+114
	genrjmp = v1+117

;***********************************************
; part 2 starts here
;***********************************************

	.res 10	;xxxcjb
;.org $8dd4 ; p2

;***********************************************
; part 2 vectors
;***********************************************

	jmp stmnt
	jmp expres
	jmp chklhp
	jmp chkrhp
	jmp chklhb
	jmp chkrhb
	jmp lookup
	jmp chkdup
	jmp condec
	jmp vardec
	jmp const
	jmp getsub
	jmp w_string
	jmp wrktkn
	jmp symwrk
	jmp wrksym
	jmp pshpcode
	jmp chk_stak
	jmp search
	jmp addsym
	jmp tknjmp

;***********************************************
; part 6 vectors
;***********************************************

	block = p6

;
chklhp:
	lda #'('
	ldx #31
	jmp getchk
;
chkrhp:
	lda #')'
	ldx #22
	jsr chktkn
	jmp gtoken
;
getsub:
	jsr chklhb
	jsr expres
	jmp chkrhb
;
chklhb:
	lda lhb
	ldx #33
	jsr getchk
	jmp gtoken
;
chkrhb:
	lda rhb
	ldx #34
	jsr chktkn
	jmp gtoken
;
get_lev:
	lda level
	ldy #symlvl
	sec 
	sbc (symitm),y
	sta displ
	rts 
;
get_dat:
	ldy #symdat
	lda (symitm),y
	sta dattyp
	rts 
;
;
;***********************************************
;search symbol table
;***********************************************
search:
	lda endsym
	sta symitm
	lda endsym+1
	sta symitm+1
sea1:
	ldy #symprv
	lda (symitm),y
	tax 
	iny 
	lda (symitm),y
	sta symitm+1	; previous link
	txa 
	sta symitm
	ora symitm+1
	bne sea2	; more to go
	rts 	; finished
sea2:
	ldy #symlen
	lda (symitm),y
	cmp tknlen
	bne sea1	; wrong length
	lda symitm
	clc 
	adc #symnam
	sta dest
	lda symitm+1
	adc #0
	sta dest+1
	lda tknadr
	sta srce
	lda tknadr+1
	sta srce+1
	ldy tknlen
	jsr comstl
	bne sea1	; not that one
	jsr get_dat
	ldy #symlvl
	lda (symitm),y
	tax 	; level
	ldy #symtyp
	lda (symitm),y
	sta bsave
	cmp #'C'	; constant
	bne sea4
	ldy #symdsp
	lda (symitm),y
	sta value
	iny 
	lda (symitm),y
	sta value+1
	iny 
	lda (symitm),y
	sta value+2
	jmp sea3
sea4:		; not constant
	cmp #'V'	; variable?
	beq sea5	; yes
	cmp #'Y'	; argument?
	bne sea3	; no
sea5:
	jsr get_off
sea3:
	lda bsave
	rts 	; should set 'neq' flag

;***********************************************
; add symbol to symbol table
;***********************************************

addsym:	ldx  endsym
	stx symitm
	ldx endsym+1
	stx symitm+1
	ldy #symtyp
	sta (symitm),y
	ldy #symlvl
	pha 
	lda level
	sta (symitm),y
	ldy #symlen
	lda tknlen
	sta (symitm),y
	tay 
	dey 
	lda symitm
	clc 
	adc #symnam
	sta dest
	lda symitm+1
	adc #0
	sta dest+1
add1:
	lda (tknadr),y
	cmp #$c1
	bcc add2
	and #$7f	; upper case
add2:
	sta (dest),y
	dey 
	bpl add1
	lda dest
	clc 
	adc tknlen
	sta endsym
	lda dest+1
	adc #0
	sta endsym+1
	lda sym_use+1
	cmp endsym+1
	bcc sym_new
	bne sym_low
	lda sym_use
	cmp endsym
	bcs sym_low
sym_new:
	lda endsym
	sta sym_use
	lda endsym+1
	sta sym_use+1
sym_low:
	lda endsym+1
	cmp himem+1
	bcc sym_ntfl
	bne sym_full
	lda endsym
	cmp himem
	bcc sym_ntfl
sym_full:
	ldx #37
	jsr error
sym_ntfl:
;
	pla 
	tax 	; entry type
	cmp #'C'	; constant??
	bne add4
	ldy #symdsp
	lda value
	sta (symitm),y
	iny 
	lda value+1
	sta (symitm),y
	iny 
	lda value+2
	sta (symitm),y
	jmp add9
add4:
	ldy #symdat
	lda #1
	sta (symitm),y
	txa 
	cmp #'V'
	bne add9
	ldy #symdsp+1
	lda frame+1
	sta (symitm),y
	dey 
	lda frame
	sta (symitm),y
	inc frame
	bne add9
	inc frame+1
add9:
	ldy #symprv
	lda symitm
	sta (endsym),y
	iny 
	lda symitm+1
	sta (endsym),y
	rts 
;
;***********************************************
; jump on token
; x/y = start of table
; end of table is a null
; a = token
;***********************************************
tknjmp:
	stx reg
	sty reg+1
	tax 
jmp1:
	ldy #0
	lda (reg),y
	bne jmp2
	txa 
	rts 
jmp2:
	txa 
	cmp (reg),y
	bne jmp3
	pla 
	pla 	; remove return address
	iny 
	lda (reg),y
	sta reg2
	iny 
	lda (reg),y
	sta reg2+1
	txa 
	jmp (reg2)
jmp3:
	lda reg
	clc 
	adc #3
	sta reg
	bcc jmp1
	inc reg+1
	bne jmp1
;
lookup:
	jsr search
	bne look1
	ldx #11
	jsr error
look1:	rts
;
chkdup:	jsr  search
	beq dup9
	txa 
	cmp level
	bne dup9
	ldx #38
	jsr error
dup9:	rts

;
; constant dec
;

condec:
	lda #'I'
	ldx #4
	jsr chktkn
	jsr tknwrk
	lda tknlen
	pha 
	lda #'='
	ldx #3
	jsr getchk
	jsr gtoken
	jsr const
	jsr wrktkn
	pla 
	sta tknlen
	jsr chkdup
	lda #'C'
	jsr addsym
	jmp gtoken
;
;
;--- symitm --> work
;
symwrk:
	pha 
	lda symitm
	sta work
	lda symitm+1
	sta work+1
	pla 
	rts 
;
;--- work --> symitm
;
wrksym:
	pha 
	lda work
	sta symitm
	lda work+1
	sta symitm+1
	pla 
	rts 
;
; push pcode onto stack
;
pshpcode:
	sta bsave
	pla 
	tax 
	pla 
	tay 
	lda pcode+1
	pha 
	lda pcode
	pha 
	tya 
	pha 
	txa 
	pha 
	lda bsave
	rts 
;
get_off:
	pha 
	ldy #symdsp
	lda (symitm),y
	sta offset
	iny 
	lda (symitm),y
	sta offset+1
	ldy #symtyp
	lda (symitm),y
	cmp #'V'
	beq geto_1
	cmp #'A'
	beq geto_1
	cmp #'Y'
	bne geto_2
geto_1:
	sec 
	lda #$fd
	sbc offset
	sta offset
	lda #$ff
	sbc offset+1
	sta offset+1
geto_2:
	pla 
	rts 
;
getexpr:
	jsr gtoken
	jmp expres
;
;
pcd_wrkd:
	pha 
	lda pcode
	sta workd
	lda pcode+1
	sta workd+1
	pla 
	rts 
;
wrk_opnd:
	pha 
	lda work
	sta opnd
	lda work+1
	sta opnd+1
	pla 
	rts 
;
wrkd_wrk:
	pha 
	lda workd
	sta work
	lda workd+1
	sta work+1
	pla 
	rts 
;
wrk_wrkd:
	pha 
	lda work
	sta workd
	lda work+1
	sta workd+1
	pla 
	rts 
;
get_comm:
	lda #','
	ldx #32
	jmp chktkn
;
get_item:
	jsr get_comm	; check for comma
	jmp getexpr
;
val_move:
	pha 
	clc 
	lda value
	sta displ
	bpl val_1
	sec 
val_1:
	lda value+1
	beq val_2
	sec 
val_2:
	sta offset
	lda value+2
	sta offset+1
	beq val_3
	sec 
val_3:
	bcc val_5
	lda #0
	jsr genadr
	pla 
	rts 
val_5:
	lda value
	ora #$80
	jsr gennop
	pla 
	rts 
;
;
chk_stak:
	tsx 
	txa 
	cmp #max_stk
	bcc stk_full
	rts 
stk_full:
stk_err:
	ldx #27
	jsr error	; full

;
; const
;

const:
	lda token
	cmp #'N'
	beq const9
	cmp #'I'
	beq const1
	cmp quot_sym
	bne const3
	ldx tknlen
	cpx #4
	bcc const9
	jmp facerr1	; string too big
const1:	jsr  search
	bne const2
const3:
	ldx #2
	jsr error
const2:	cmp  #'C'
	bne const3
const9:	rts
;
; variable dec
;
vardec:	lda  #'I'
	ldx #4
	jsr chktkn
	jsr chkdup
	lda #'V'
	jsr addsym
	jmp gtoken
;
; simple expression
;
simexp:
	lda token
	cmp #'+'
	beq sim1
	cmp #'-'
	bne sim2
sim1:	pha
	jsr gtoken
	jsr term
	pla 
	cmp #'-'
	bne sim3
	lda #2
	jsr gennop	; negate
sim3:	lda  token
	cmp #'+'
	beq sim4
	cmp #'-'
	beq sim4
	cmp #$8a	; or
	beq sim4
	cmp #$a4	; xor
	beq sim4
	rts 
sim4:	pha
	jsr gtoken
	jsr term
	pla 
	cmp #'-'
	beq sim5
	cmp #'+'
	beq sim6
	cmp #$a4	; xor
	beq sim8
	lda #26	; or
sim7:	jsr  gennop
	jmp sim3
sim5:	lda  #6	; minus
	bne sim7
sim6:	lda  #4	; plus
	bne sim7
sim2:	jsr  term
	jmp sim3
sim8:	lda  #58	; xor
	bne sim7
;
; term
;
termt1:	.byte '*'
	.word term1
	.byte $8b
	.word term1
	.byte '/'
	.word term1
	.byte $8d
	.word term1
	.byte $8c
	.word term1
	.byte $8e
	.word term1
	.byte $8f
	.word term1
	.byte 0
;
term:	jsr  factor
term2:	ldx  #<termt1
	ldy #>termt1
	lda token
	jsr tknjmp
	rts 
;
term1:	pha
	jsr gtoken
	jsr factor
	pla 
	ldx #<termt3
	ldy #>termt3
	jsr tknjmp
;
term4:	lda  #10
term3:	jsr  gennop
	jmp term2
term5:	lda  #27	; and
	bne term3
term6:	lda  #11	; mod
	bne term3
term7:	lda  #34
	bne term3
term8:	lda  #36
	bne term3
term9:	lda  #8
	bne term3
;
termt3:	.byte $8b
	.word term4
	.byte '/'
	.word term4
	.byte $8d
	.word term5
	.byte $8c
	.word term6
	.byte $8e
	.word term7
	.byte $8f
	.word term8
	.byte '*'
	.word term9
	.byte 0

;
; factor
;
factor:	jsr  chk_stak
	lda token
	ldx #<factb1
	ldy #>factb1
	jsr tknjmp
	ldx #23
	jsr error
;
ident:	jsr  lookup
ident1:	cmp  #'P'
	bne ident2
	ldx #21
	jsr error
ident2:	cmp  #'Y'
	bne ident3
	lda #0
	sta opnd+1
	lda #3
	sta opnd
	ldy #symprv
	lda (symitm),y
	tax 
	iny 
	lda (symitm),y
	sta symitm+1
	txa 
	sta symitm
	lda #59
	jsr genjmp
	jmp fncprc
;
ident3:	cmp  #'A'
	beq ident4
	cmp #'C'
	bne ident5
	jsr val_move
	jmp ident7
;
facad1:	lda  #12
	jsr ident5_a
	jmp chkrhp
;
ident5:	lda  #44
ident5_a:	pha
;
	stx bsave
	lda level
	sec 
	sbc bsave
	sta displ
	pla 
ident6:	clc
	adc dattyp
	jsr genadr
ident7:	jmp  gtoken
;
facad2:	lda  #14
	jsr ident4_a
	jmp chkrhp
;
ident4:	lda  #48
ident4_a:	pha
;
	jsr symwrk
	jsr pshwrk
	jsr getsub
	jsr pulwrk
	jsr wrksym
	jsr get_dat
	jsr get_lev
	jsr get_off
	pla 
	clc 
	adc dattyp
	jmp genadr
;
; address (identifier)
;
;
facadr:
	jsr chklhp
	jsr get_look
	cmp #'V'
	beq facad1
	cmp #'A'
	beq facad2
	ldx #23
	jsr error
;
;
facstr:	lda  tknlen
	cmp #4
	bcc facnum
facerr1:	ldx  #29	; string too big
	jsr error
facnum:
	jsr val_move
	jmp ident7
;
paren:	jsr  getexpr
	jmp chkrhp
;
facmem:	lda  #0
	sta dattyp
	beq facm2
facmmc:	lda  #1
	sta dattyp
facm2:	lda  dattyp
	pha 
	jsr getsub
	pla 
	clc 
	adc #46
	bne gennop1
;
facnot:	jsr  gtoken
	jsr factor
	lda #32
gennop1:	jmp  gennop
;
spcl_fac:	jsr  tkncnv
facrnd1:	jsr  gennop
	jmp gtoken
;
s_freeze:	lda  #$15
	bne wait1_j
;
close_fl:	lda  #$5f
	bne wait1_j
;
get:	lda  #$60
	bne wait1_j
;
put:	lda  #$61
	bne wait1_j
;
;
spc_fac2:
	jsr tkncnv
wait1_j:
	jmp wait_1	; now get its argument
;
tkncnv:
	lda token
	sec 
	sbc #$a0
	rts 
;
facgtky:
	lda #7
	bne facrnd1	; getkey
;
factb1:	.byte 'I'
	.word ident
	.byte 'N'
	.word facnum
factqt1:
	.byte $22	; quote symbol
	.word facstr
	.byte '('
	.word paren
	.byte $91
	.word facmem	; mem
	.byte $90
	.word facnot
	.byte $a2
	.word facmmc	; memc
	.byte $a9
	.word facadr
	.byte $e6
	.word spcl_fac	; spritecollide
	.byte $e7
	.word spcl_fac	; bkgndcollide
	.byte $e8
	.word spcl_fac	; cursorx
	.byte $e9
	.word spcl_fac	; cursory
	.byte $ea
	.word spc_fac2	; clock
	.byte $eb
	.word spc_fac2	; paddle
	.byte $ed
	.word spc_fac2	; joystick
	.byte $ef
	.word spcl_fac	; random
	.byte $f0
	.word spcl_fac	; envelope
	.byte $f1
	.word spcl_fac	; scrollx
	.byte $f2
	.word spcl_fac	; scrolly
	.byte $f3
	.word spc_fac2	; spritestatus
	.byte $a7
	.word facgtky	; getkey
	.byte $ec
	.word spc_fac2	; spritex
	.byte $ee
	.word spc_fac2	; spritey
	.byte $f9
	.word spcl_fac	; invalid
	.byte $f8
	.word spc_fac2	; abs
	.byte $fd
	.word spcl_fac	; freezestatus
	.byte 0

;
; expression
;

expres:	jsr  chk_stak
	jsr simexp
	lda token
	ldx #<exptb1
	ldy #>exptb1
	jsr tknjmp
	rts 
;
exptb1:	.byte '='
	.word expr1
	.byte 'U'
	.word expr1
	.byte '<'
	.word expr1
	.byte $80
	.word expr1
	.byte $81
	.word expr1
	.byte '>'
	.word expr1
	.byte 0
;
expr1:	pha
	jsr gtoken
	jsr simexp
	pla 
	ldx #<exptb3
	ldy #>exptb3
	jsr tknjmp
;
exptb3:	.byte '='
	.word expr2
	.byte 'U'
	.word expr3
	.byte '<'
	.word expr4
	.byte $81
	.word expr5
	.byte '>'
	.word expr6
	.byte $80
	.word expr7
	.byte 0
;
expr2:	lda  #16
expr8:	jsr  gennop
	rts 
expr3:	lda  #18
	bne expr8
expr4:	lda  #20
	bne expr8
expr5:	lda  #22
	bne expr8
expr6:	lda  #24
	bne expr8
expr7:	lda  #25
	bne expr8
;
; statement
;
stmnt:	jsr  chk_stak
	lda token
	ldx #<stmnt1
	ldy #>stmnt1
	jsr tknjmp
	rts 
;
stmnt1:	.byte 'I'
	.word assign
	.byte $92
	.word if
	.byte $9a
	.word for
	.byte $96
	.word while
	.byte $95
	.word case
	.byte $98
	.word repeat
	.byte $88
	.word beg
	.byte $9e
	.word read
	.byte $9d
	.word write
	.byte $91
	.word mem
	.byte $9f
	.word callsb
	.byte $a2
	.word memc
	.byte $a3
	.word cursor
	.byte $a5
	.word def_sprt
	.byte $a6
	.word hplot
	.byte $aa
	.word wait
	.byte $81
	.word get
	.byte $ad
	.word s_freeze	; freezesprite (n)
	.byte $ae
	.word close_fl
	.byte $af
	.word put
	.byte $df
	.word sprite
	.byte $e0
	.word mve_sprt
	.byte $e1
	.word voice
	.byte $e2
	.word graphics
	.byte $e3
	.word sound
	.byte $e4
	.word setclock
	.byte $e5
	.word scroll
	.byte $a8
	.word clear
	.byte $f4
	.word mve_sprt	; movesprite (6 args)
	.byte $f5
	.word spc_fac2	; stopsprite
	.byte $f6
	.word spc_fac2	; startsprite
	.byte $f7
	.word anim_spt
	.byte $fa
	.word load_fil
	.byte $fb
	.word save_fil
	.byte $fc
	.word open_fil
	.byte $ff
	.word writeln
	.byte 0

;
; assignment
;

assign:	jsr  lookup
ass1:	ldx  #<asstb1
	ldy #>asstb1
	jsr tknjmp
	ldx #24
	jsr error
;
asstb1:	.byte 'A'
	.word assarr
	.byte 'V'
	.word assvar
	.byte 'Y'
	.word assvar
	.byte 'P'
	.word fncprc
	.byte 0

;
assarr:	jsr  symwrk
	jsr pshwrk
	lda #54
	clc 
	adc dattyp
	pha 
	jsr getsub
	jmp ass2
;
assvar:	jsr  symwrk
	jsr pshwrk
	lda #50
	clc 
	adc dattyp
	pha 
	jsr gtoken
ass2:	lda  #'A'
	ldx #13
	jsr chktkn
	jsr getexpr
	pla 
	jsr pulwrk
	jsr wrksym
	pha 
	jsr get_lev
	jsr get_off
	pla 
	jmp genadr
;
; load/save
;
open_fil:
load_fil:
save_fil:
	jsr args3
	pha 
	jsr get_comm
	lda quot_sym
	ldx #8	; " expected
	jsr getchk
	pla 
	jsr w_string
	jmp chkrhp
;
;
; writeln
;
writeln:
	jsr gtoken	; see if ( present
	cmp #'('
	bne writeln9	; nope
	jsr writ9
writeln9:
	lda #$5e	; output c/r
	jmp gennop

;
; write
;

write:	jsr  chklhp
writ9:	jsr  gtoken
	cmp quot_sym
	bne writ1
	lda #35
	jsr w_string
	jmp writ5
;
w_string:
	jsr gennop
	lda tknlen
	jsr gennop
	ldy #0
writ2:	lda  (tknadr),y
	cmp quot_sym
	bne writ10
	iny 
writ10:	iny
	tax 	; save a temporarily
	tya 	; save y on stack
	pha 
	txa 
	jsr gennop
	pla 
	tay 
	dec tknlen
	bne writ2
	jmp gtoken
;
writ1:		; here if not string
	cmp #$ab	; chr?
	beq w_chr	; yes
	cmp #$ac	; hex?
	beq w_hex	; yes
	jsr expres	; just ordinary number - get it
	lda #30	; output number
	jsr gennop
writ5:	lda  token
	cmp #','
	beq writ9
	jmp chkrhp
;
; here for write (chr(x))
;
w_chr:
	lda #31	; output character
w_chr1:
	jsr wait_1	; process expression in parentheses
	jmp writ5	; back for next item
;
; here for write (hex(x))
;
w_hex:
	lda #33	; output hex
	bne w_chr1
;
;
;
; get next token - must be identifier
; then look it up in symbol table
;
get_look:
	lda #'I'
	ldx #4
	jsr getchk
	jmp lookup

;
; read
;
read:	jsr  chklhp
read8:	jsr  get_look
read2:	jsr  symwrk
	jsr pshwrk
	ldx #0
	stx count1
	cmp #'A'
	beq read3
	cmp #'V'
	beq read9
	ldx #12
	jsr error
read9:	jsr  gtoken
read11:	pha
	lda #28
	clc 
	adc dattyp
	tax 
	pla 
read4:	cmp  #'$'
	bne read6
	ldx #23
read5:	txa
	pha 
	jsr gtoken
	pla 
	tax 
read6:	txa
	jsr gennop
	jsr pulwrk
	jsr wrksym
	jsr get_dat
read10:	jsr  get_lev
	jsr get_off
	lda #50
	ldx count1
	beq read7
	lda #54
read7:	clc
	adc dattyp
	jsr genadr
read7_a:	lda  token
	cmp #','
	beq read8
	jsr chkrhp
	rts 
read3:	lda  dattyp
	pha 
	jsr gtoken
	cmp lhb
	beq read3_a
	pla 
	sta dattyp
	bne read3_b
	ldx #24
	jsr error
read3_b:	jsr  pulwrk
	jsr wrksym
	lda #37	; read string
	jsr gennop
	jsr get_lev
	jsr get_off
	ldy #symsub
	lda (symitm),y
	jsr genadr	; a = length
	jmp read7_a
;
read3_a:	jsr  getexpr
	jsr chkrhb
	inc count1
	pla 
	sta dattyp
	lda token
	jmp read11
;
; clear
;
clear:	lda  #9
	jmp scroll_1

;
; cursor
;
cursor:	lda  #19
	pha 
;
;
two_op:	jsr  chklhp
	jsr getexpr
one_op2:	jsr  get_item
one_op:	jsr  chkrhp
	pla 
	jmp gennop
;
; graphics/sound/sprite/movesprite/voice
;
graphics:
sound:
sprite:
mve_sprt:
voice:
	jsr tkncnv
	pha 	; save for later
	jsr chklhp
voice_1:
	jsr getexpr
	jsr get_item
	pla 
	pha 
	cmp #$54	; 6-arg move sprite
	beq voice_3
	cmp #$42	; graphics
	bcs voice_2	; only 2 arguments wanted
	jsr get_item
	jmp voice_2
voice_3:		; want 4 more args
	jsr get_item
	jsr get_item
	jsr get_item
	jsr get_item
voice_2:
	pla 
	pha 
	jsr gennop
	lda token
	cmp #','
	beq voice_1	; another 3
	pla 
	jmp chkrhp
;
; process 3 arguments
;
args3:
	jsr tkncnv
	pha 
	jsr chklhp
	jsr getexpr
	jsr get_item
	jsr get_item
	pla 
	rts 
;
; setclock ( hours, mins, secs, 10ths. )
;
setclock:
	jsr args3
	pha 
	jmp one_op2
;
wait:	lda  #57
wait_1:	pha
	jsr chklhp
	jsr getexpr
	jmp one_op
;
; scroll
;
scroll:
	lda #69
scroll_1:	pha
	jmp two_op
;
anim_spt:
	lda #$57
	pha 
	lda #17	; count plus 16 pointers
	bne def_spt2
;
;
; definesprite
;
def_sprt:
	lda #1	; pcode
	pha 
	lda #21	; row count
def_spt2:	pha
	jsr chklhp
	jsr getexpr	; sprite pointer
def_1:	jsr  get_item	; next row
	pla 
	tax 
	dex 	; one less row
	beq def_8	; zero? none left
	txa 
	pha 
	lda token
	cmp #','
	beq def_1	; more supplied
;
; no more supplied - zero out rest
;
def_2:	lda  #$80	; load zero pcode
	jsr gennop
	pla 
	tax 
	dex 
	beq def_8	; all done
	txa 
	pha 
	bne def_2	; do another
def_8:	jsr  chkrhp
	pla 	; pcode for define/animate sprite
gennop2:	jmp  gennop
;
;
; hplot
;
hplot:	jsr  chklhp
	jsr getexpr	; colour
	lda #3
	pha 
	jsr get_item
	jmp one_op2
;
;
; mem
;
mem:	lda  #0
	pha 
	beq mem2
memc:	lda  #1
	pha 
mem2:	jsr  getsub
	lda #'A'
	ldx #13
	jsr chktkn
	jsr getexpr
	pla 
	clc 
	adc #52
	bne gennop2
;
; call absolute address
;
callsb:	jsr  chklhp
	jsr getexpr
	jsr chkrhp
	lda #43
	bne gennop2
;
; function or procedure call
;
fncprc:	lda  #0
	sta count1
	ldy #symarg
	lda (symitm),y
	beq fnc1
	jsr chklhp
fnc2:	lda  count1
	pha 
	jsr symwrk
	jsr pshwrk
	jsr getexpr
	jsr pulwrk
	jsr wrksym
	pla 
	sta count1
	inc count1
	lda token
	cmp #','
	beq fnc2
	lda count1
	ldy #symarg
	cmp (symitm),y
	beq fnc3
	ldx #35
	jsr error
fnc3:	jsr  chkrhp
	jmp fnc5
fnc1:	jsr  gtoken
fnc5:	jsr  get_lev
	jsr get_off
	ldy #symdat
	lda (symitm),y
	bne fnc5a
	lda offset
	sec 
	sbc pcode
	sta offset
	lda offset+1
	sbc pcode+1
	sta offset+1
	lda #39
	bne fnc5b
fnc5a:	lda  #56
fnc5b:	jsr  genadr
	lda count1
	beq fnc4
	lda count1	; times 3
	asl 
	bcs fnc6
	adc count1
	sta count1
	bcs fnc6
	lda #0
	sec 
	sbc count1
	sta opnd
	lda #$ff
	sta opnd+1
	lda #59
	jsr genjmp
fnc4:	rts
fnc6:	ldx  #15
	jsr error
;
;
; if
;
if:	jsr  getexpr
	lda #$93
	ldx #16
	jsr chktkn
	jsr gtoken
	jsr pshpcode
	lda #61
	jsr gennjm
	jsr stmnt
	lda token
	cmp #$94	; else
	beq if1
if2:	jsr  pulwrk
	jsr fixad
	rts 
if1:	jsr  pulwrk	; here for else
	jsr wrk_wrkd
	jsr pshpcode
	jsr gennjp
	jsr wrkd_wrk
	jsr fixad
	jsr gtoken
	jsr stmnt
	jmp if2
;
; begin
;
beg:	jsr  gtoken
	jsr stmnt
	lda token
	cmp #';'
	beq beg
	lda #$89	; end
	ldx #17
	jsr chktkn
	jmp gtoken
;
; repeat
;
repeat:	jsr  pshpcode
rep1:	jsr  gtoken
	jsr stmnt
	lda token
	cmp #';'
	beq rep1
	lda #$99
	ldx #10
	jsr chktkn
	jsr getexpr
	jsr pulwrk
	jsr wrk_opnd
	lda #61
	jmp genrjmp
;
; while
;
while:	jsr  pshpcode
	jsr getexpr
	jsr pshpcode
	lda #61
	jsr gennjm
	lda #$97
	ldx #18
	jsr chktkn
	jsr gtoken
	jsr stmnt
	jsr pulwrk
	jsr wrk_wrkd
	jsr pulwrk
	jsr wrk_opnd
	lda #60
	jsr genrjmp
	jsr wrkd_wrk
	jmp fixad

;
; case
;
case:	jsr  getexpr
	lda #$85	; of
	ldx #25
	jsr chktkn
	lda #1
	sta count1
case7:	lda  #0
	sta count2
case2:
	lda #42	; make copy of selector
	jsr gennop
	jsr getexpr	; next expression to compare
	lda #16
	jsr gennop
	lda token
	cmp #':'
	beq case1
	lda #','
	ldx #5
	jsr chktkn
	jsr pshpcode
	lda #62
	jsr gennjm
	inc count2
	jmp case2
case1:	jsr  pcd_wrkd
	lda #61
	jsr gennjm
	lda count2
	beq case3
case4:	jsr  pulwrk
	jsr fixad
	dec count2
	bne case4
case3:	jsr  wrkd_wrk
	jsr pshwrk
	jsr gtoken
	lda count1
	pha 
	jsr stmnt
	pla 
	sta count1
	lda token
	cmp #$94	; else
	beq case5
	cmp #';'
	bne case6
	jsr pcd_wrkd
	jsr gennjp
	jsr pulwrk
	jsr fixad
	jsr wrkd_wrk
	jsr pshwrk
	inc count1
	jmp case7
case5:	jsr  pcd_wrkd
	jsr gennjp
	jsr pulwrk
	jsr fixad
	jsr wrkd_wrk
	jsr pshwrk
	jsr gtoken
	lda count1
	pha 
	jsr stmnt
	pla 
	sta count1
case6:	lda  #$89	; end
	ldx #17
	jsr chktkn
	lda count1
	beq case8
case9:	jsr  pulwrk
	jsr fixad
	dec count1
	bne case9
case8:	jsr  for6
	jmp gtoken
;
; for
;
for:	lda  #'I'
	ldx #4
	jsr getchk
	jsr lookup
for1:	cmp  #'V'
	beq for2
	cmp #'Y'
	beq for2
	ldx #12
	jsr error
for2:	jsr  assvar
	jsr symwrk
	lda #0
	sta count1
	lda token
	cmp #$9b	; to
	beq for3
	lda #$9c	; downto
	ldx #28
	jsr chktkn
	dec count1
for3:	lda  count1
	pha 
	jsr pshwrk
	jsr getexpr
	jsr pulwrk
	jsr wrksym
	pla 
	sta count1
	jsr pshpcode
	lda #42
	jsr gennop
	jsr get_lev
	jsr get_off
	jsr get_dat
	clc 
	adc #44
	jsr genadr
	lda #22	; up (geq)
	ldx count1
	beq for4
	lda #25	; down (leq)
for4:	jsr  gennop
	jsr pshpcode
	lda #61
	jsr gennjm
	lda count1
	pha 
	jsr symwrk
	jsr pshwrk
	lda #$97
	ldx #18
	jsr chktkn
	jsr gtoken
	jsr stmnt
	jsr pulwrk
	jsr wrksym
	jsr get_lev
	jsr get_dat
	jsr get_off
	lda dattyp
	clc 
	adc #44
	jsr genadr
	pla 
	sta count1
	lda #38
	ldx count1
	beq for5
	lda #40	; dec
for5:	jsr  gennop
	lda #50
	clc 
	adc dattyp
	jsr genadr
	jsr pulwrk
	jsr wrk_wrkd
	jsr pulwrk
	jsr wrk_opnd
	lda #60
	jsr genrjmp
	jsr wrkd_wrk
	jsr fixad
for6:	lda  #$ff
	sta opnd+1
	lda #$fd
	sta opnd
	lda #59
	jmp genjmp

	brk 

	.endscope 

;************************************************
; pascal compiler
; for commodore 64
; part 3
; authors_ nick gammon & sue gobbett
;   sym $9000
;***********************************************

	.scope 

;***********************************************
; part 0 vectors
;***********************************************

	txt2reu = p0+46
	reu2txt = p0+84

;***********************************************
; part 1 vectors
;***********************************************

	v1 = p1
	init = v1
	getnext = v1+3
	comstl = v1+6
	isithx = v1+9
	isital = v1+12
	isitnm = v1+15
	char = v1+18
	gen2_b = v1+21
	dishx = v1+24
	error = v1+27
	getchk = v1+30
	chktkn = v1+33
	gennop = v1+36
	genadr = v1+39
	gennjp = v1+42
	gennjm = v1+45
	tknwrk = v1+48
	prbyte = v1+51
	gtoken = v1+54
	spare2 = v1+57
	fixad = v1+60
	pshwrk = v1+63
	pulwrk = v1+66
	pc = v1+69
	pt = v1+72
	pl = v1+75
	token1 = v1+78
	getans = v1+81
	putsp = v1+84
	dispad = v1+87
	crout = v1+90
	shlval = v1+93
	get_num = v1+96
	get_hex = v1+99
	fnd_end = v1+102
	pause = v1+105
	home = v1+108
	rdkey = v1+111
	genjmp = v1+114
	genrjmp = v1+117
	us = v1+120
	v1_next = v1+23	; available
;***********************************************
; part 2 vectors
;***********************************************
	v2 = p2
	tknjmp = v2+60
;
;
;***********************************************
; part 4 vectors
;***********************************************
	interj = p4
	dis4 = p4+3
	break = p4+6
	chk_err = p4+9	; invalid parameter in function call
	main = p4+12
	mainp = p4+15
	chk_top = p4+18
	true2 = p4+21
	pultop = p4+24
	s_pnt2 = p4+27
	masks = p4+30	; 8 bytes
	xmasks = p4+38	; 8 bytes
;
;***********************************************
; part 5 vectors
;***********************************************
	editor = p5
	getln = p5+3
	getlnz = p5+6
	getln1 = p5+9

;***********************************************
; part 6 vectors
;***********************************************

	block = p6

;***********************************************
; part 3 starts here
;***********************************************

	.res 1
;.org $992e ; p3

;***********************************************
; part 3 vectors
;***********************************************

	jmp start
	jmp restart
	jmp dsp_bin
	jmp st_cmp
	jmp st_syn
	jmp debug
	jmp st_edi
	jmp st_fil
	jmp bell1x
	jmp mov_spt
	jmp spt_stat
	jmp sprt_x
	jmp sprt_y
	jmp stop_spt
	jmp strt_spt
	jmp anm_spt
	jmp loadit
	jmp saveit
	jmp freeze_s
	jmp fr_stat
	jmp x_open
	jmp x_close
	jmp x_put
	jmp x_get

;***********************************************
; compiler mainline
;***********************************************

endmsg:	.byte $b0,$c3
endmg2:	.byte $d4
	.byte " FINISHED: NO",$bd
	.byte "S",$0d
fil_msg:
	.byte $0d,$db
	.byte "l>OAD,",$db
	.byte "a>PPEND, ",$db
	.byte "p>RINT, ",$db
	.byte "d>OS,",$0d,$db
	.byte "s>AVE,",$db
	.byte "n>OPRINT,",$db
	.byte "v>ERIFY,",$d7
	.byte ",",$0d,$da,$db
	.byte "c>ATALOG,",$db
	.byte "o>BJECT ? "
msg1:	.byte 13,$da,$d4,",",$db
	.byte "d>EBUG,",$db
	.byte "f>ILES,",$0d,$db
	.byte "r>UN, ",$d5
	.byte ", ",$db
	.byte "t>RACE,",$d7
	.byte " ? "
msg4:	.byte $c4,$c8,$c3
msg6:	.byte "nO VALID",$d4
	.byte " DONE BEFORE",$0d
;
prbytecr:
	jsr prbyte
	jmp crout
;
dos_cold:
	ldx #0
	ldy #$a0
	clc 
	jsr memtop	; normal basic top of memory
	lda #47
	sta $1	; make basic available
	jmp ($a000)
;
;
;
compil:	ldx  #new_stk
	txs 
	ldx #0
	ldy #$d0	; use spare memory
	clc 
	jsr memtop	; set top of memory
	lda #<st_edi
	sta err_rtn
	lda #>st_edi
	sta err_rtn+1
	lda himem+1
	sec 
	sbc sym_size
	sta symtbl+1
	lda himem
	sta symtbl
	sta sym_use
	sta endsym
;
;
	jsr home
	jsr init
	jsr gtoken
	ldy #0
	sty val_cmp
	tya 
	sta (endsym),y
	iny 
	sta (endsym),y
	jsr block
	lda #'.'
	ldx #9
	jsr chktkn
	lda #0
	ldx #19
	jsr getchk
	jsr crout
	lda #<endmsg
	ldx #>endmsg
	ldy #2
	jsr pt
	lda pcode+1
	sta end_pcd+1
	jsr prbyte
	lda pcode
	sta end_pcd
	jsr prbytecr
	lda #<msg4
	ldx #>msg4
	ldy #3
	jsr pt
	lda sym_use+1
	jsr prbyte
	lda sym_use
	jsr prbytecr
	lda #<endmg2
	ldx #>endmg2
	jsr pl
	ldx syntax
	bne end_cmp
	inx 
	stx val_cmp
end_cmp:
	jmp st1
;
chk_val:
	lda val_cmp
	bne chk_val9
	lda #<msg6
	ldx #>msg6
	jsr pl
	jmp st1
chk_val9:
bell1x:		; no bell yet
	rts 
;
chk_run:
	jsr txt2reu
	jmp interp

;
; start
;

main_tbl:
	.byte 'C'
	.word st_cmp
	.byte 'R'
	.word st_run
	.byte 'S'
	.word st_syn
	.byte 'E'
	.word new_edi
	.byte 'Q'
	.word st_qui
	.byte 'D'
	.word st_deb
	.byte 'T'
	.word st_tra
	.byte 'F'
	.word st_fil
	.byte $0
fil_tbl:
	.byte 'E'
	.word st_edi
	.byte 'Q'
	.word st1
	.byte 'A'
	.word st_app
	.byte 'L'
	.word st_loa
	.byte 'S'
	.word st_wri
	.byte 'V'
	.word st_vfy
	.byte 'P'
	.word st_pri
	.byte 'N'
	.word st_nop
	.byte 'O'
	.word st_obj
	.byte 'C'
	.word st_cat
	.byte 'D'
	.word st_dos
	.byte $0
;
;
;
;
; here for cold start - initialize all c64 routines
; do ram test, clear text file to null etc. etc.
;
start:
	sei 
	ldx #0
	stx vic+$16	; clear reset bit in vic
	dex 
	txs 
	jsr ioinit
	nop 
	nop 
	nop 	; instead of jsr ramtas
	jsr restor
	jsr cint
	jsr initio
	cli 
	lda ts
	sta reg
	lda ts+1
	sta reg+1
	lda #0
	tay 
	sta (reg),y	; null edit file
	sty val_cmp
	tax 
	ldy #$d0	; use spare memory
	clc 
	jsr memtop	; set top of memory
	lda cinv
	sta int_rtn
	lda cinv+1
	sta int_rtn+1	; interrupt return address
	jmp rest1
;
restart:
	sei 
	lda int_rtn
	sta cinv
	lda int_rtn+1
	sta cinv+1
	cli 
	cld 
	ldx #$ff
	txs 	; reset stack
	jsr clall	; close any files run left open
	jsr cint	; reset video
	jsr reu2txt
;
rest1:
	jsr proff
	lda #$c0
	jsr setmsg
	lda #0
	sta running
	lda #6	; blue
	sta border
	sta bkgnd	; background to blue
	lda #<restart
	sta warm_str
	sta ctrlc_rt
	lda #>restart
	sta warm_str+1
	sta ctrlc_rt+1
	jsr home
	lda #<us
	ldx #>us
	ldy #8
	jsr pt
st1:	lda  #<msg1
	ldx #>msg1
	ldy #43
	jsr getans
	ldx #<main_tbl
	ldy #>main_tbl
;
	jsr tknjmp
st1_jump:	jmp  st1
;
;
zero_sid:
	ldy #24
	lda #0
zero_1:	sta  sid,y
	dey 
	bpl zero_1
	rts 
;
initio:
	jsr ioinit
	jsr clall
	jsr zero_sid
	lda #47
	sta $0	; data direction register
	lda #46
	sta $1	; disable basic
	lda #0
	sta $f8	; de-allocate rs232 buffers
	sta $fa
	sta st	; clear st flag
	sta enabrs	; clear rs232 enables
	lda #4
	sta hibase	; normal video page
	rts 
;
st_cmp:
	lda #0
	sta syntax
	jmp compil
st_syn:
	sta syntax
	jmp compil
st_run:
	lda #0
	sta dbgflg
	sta dcode
	jmp chk_run
st_edi:
	jmp editor
st_fil:
	ldx #$ff
	txs 	; reset stack
	lda #<fil_msg
	ldx #>fil_msg
	ldy #85
	jsr getans
	ldx #<fil_tbl
	ldy #>fil_tbl
	sta token	; in case we want to know
	jsr tknjmp
st_fil9:
	jmp st_fil
;
quit_msg:	.byte  $d7,$cb

st_qui:
	lda #<quit_msg
	ldx #>quit_msg
	ldy #2
	jsr getans
	cmp #'Y'
	bne st1_jump
	jmp dos_cold	; coldstart dos
st_deb:
	sta dbgflg
	sta dcode
	jmp chk_run
st_tra:
	ora #$80
	sta dbgflg
	sta dcode
	jmp chk_run
st_pri:
	jsr pron
	jmp st_fil
st_nop:
	jsr proff
	jmp st_fil

file_msg:	.byte "fILE NAME? "
file_mg2:	.byte $db
	.byte "c>ASSETTE OR",$db
	.byte "d>ISK? "
file_mg3:	.byte "cOMMAND? "

;
get_file:
	lda #<file_mg2
	ldx #>file_mg2
	ldy #21
	jsr getans
	cmp #'D'
	beq get_fil1
	cmp #'C'
	beq get_fil0
	jmp st_fil9
get_fil1:	lda  disk_chn	; serial bus disk drive
	bne get_fil8
get_fil0:	lda  #1	; datasette
get_fil8:	sta  reg2b
	lda #<file_msg
	ldx #>file_msg
	ldy #11
	jsr pt
get_fil2:
	jsr getln1
	stx tknlen
	cpx #0
	bmi get_fil4
	bne get_fil3
	lda token	; zero length ok on cassette load
	cmp #'V'
	beq get_fil9	; verify
	cmp #'A'
	beq get_fil9	; append
	cmp #'L'
	bne get_fil4	; not load
get_fil9:	lda  reg2b
	cmp #1	; cassette?
	beq get_fil7	; yes - hooray!
get_fil4:	jmp  st_fil
get_fil3:		; no check on alpha file name now
get_fila:	ldy  #19
get_fil5:	lda  inbuf,y
	sta bpoint,y
	dey 
	bpl get_fil5
get_fil7:
;
; if disk load/save etc. open error channel (15)
;
	ldx reg2b
	cpx disk_chn	; disk?
	bne get_filb	; nope
	lda #15
	tay 	; error channel
	jsr setlfs
	lda #0	; no command
	jsr setnam
	jsr open	; right - it's open
;
get_filb:
	lda #1	; logical file number
	ldx reg2b	; 1 = cassette, 8 = disk
	ldy #0	; secondary address
	jsr setlfs
	lda tknlen
	cmp #20
	bcc get_fil6
	lda #20
get_fil6:
	ldx #<bpoint
	ldy #>bpoint	; temporary buffer as inbuf is tape buffer
	jmp setnam	; setup file name
;
catalog:	.byte "$"
disk_msg:	.byte "dISK:"

;
st_cat:
;
; here for directory list
;
	lda #1
	ldx disk_chn
	ldy #0	; relocated load
	sty regb
	jsr setlfs
	lda #1	; $ length
	ldx #<catalog
	ldy #>catalog
	jsr setnam
	lda #0	; load
	ldx #<$c000	; use symbol table
	stx tknadr
	ldy #>$c000	; for directory
	sty tknadr+1
	jsr load
	bcs st_filj	; error on load
	jsr crout
	jsr crout
	lda #<disk_msg
	ldx #>disk_msg
	ldy #5
	jsr pt
	jmp cat_strt	; no sector count for first line
cat_loop:
	ldy #1
	lda (tknadr),y	; end?
	bne cat_more	; no
	iny 
	lda (tknadr),y
	beq cat_end	; finished
	dey 
cat_more:
	iny 
	iny 	; bypass line number link
	lda (tknadr),y	; no_ sectors
	sta reg
	iny 
	lda (tknadr),y	; no_ sectors
	sta reg+1
	jsr dsp_bin	; display sector count
	jsr putsp
cat_strt:
	ldy #5
cat_cnt:
	lda (tknadr),y
	beq cat_cntd	; found end of this line
	iny 
	bne cat_cnt
cat_cntd:
	sty tknlen	; size of this entry
	tya 
	sec 
	sbc #5	; forget initial stuff
	tay 	; read for pt
	lda tknadr
	clc 
	adc #5
	pha 
	lda tknadr+1
	adc #0
	tax 
	pla 
	jsr pt	; at last! - display the info
	jsr crout
	lda tknadr
	clc 
	adc tknlen
	sta tknadr
	bcc cat_loop
	inc tknadr+1
	bne cat_loop
cat_end:
st_filj:	jmp  st_fil
;
st_vfy:
	jsr get_file
	ldx ts
	ldy ts+1
	lda #1
	bne st_loa1
;
dos_err:	.byte 13,$bd
	.byte ": "

;
fin_dos:
	php 
	jsr crout
	jsr readst	; check status (verify error etc_)
	and #$bf	; ignore end-of-file
	beq fin_dos2	; ok
	pha 
	lda #<dos_err
	ldx #>dos_err
	ldy #4
	jsr pt
	pla 
	jsr prbytecr
fin_dos2:
	lda reg2b
	cmp disk_chn
	bne fin_dos9
	plp 	; back to initial carry flag
	jmp st_err	; read error channel if disk
fin_dos9:
	plp 
	jmp st_fil
;
st_loa:
	jsr get_file
	lda #0
	sta val_cmp
	ldx ts
	ldy ts+1
st_loa1:	jsr  load
st_loa2:	jmp  fin_dos
;
;
st_obj:
	jsr chk_val
	jsr get_file
	lda act_pcda
	sta temp
	lda act_pcda+1
	sta temp+1
	ldx end_pcd
	ldy end_pcd+1
st_obj1:	lda  #temp
	jsr save
st_obj2:	jmp  fin_dos
;
st_wri:
	jsr get_file
	jsr fnd_end
	lda ts
	sta temp
	lda ts+1
	sta temp+1
	ldx p
	ldy p+1
	jmp st_obj1
;
;
st_app:
	jsr get_file
	lda #0
	sta val_cmp
	jsr fnd_end
	lda p
	sec 
	sbc #1
	tax 
	lda p+1
	sbc #0
	tay 
	lda #0
	beq st_loa1
;
dos_msg:	.byte "cODE: "
;
; dos
;
st_dos:
	lda #<file_mg3	; 'Command?'
	ldx #>file_mg3
	ldy #9
	jsr pt
	jsr getln1	; get response
	cpx #0
	beq st_dos9	; nosing
	stx tknlen
	ldx disk_chn	; now open disk channel 15
	lda #15
	tay 	; command channel
	jsr setlfs
	ldx #<inbuf
	ldy #>inbuf
	lda tknlen
	jsr setnam	; send command
	jsr open
	bcs st_dos8
st_err:
	ldx #15
	jsr chkin	; read error channel
	bcs st_dos8
	jsr getln1	; read it
	jsr clrchn	; back to keyboard
	lda #<dos_msg
	ldx #>dos_msg
	ldy #6
	jsr pt
	lda #<inbuf
	ldx #>inbuf
	jsr pl	; display the message
st_dos8:	lda  #15	; close command channel
	jsr close
st_dos9:	jmp  st_fil	; finito
;
;
pron:
	lda #4
	sta pflag
	ldx pr_chan	; printer
	ldy #0	; normal mode
	jsr setlfs
	jsr open	; printer is unit 1
	bcc xxxrts	; open ok

;
proff:
	lda #0
	sta pflag
	jmp clall	; close all files (incl. printer & screen )

;
unpack:
	pha 
	clc 
	ror 
	clc 
	ror 
	clc 
	ror 
	clc 
	ror 
	ora #$30
	sta asc_wrk,x
	inx 
	pla 
	and #$0f
	ora #$30
	sta asc_wrk,x
	inx 
xxxrts:	rts

;
;
bin_tbl:
	.byte $76,$85,$04,$01
	.byte $36,$55,$06,$00
	.byte $96,$40,$00,$00
	.byte $56,$02,$00,$00
	.byte $16,$00,$00,$00
	.byte $01,$00,$00,$00

;
dsp_bin:
	lda reg
	ldx reg+1
	ldy regb
	bpl out_plus
	lda #'-'
	jsr pc
	sec 
	lda #0
	sbc reg
	pha 
	lda #0
	sbc reg+1
	tax 
	lda #0
	sbc regb
	tay 
	pla 
out_plus:
	sta bin_wrk
	stx bin_wrk+1
	sty bin_wrk+2
	lda #$f0
	sta reg2
	lda #0
	sta value
	sta value+1
	sta temp
	sta temp+1
	tax 
	ldy #2
out4:
	lda bin_wrk,y
	and reg2
	sta reg2+1
	beq out2
	lda reg2
	bpl out1
	lsr reg2+1
	lsr reg2+1
	lsr reg2+1
	lsr reg2+1
out1:
	sed 
	lda temp
	clc 
	adc bin_tbl,x
	sta temp
	lda temp+1
	adc bin_tbl+1,x
	sta temp+1
	lda value
	adc bin_tbl+2,x
	sta value
	lda value+1
	adc bin_tbl+3,x
	sta value+1
	cld 
	dec reg2+1
	bne out1
out2:
	inx 
	inx 
	inx 
	inx 
	lda reg2
	eor #$ff
	sta reg2
	bpl out4
	dey 
	bpl out4
out3:
	ldx #0
	lda value+1
	jsr unpack
	lda value
	jsr unpack
	lda temp+1
	jsr unpack
	lda temp
	jsr unpack
	ldx #7	; zero suppress
	ldy #0
out5:	lda  asc_wrk,y
	cmp #'0'
	bne out6
	iny 
	dex 
	bne out5
out6:	lda  asc_wrk,y
	sty bin_wrk
	stx bin_wrk+1
	jsr pc
	ldy bin_wrk
	ldx bin_wrk+1
	iny 
	dex 
	bpl out6
db9:	rts

;
dm1:	.byte " sTACK: "
dm2:	.byte " bASE:  "
dm4:	.byte "rUNNING",$0d

;
debug:
db11:	jsr  dispad
	lda p
	sta work
	lda p+1
	sta work+1
	ldx #4
	jsr dis4
	jsr crout
	ldx dbgflg
	bmi db9	; trace only
	lda #<dm1
	ldx #>dm1
	ldy #8
	jsr pt
	lda t+1
	jsr prbyte
	lda t
	jsr dishx
	lda #'='
	jsr pc
	lda t
	sta work
	lda t+1
	sta work+1
	ldx #8
	jsr dis4
	jsr crout
	lda #<dm2
	ldx #>dm2
	ldy #8
	jsr pt
	lda base+1
	jsr prbyte
	lda base
	jsr dishx
	lda #'='
	jsr pc
	lda base
	sec 
	sbc #6
	sta work
	lda base+1
	sbc #0
	sta work+1
	ldx #6
	jsr dis4
	jmp crout
;
;
; interpreter initialization
;
interp:
	php 
	pla 
	sta call_p
	lda act_pcda
	sta p
	lda act_pcda+1
	sta p+1
	lda #<break
	sta err_rtn
	sta ctrlc_rt
	lda #>break
	sta err_rtn+1
	sta ctrlc_rt+1
	lda #<dm4
	ldx #>dm4
	jsr pl
	ldy #12
	sty running
	lda #<p_stack
	sta t
	sta base
	lda #>p_stack
	sta t+1
	sta base+1
	lda #0
	sta dos_flg
	sta coll_reg
	sta mask
	ldy #7
inter_lp:
	sta s_active,y
	sta s_animct,y
	dey 
	bpl inter_lp
;
; now process our sprite table on timer interrupts
;
	sei 
	lda #<timer_in
	sta cinv
	lda #>timer_in
	sta cinv+1
	cli 
	jmp interj
;
;
spt_stat:
	jsr get_spt
	lda s_active,x
	jmp true2
;
mov_spt:
	jsr pultop
	sta bpoint+10
	stx bpoint+11	; moves
	jsr pultop
	sta bpoint+12
	stx bpoint+13	; yinc
	jsr pultop
	sta bpoint+14
	stx bpoint+15	; xinc
	sty bpoint+16
	jsr pultop
	sta bpoint+17	; y pos
	jsr pultop
	sta bpoint+18
	stx bpoint+19	; x pos
	jsr get_spt
	sei 
	lda bpoint+10
	sta s_count,x
	lda bpoint+11
	sta s_count+8,x
	lda bpoint+12
	sta s_yinc,x
	lda bpoint+13
	sta s_yinc+8,x
	lda bpoint+14
	sta s_xinc,x
	lda bpoint+15
	sta s_xinc+8,x
	lda bpoint+16
	sta s_xinc+16,x
	lda bpoint+17
	sta s_ypos+8,x
	lda bpoint+18
	sta s_xpos+8,x
	lda bpoint+19
	sta s_xpos+16,x
	lda #0
	sta s_ypos,x
	sta s_xpos,x
	lda #1
	sta s_active,x
	jsr pos_sprt
	lda masks,x
	ora vic+$15	; activate it
	sta vic+$15
	cli 
	jmp main
;
anm_spt:
	lda #16
	sta bpoint	; pointer count
anm_1:
	jsr pultop	; get pointer
	ldy bpoint
	sta bpoint,y
	dec bpoint
	bne anm_1	; more
	lda #254
	jsr chk_top	; frames per pointer
	tay 
	iny 	; back to supplied value
	sty bpoint+18	; frame change count
	jsr get_spt
	sta bpoint+19	; sprite
	inx 
	txa 	; next one (so we can work backwards)
	asl 
	asl 
	asl 
	asl 	; times 16
	tay 
	ldx #16	; pointer count
	sei 
anm_3:
	lda bpoint,x
	beq anm_2	; not used
	inc bpoint	; count used ones
anm_2:
	dey 
	sta s_pointr,y
	dex 
	bne anm_3	; more
	ldy bpoint+19	; sprite
	lda bpoint	; used count
	sta s_animfm,y
	lda bpoint+18	; frame count
	sta s_animct,y
	lda #0
	sta s_animps,y
	sta s_animcc,y
	cli 
	sty spriteno
	lda bpoint+1	; first pointer
	sta fnc_val
	jmp s_pnt2	; now point to first
;
sprt_x:
	jsr get_spt
	asl 
	tay 
	lda vic,y	; low byte
	sta reg
	lda vic+$10
	and masks,x	; high bit
	beq sprt_x1	; off
	lda #1	; mark on
sprt_x1:	sta  reg+1
	jmp mainp	; push result
;
sprt_y:
	jsr get_spt
	asl 
	tay 
	lda vic+1,y
	jmp true2	; result
;
stop_spt:
	lda #0
	pha 
chng_spt:
	jsr get_spt
	pla 	; get new status
	sta s_active,x	; mark inactive/active
	jmp main
;
strt_spt:
	lda #1
	pha 
	bne chng_spt
;
timer_in:		; here for timer interrupts
;
; first look for collisions
;
	lda vic+25	; interrupt register
	and #$84	; sprite-sprite collision?
	cmp #$84
	bne timer2	; nope - normal interrupt
	sta vic+25	; re-enable
	lda vic+30	; collision register
	tax 	; save it
	and mask
	beq forget	; wrong sprite - forget interrupt
	stx coll_reg	; save interrupt register
	ldx #1
	stx int_temp
	dex 
tim_col1:	lda  coll_reg
	and int_temp	; this sprite involved?
	beq tim_col2	; no
	lda #0
	sta s_active,x	; yes - stop it
tim_col2:	inx	; next sprite
	asl int_temp	; next mask
	bne tim_col1	; more to go
	sta mask	; stop further tests
	lda vic+26
	and #$fb	; stop interrupts
	sta vic+26
forget:	pla
	tay 
	pla 
	tax 
	pla 
	rti 	; off we go

;
timer2:		; here for non-collision interrupts
	cld 
	ldx #0
timer_ck:	lda  s_active,x	; sprite active?
	bne timer_ac	; yes
timer_nx:	inx
	cpx #8
	bne timer_ck	; more to go
	jmp (int_rtn)
;
timer_ac:		; here for active one
	clc 
	lda s_xinc,x
	adc s_xpos,x
	sta s_xpos,x
	lda s_xinc+8,x
	adc s_xpos+8,x
	sta s_xpos+8,x
	lda s_xinc+16,x
	adc s_xpos+16,x
	sta s_xpos+16,x
	clc 
	lda s_yinc,x
	adc s_ypos,x
	sta s_ypos,x
	lda s_yinc+8,x
	adc s_ypos+8,x
	sta s_ypos+8,x
	sec 
	lda s_count,x
	sbc #1
	sta s_count,x
	lda s_count+8,x
	sbc #0
	sta s_count+8,x
	bpl timer_on
	lda #0	; finished with this oe
	sta s_active,x	; turn it off
timer_on:
	jsr pos_sprt
	lda s_animct,x
	beq no_anim
;
; now animate the pointers
;
	ldy s_animcc,x	; current frame count
	iny 
	tya 
	cmp s_animct,x	; reached limit?
	bcs new_fram	; yes
	sta s_animcc,x	; no - save it
	bcc no_anim	; that's all
new_fram:
	lda #0
	sta s_animcc,x	; back to start
	ldy s_animps,x	; which position next?
	iny 
	tya 
	cmp s_animfm,x	; limit?
	bcc anim_ok	; no
	lda #0	; yes
anim_ok:
	sta s_animps,x	; save current position
	txa 	; sprite
	asl 
	asl 
	asl 
	asl 	; times 16
	ora s_animps,x	; plus frame number
	tay 
	lda s_pointr,y	; get pointer
	sta int_temp
;
; now point the sprite
;
	lda cia2+2
	pha 
	and #$fc
	sta cia2+2
	lda cia2
	and #3
	eor #$ff
	asl 
	asl 
	asl 
	asl 
	asl 
	asl 
	sta int_tmp2	; bank
	pla 
	sta cia2+2
	lda reg
	pha 
	lda reg+1
	pha 
	lda vic+$18
	and #$f0	; video base
	lsr 
	lsr 
	clc 
	adc #3
	adc int_tmp2	; add bank
	sta reg+1
	lda #$f8
	sta reg
	txa 	; sprite
	tay 
	lda int_temp	; pointer
	sta (reg),y
	pla 
	sta reg+1
	pla 
	sta reg
no_anim:	jmp  timer_nx
;
pos_sprt:
	lda s_xpos+16,x
	and #1
	beq pos_1	; high-order zero
	lda masks,x
pos_1:	sta  int_temp
	lda xmasks,x
	and vic+$10
	ora int_temp
	sta vic+$10
	txa 
	asl 
	tay 
	lda s_xpos+8,x
	sta vic,y
	lda s_ypos+8,x
	sta vic+1,y
	rts 
;
get_spt:
	lda #7
	jsr chk_top
	tax 
	rts 
;
loa_sve:		; get ready for load/save
	jsr pultop	; load/verify flag
	sta sce_lim
	stx sce_lim+1
	jsr pultop
	sta temp	; address to load/save
	stx temp+1
	jsr pultop	; device number
	tax 	; device
	lda #1	; file 1
	ldy #0
loa_sve1:	jsr  setlfs
	ldy #0
	lda (p),y	; length of name
	pha 
	lda p	; address of name
	clc 
	adc #1
	tax 
	lda p+1
	adc #0
	tay 
	pla 	; size of name
	jsr setnam
	clc 
	adc #1	; bypass length
	adc p
	sta p
	bcc loadit1
	inc p+1
loadit1:
	rts 
;
loadit:
	jsr loa_sve
	lda sce_lim	; load/verify flag
	ldx temp
	ldy temp+1	; address
	jsr load
	stx call_x
	sty call_y
loadit2:	bcs  loadit3	; error in accumulator
loadit4:	jsr  readst	; otherwise check readst
loadit3:
	sta dos_flg
	jmp main	; done
;
saveit:
	jsr loa_sve
	ldx sce_lim
	ldy sce_lim+1
	cpy temp+1	; end less than start?
	bcc save_err	; yes - oops
	bne save_ok	; not equal - must be greater (ok)
	cpx temp	; high order same - is low order less than start?
	bcc save_err	; yes - oops
	beq save_err	; even same is no good
save_ok:
	lda #temp
	jsr save
	jmp loadit2
;
save_err:	jmp  chk_err	; start address >= end address
;
x_open:		; open a file
	jsr pultop	; secondary address
	sta temp
	jsr pultop	; device
	sta temp+1
	jsr pultop	; unit
	ldx temp+1	; device
	ldy temp	; secondary address
	jsr loa_sve1	; now setlfs and process file name
	jsr open	; now open the file
	jmp loadit2	; and see the result
;
x_close:		; close a file
	jsr pultop	; file number
	jsr close
	jmp loadit2	; and see the result
;
x_put:
	jsr pultop	; file number
	tax 	; zero (clear channel?)
	beq x_put0
	jsr chkout
	jmp loadit2	; result
;
x_get:
	jsr pultop	; file number
	tax 	; zero (clear channel?)
	beq x_get0
	jsr chkin
	jmp loadit2	; result
;
x_get0:
x_put0:
	jsr clrchn	; clear channels
	jmp main
;
;
;
freeze_s:
	sei 
	sty coll_reg	; no collision yet
	lda vic+25
	and #$84	; clear any pending interrupts
	sta vic+25
	lda vic+30	; clear any current collisions
	jsr pultop	; mask
	sta mask
	cmp #0	; none?
	beq freeze1	; yes - disable interrupts
	lda vic+26
	ora #4	; enable interrupts
freeze2:	sta  vic+26
	cli 
	jmp main
;
freeze1:	lda  vic+26
	and #$fb
	jmp freeze2
;
fr_stat:
	lda coll_reg
	jmp true2
;
;
;brk

	.endscope 

;***********************************************
; pascal compiler
; for commodore 64
; part 4
; authors_ nick gammon & sue gobbett
;  himem_$8500  sym $9500
;***********************************************

	.scope 

	v1 = p1
	init = v1
	getnext = v1+3
	comstl = v1+6
	isithx = v1+9
	isital = v1+12
	isitnm = v1+15
	char = v1+18
	gen2_b = v1+21
	dishx = v1+24
	error = v1+27
	prbyte = v1+51
	gtoken = v1+54
	spare2 = v1+57
	fixad = v1+60
	pshwrk = v1+63
	pulwrk = v1+66
	pc = v1+69
	pt = v1+72
	pl = v1+75
	token1 = v1+78
	getans = v1+81
	putsp = v1+84
	dispad = v1+87
	crout = v1+90
	get_num = v1+96
	get_hex = v1+99
	pause = v1+105
	home = v1+108
	rdkey = v1+111

;***********************************************
; part 3 vectors
;***********************************************

	v3 = p3
	start = v3
	restart = v3+3
	dsp_bin = v3+6
	st_cmp = p3+9
	st_syn = p3+12
	debug = p3+15
	bell1_ax = p3+24
	mov_spt = p3+27
	spt_stat = p3+30
	sprt_x = p3+33
	sprt_y = p3+36
	stop_spt = p3+39
	strt_spt = p3+42
	anm_spt = p3+45
	loadit = p3+48
	saveit = p3+51
	freeze_s = p3+54
	fr_stat = p3+57
	x_open = p3+60
	x_close = p3+63
	x_put = p3+66
	x_get = p3+69

;***********************************************
; part 5 vectors
;***********************************************

	editor = p5
	getln = p5+3
	getlnz = p5+6
	getln1 = p5+9

;***********************************************
; part 4 starts here
;***********************************************

;.org $a380 ; p4

;***********************************************
; part 4 vectors
;***********************************************

	jmp interj
	jmp dis4
	jmp break
	jmp chk_err
	jmp main
	jmp mainp
	jmp chk_top
	jmp true2
	jmp pultop
	jmp s_pnt2

masks:	.byte $01,$02,$04,$08,$10,$20,$40,$80
xmasks:		; complement of above
	.byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f

;
dm5:	.byte $b6,$c7,$0d
dm6:	.byte "bREAK ...",$0d
dm7:	.byte $bd," OCCURRED AT",$b0,32

;
; display (x) characters from (work)
;
dis4:	txa
	pha 
	jsr putsp
	pla 
	tax 
dis5:	ldy  #0
	lda (work),y
	inc work
	bne dis5_a
	inc work+1
dis5_a:
	tay 
	txa 
	pha 
	tya 
	jsr dishx
	pla 
	tax 
	dex 
	bne dis5
	rts 

; interpreter initialization
;
interj:
	lda #$7f
	sta cia2+13
	jmp sound_cl	; clear sid, go to main

;
;
bell1:
	pha 
	lda #0
	sta running
	jsr crout
	pla 
	rts 

;
runerr:	jsr  bell1
	lda #<dm7
	ldx #>dm7
	ldy #15
	jsr pt
	lda lastp+1
	jsr prbyte
	lda lastp
	jsr dishx
finishd:
	lda #0
	sta queue	; clear keyboard queue
	jsr crout
	lda #<fin_msg
	ldx #>fin_msg
	ldy #30
	jsr pt
	jsr rdkey	; wait till message seen
	jmp restart

;
fin_msg:	.byte "RUN FINISHED - PRESS A KEY ..."

;
chk_kbd:
	cmp #$aa	; commodore/n
	bne chk_notn
	jsr getin
	lda #0
	sta dbgflg
	sec 
	rts 
chk_notn:
	cmp #$a3	; commodore/t
	bne chk_nott
	jsr getin
	lda #$80
	sta dbgflg
	sta dcode
	sec 
	rts 
chk_nott:
	cmp #$ac	; commodore/d
	bne chk_notd
	jsr getin
	lda #1
	sta dbgflg
	sta dcode
	sec 
	rts 
chk_notd:
	clc 
	rts 

;
outcr:
	jsr crout	; output c/r
	jmp main

;
lowlit:	sty  reg+1
	and #$7f
	sta reg
mainp:	jsr  pshtop
main:	lda  dbgflg
	beq main_2
	jsr debug
main_2:
	jsr stop
	bne main_3
	jmp break	; stop pressed
main_3:	lda  queue	; key in queue?
	beq main_ok	; no
	lda kbd_buf	; what is it?
	jsr chk_kbd
main_ok:
	lda p
	sta lastp
	lda p+1
	sta lastp+1
	ldy #0
	sty regb
	lda (p),y
	bmi main_5
	cmp #$62
	bcs invins
main_5:
	inc p
	bne main_1
	inc p+1
main_1:
	tax 
	bmi lowlit
	lda exadtbh,x
	pha 
	lda exadtbl,x
	pha 
	rts 

;
notimp:
invins:
	lda #<dm5
	ldx #>dm5
notim1:
	jsr bell1
	jsr pl
	jmp runerr
;
break:
	lda #<dm6
	ldx #>dm6
	jmp notim1
;
exadtbh:
	.hibytes lit-1
	.hibytes def_sprt-1
	.hibytes neg-1
	.hibytes hplot-1
	.hibytes add-1
	.hibytes tohplot-1
	.hibytes sub-1
	.hibytes getkey-1
	.hibytes mul-1
	.hibytes clear-1
	.hibytes div-1
	.hibytes mod-1
	.hibytes adrnn-1
	.hibytes adrnc-1
	.hibytes adran-1
	.hibytes adrac-1
	.hibytes eql-1
	.hibytes finishd-1
	.hibytes neq-1
	.hibytes cur-1
	.hibytes lss-1
	.hibytes freeze_s-1
	.hibytes geq-1
	.hibytes inh-1
	.hibytes gtr-1
	.hibytes leq-1
	.hibytes orr-1
	.hibytes xxxand-1
	.hibytes inp-1
	.hibytes inpc-1
	.hibytes out-1
	.hibytes outc-1
	.hibytes xxxeor-1
	.hibytes ouh-1
	.hibytes shl-1
	.hibytes ous-1
	.hibytes shr-1
	.hibytes ins-1
	.hibytes xxxinc-1
	.hibytes cll-1
	.hibytes xxxdec-1
	.hibytes rtn-1
	.hibytes mov-1
	.hibytes cla-1
	.hibytes lod-1
	.hibytes lodc-1
	.hibytes xxxlda-1
	.hibytes ldac-1
	.hibytes ldi-1
	.hibytes ldic-1
	.hibytes sto-1
	.hibytes stoc-1
	.hibytes xxxsta-1
	.hibytes stac-1
	.hibytes sti-1
	.hibytes stic-1
	.hibytes abscll-1
	.hibytes wait-1
	.hibytes xor-1
	.hibytes int-1
	.hibytes xxxjmp-1
	.hibytes jmz-1
	.hibytes jm1-1
	.hibytes sprite-1
	.hibytes mve_sprt-1
	.hibytes voice-1
	.hibytes graphics-1
	.hibytes sound-1
	.hibytes set_clk-1
	.hibytes scroll-1
	.hibytes sp_coll-1
	.hibytes bk_coll-1
	.hibytes cursorx-1
	.hibytes cursory-1
	.hibytes clock-1
	.hibytes paddle-1
	.hibytes sprt_x-1
	.hibytes joy-1
	.hibytes sprt_y-1
	.hibytes osc3-1
	.hibytes voice3-1
	.hibytes scrollx-1
	.hibytes scrolly-1
	.hibytes spt_stat-1
	.hibytes mov_spt-1
	.hibytes stop_spt-1
	.hibytes strt_spt-1
	.hibytes anm_spt-1
	.hibytes abs-1
	.hibytes invalid-1
	.hibytes loadit-1
	.hibytes saveit-1
	.hibytes x_open-1
	.hibytes fr_stat-1
	.hibytes outcr-1
	.hibytes x_close-1
	.hibytes x_get-1
	.hibytes x_put-1
exadtbl:
	.lobytes lit-1
	.lobytes def_sprt-1
	.lobytes neg-1
	.lobytes hplot-1
	.lobytes add-1
	.lobytes tohplot-1
	.lobytes sub-1
	.lobytes getkey-1
	.lobytes mul-1
	.lobytes clear-1
	.lobytes div-1
	.lobytes mod-1
	.lobytes adrnn-1
	.lobytes adrnc-1
	.lobytes adran-1
	.lobytes adrac-1
	.lobytes eql-1
	.lobytes finishd-1
	.lobytes neq-1
	.lobytes cur-1
	.lobytes lss-1
	.lobytes freeze_s-1
	.lobytes geq-1
	.lobytes inh-1
	.lobytes gtr-1
	.lobytes leq-1
	.lobytes orr-1
	.lobytes xxxand-1
	.lobytes inp-1
	.lobytes inpc-1
	.lobytes out-1
	.lobytes outc-1
	.lobytes xxxeor-1
	.lobytes ouh-1
	.lobytes shl-1
	.lobytes ous-1
	.lobytes shr-1
	.lobytes ins-1
	.lobytes xxxinc-1
	.lobytes cll-1
	.lobytes xxxdec-1
	.lobytes rtn-1
	.lobytes mov-1
	.lobytes cla-1
	.lobytes lod-1
	.lobytes lodc-1
	.lobytes xxxlda-1
	.lobytes ldac-1
	.lobytes ldi-1
	.lobytes ldic-1
	.lobytes sto-1
	.lobytes stoc-1
	.lobytes xxxsta-1
	.lobytes stac-1
	.lobytes sti-1
	.lobytes stic-1
	.lobytes abscll-1
	.lobytes wait-1
	.lobytes xor-1
	.lobytes int-1
	.lobytes xxxjmp-1
	.lobytes jmz-1
	.lobytes jm1-1
	.lobytes sprite-1
	.lobytes mve_sprt-1
	.lobytes voice-1
	.lobytes graphics-1
	.lobytes sound-1
	.lobytes set_clk-1
	.lobytes scroll-1
	.lobytes sp_coll-1
	.lobytes bk_coll-1
	.lobytes cursorx-1
	.lobytes cursory-1
	.lobytes clock-1
	.lobytes paddle-1
	.lobytes sprt_x-1
	.lobytes joy-1
	.lobytes sprt_y-1
	.lobytes osc3-1
	.lobytes voice3-1
	.lobytes scrollx-1
	.lobytes scrolly-1
	.lobytes spt_stat-1
	.lobytes mov_spt-1
	.lobytes stop_spt-1
	.lobytes strt_spt-1
	.lobytes anm_spt-1
	.lobytes abs-1
	.lobytes invalid-1
	.lobytes loadit-1
	.lobytes saveit-1
	.lobytes x_open-1
	.lobytes fr_stat-1
	.lobytes outcr-1
	.lobytes x_close-1
	.lobytes x_get-1
	.lobytes x_put-1

getadr:	ldy  #0
	lda (p),y
	sta count1
	lda base+1
	ldx base
get2:
	sta data+1
	stx data
	tay 
	lda count1
	beq get1
	sec 
	txa 
	sbc #2
	sta work
	tya 
	sbc #0
	sta work+1
	ldy #0
	lda (work),y
	iny 
	tax 
	lda (work),y
	dec count1
	jmp get2
get1:
	ldy #1
	clc 
	lda (p),y
	adc data
	sta data
	iny 
	lda (p),y
	adc data+1
	sta data+1
	lda p
	clc 
	adc #3
	sta p
	bcc get1_a
	inc p+1
get1_a:
	rts 
pultop:
	ldy #0
	lda (t),y
	sta reg
	iny 
	lda (t),y
	sta reg+1
	iny 
	lda (t),y
	sta regb
	lda t
	clc 
	adc #3
	sta t
	bcc pul_end
	inc t+1
pul_end:
	lda reg
	ldx reg+1
	ldy regb
	rts 
pulboth:	jsr  pultop	; pulls both of them
pultop2:
	ldy #0
	lda (t),y
	sta reg2
	iny 
	lda (t),y
	sta reg2+1
	iny 
	lda (t),y
	sta reg2b
	lda t
	clc 
	adc #3
	sta t
	bcc pul2_end
	inc t+1
pul2_end:
	lda reg2
	ldx reg2+1
	ldy reg2b
	rts 
pshtop:
	sec 
	lda t
	sbc #3
	sta t
	bcs psh1
	dec t+1
psh1:
	ldy #0
	lda reg
	sta (t),y
	iny 
	lda reg+1
	sta (t),y
	iny 
	lda regb
	sta (t),y
	rts 
getlit:
	ldy #0
	lda (p),y
	sta reg
	iny 
	lda (p),y
	sta reg+1
	lda p
	clc 
	adc #2
	sta p
	bcc get_end
	inc p+1
get_end:	rts
;
;
lit:	jsr  getlit
	dey 
	lda (p),y
	sta regb
	inc p
	bne lit1
	inc p+1
lit1:
	jmp mainp
;
neg:	jsr  pultop
	sec 
	lda #0
	sbc reg
	sta reg
	lda #0
	sbc reg+1
	sta reg+1
	lda #0
	sbc regb
	sta regb
	jmp mainp
;
;
add:	jsr  pulboth
	clc 
	adc reg
	sta reg
	txa 
	adc reg+1
	sta reg+1
	tya 
	adc regb
	sta regb
	jmp mainp
;
sub:	jsr  substk
	jmp mainp
;
mul:
	jsr fndsgn
	ldx #24
mul5:	asl  res
	rol res+1
	rol res+2
	asl dvdn
	rol dvdn+1
	rol dvdn+2
	bcc mul6
	clc 
	lda mcand
	adc res
	sta res
	lda mcand+1
	adc res+1
	sta res+1
	lda mcand+2
	adc res+2
	sta res+2
mul6:	dex
	bne mul5
	jmp fixsgn
;
chk_top:
	pha 	; limit
	jsr pultop
	dec reg	; make zero relative
	lda reg+1
	ora regb
	bne chk_err
	pla 
	cmp reg
	bcc chk_err	; too big
	lda reg
	rts 	; ok
;
chk_err:
	lda #<chk_mg
	ldx #>chk_mg
	jmp notim1
;
chk_mg:	.byte $b6,$d9," IN FUNCTION CALL",$0d

;
mve_sprt:
	jsr pultop
	sta ypos
	jsr pultop
	sta xposl
	txa 
	and #1
	sta xposh
	lda #7
	jsr chk_top
	tax 	; sprite number
	lda #0
	sta s_active,x	; non-active now
	lda xposh
	beq mve_1
	lda masks,x
mve_1:	sta  temp
	lda xmasks,x
	and vic+$10
	ora temp	; set high order x bit on/off
	sta vic+$10
	txa 
	asl 
	tax 
	lda xposl
	sta vic,x	; low order 8 bits of position
	lda ypos
	sta vic+1,x	; y co-ord
	jmp main
;
invalid:
	lda dos_flg
	jmp true2
;
sp_coll:
	lda vic+30
	jmp true2
;
bk_coll:
	lda vic+31
	jmp true2
;
cursorx:
	sec 
	jsr plot
	iny 
	tya 
	jmp true2
;
cursory:
	sec 
	jsr plot
	inx 
	txa 
	jmp true2
;
clock:
	lda #3
	jsr chk_top
	tax 
	lda cia2+8,x	; 1 = 10ths, 2 = secs etc.
	sta temp+1
	and #$7f
	pha 
	lsr 
	lsr 
	lsr 
	lsr 
	asl 
	sta temp	; times 2
	asl 
	asl 	; times 8
	adc temp	; times 10
	sta temp
	pla 
	and #$0f
	clc 
	adc temp
	cpx #3	; asking for hours, oh newt?
	bne clock_2	; forget it then
	cmp #12	; 12 o'clock?
	bne clock_3	; no
	lda #0	; make 12=0 so output looks right
clock_3:	ldy  temp+1	; pm?
	bpl clock_2
	clc 
	adc #12
clock_2:
	jmp true2	; answer
;
padl_rd:
	sei 
	lda cia1+2	; save ddr register
	pha 
	lda #$c0
	sta cia1+2	; set porta for read
	txa 	; which paddle to read
	sta cia1
	ldy #$81	; wait a while
pdlrd_2:
	nop 
	dey 
	bne pdlrd_2
	lda sid+25	; x value
	sta reg
	lda sid+26
	sta reg+1
	jmp joy_rd1
;
wait:
	jsr pultop	; raster line
wait_dly:	lda  vic+$11	; msb bit of raster
	and #$80
	cmp reg+1
	bcc wait_dly	; not yet
	lda vic+$12	; other bits
	cmp reg
	bcc wait_dly	; too low
	jmp main
;
paddle:
	lda #1
	jsr chk_top
	bne paddle2
;
paddle1:
	ldx #$40
padl2_a:
	jsr padl_rd
	jmp mainp
;
paddle2:
	ldx #$80
	bne padl2_a

;
joy_rd:	sei
	lda cia1+2,y
	pha 
	lda #0
	sta cia1+2,y	; ddr
	lda cia1,y	; read joystick
	and #$1f
	eor #$1f	; reverse
	tax 
joy_rd1:
	pla 
	sta cia1+2,y
	lda #$7f	;was $00 in the v3.0 source -cjb
	sta cia1
	txa 
	cli 
	rts 

;
joy:	lda #1
	jsr chk_top
	bne joy2

;
joy1:	ldy #1
joy1_a:	jsr joy_rd
	jmp true2

;
joy2:
	ldy #0
	beq joy1_a
;
osc3:
	lda sid+27
	jmp true2
;
voice3:
	lda sid+28
	jmp true2
;
scrollx:
	lda vic+$16
scrollx1:	and  #7
	jmp true2
;
scrolly:
	lda vic+$11
	jmp scrollx1
;
addit:	tax
	beq addit_9
	lda #0
	clc 
addit_1:	adc  #1
	dex 
	bne addit_1
addit_9:	rts

;
set_clk:
	lda cia2+14
	ora #$80
	sta cia2+14
	lda cia2+15
	and #$7f
	sta cia2+15
	jsr pultop
	pha 
	jsr pultop
	pha 
	jsr pultop
	pha 
	jsr pultop
	ldx #0
	cmp #12
	bcc clk_am
	ldx #$80
	sec 
	sbc #12	; back to range 0 to 11
clk_am:	stx  cntr
	sed 
	jsr addit
	ora cntr
	sta cia2+11
	pla 
	jsr addit
	sta cia2+10
	pla 
	jsr addit
	sta cia2+9
	pla 
	jsr addit
	sta cia2+8
	cld 
	jmp main
;
scroll:
	jsr pultop
	and #7	; y co-ord
	sta fnc_val
	lda vic+$11
	and #$f8
	ora fnc_val
	sta vic+$11
	jsr pultop
	and #7	; x co-ord
	sta fnc_val
	lda vic+$16
	and #$f8
	ora fnc_val
	sta vic+$16
	jmp main
;
get_bnk:
	lda cia2+2
	and #$fc
	sta cia2+2	; set data direction to read
	lda cia2
	and #3	; video bank
	eor #$ff	; make zero relative
	asl 
	asl 
	asl 
	asl 
	asl 
	asl 
	sta temp+1
	rts 
;
clear:
	jsr pultop
	and #$0f
	sta fnc_val	; colour
	jsr pultop
	asl 
	asl 
	asl 
	asl 
	ora fnc_val
	sta fnc_val
	jsr get_bnk
	lda vic+$18	; character base
	and #$0e
	asl 
	asl 
	ora temp+1
	sta temp+1
	cmp #$04
	bcs clr_2
clr_err:	jmp  chk_err	; too low
clr_2:	lda  #0
	sta temp
	lda vic+17	; mode
	and #$20
	beq clr_err	; not bit map
	lda #<8000	; hi-res (bit map)
	sta reg
	lda #>8000
	sta reg+1
	ldy #0
	tya 
	jsr clr_loop	; clear character memory
	jsr get_bnk
	lda vic+$18	; now do screen memory
	and #$f0
	lsr 
	lsr 
	ora temp+1
	sta temp+1
	cmp #$04
	bcs clr_3
	jmp chk_err	; too low
clr_3:
	lda #0
	tay 
	sta temp
	lda #<1000
	sta reg
	lda #>1000
	sta reg+1
	lda fnc_val	; colour
	jsr clr_loop	; clear screen memory
	jmp main
;
clr_loop:
	sta (temp),y
	inc temp
	bne clr_1
	inc temp+1
clr_1:
	dec reg
	ldx reg
	cpx #$ff
	bne clr_loop
	dec reg+1
	bpl clr_loop
	rts 
;
getkey:
	jsr getin
	jmp true2
;
;
sprite:
	jsr pultop
	sta fnc_val
	lda #6
	jsr chk_top	; function
	sta function
	lda #7
	jsr chk_top
	sta spriteno
	lda function
	beq sprt_col	; set colour
	cmp #1
	beq sprt_pnt	; point sprite
	asl 
	tax 	; offset into table
	lda sprt_tb-4,x
	sta temp
	lda sprt_tb-3,x
	sta temp+1
	ldx spriteno
	ldy #0
	lda fnc_val
	and #1
	beq sprt_1
	lda masks,x
sprt_1:	sta  temp1
	lda xmasks,x
	and (temp),y
	ora temp1	; set bit
	jmp gr_4
;
sprt_col:
	ldx spriteno
	lda fnc_val
	and #15
	sta vic+$27,x	; set colour
	jmp main
;
sprt_pnt:
	lda #0
	ldy spriteno
	sta s_animct,y
s_pnt2:
	jsr get_bnk
	lda vic+$18
	and #$f0	; video base
	lsr 
	lsr 
	clc 
	adc #3
	adc temp+1	; add bank
	sta temp+1
	lda #$f8
	sta temp	; sprite pointers
	ldy spriteno
	lda fnc_val
	jmp gr_4	; point sprite pointer

;
sprt_tb:
	.word vic+$1c
	.word vic+$1d
	.word vic+$17
	.word vic+$1b
	.word vic+$15

;
w_base:
	jsr get_bnk
	lda fnc_val
	and #$0f
	asl 
	asl 	; times 4
	ora temp+1	; bank
	sta hibase
	jmp main
;
graphics:
	jsr pultop
	sta fnc_val
	lda #17
	jsr chk_top
	sta function
	cmp #17
	beq w_base	; write base
	cmp #6
	bne gr_3
	lda cia2+2
	ora #3	; set data direction register
	sta cia2+2
	lda fnc_val
	eor #$ff
	sta fnc_val
	lda function
gr_3:	asl
	asl 
	clc 
	adc function	; times 5
	tax 
	lda g_table,x	; address to patch
	sta temp
	lda g_table+1,x
	sta temp+1
	lda g_table+2,x	; mask
	and fnc_val
	ldy g_table+3,x	; bit to shift left
	beq gr_1	; none
gr_2:	asl
	dey 
	bne gr_2
gr_1:	sta  fnc_val
	lda (temp),y	; old value of location
	and g_table+4,x	; mask out required bits
	ora fnc_val	; or in new bits
gr_4:	sta  (temp),y	; new value
	jmp main	; finished!

;
g_table:
;
; graphics controls
;
	.word vic+$11	; hires
	.byte $01,$05,$df
	.word vic+$16	; multicolour
	.byte $01,$04,$ef
	.word vic+$11	; ext. bkgnd
	.byte $01,$06,$bf
	.word vic+$16	; 40 cols
	.byte $01,$03,$f7
	.word vic+$11	; 25 lines
	.byte $01,$03,$f7
	.word vic+$11	; blank screen
	.byte $01,$04,$ef
	.word cia2	; bank select
	.byte $03,$00,$fc
	.word vic+$18	; char gen base
	.byte $07,$01,$f1
	.word vic+$18	; video base
	.byte $0f,$04,$0f
	.word $286	; cursor colour
	.byte $0f,$00,$f0
	.word vic+$20	; border colour
	.byte $0f,$00,$f0
	.word vic+$21	; other colours
	.byte $0f,$00,$f0
	.word vic+$22
	.byte $0f,$00,$f0
	.word vic+$23
	.byte $0f,$00,$f0
	.word vic+$24
	.byte $0f,$00,$f0
	.word vic+$25
	.byte $0f,$00,$f0
	.word vic+$26
	.byte $0f,$00,$f0
;
; voice controls
;
	.word sid+5	; attack
	.byte $0f,$04,$0f
	.word sid+5	; decay
	.byte $0f,$00,$f0
	.word sid+6	; sustain
	.byte $0f,$04,$0f
	.word sid+6	; release
	.byte $0f,$00,$f0
	.word sid+4	; play
	.byte $01,$00,$fe
	.word sid+4	; sync
	.byte $01,$01,$fd
	.word sid+4	; ring mod
	.byte $01,$02,$fb
	.word sid+4	; triangle
	.byte $01,$04,$ef
	.word sid+4	; sawtooth
	.byte $01,$05,$df
	.word sid+4	; pulse
	.byte $01,$06,$bf
	.word sid+4	; noise
	.byte $01,$07,$7f
	.word sid+4	; test
	.byte $01,$03,$f7
;
; sound controls
;
	.word sid+24	; volume
	.byte $0f,$00,$f0
	.word sid+23	; resonance
	.byte $0f,$04,$0f
	.word sid+24	; low pass
	.byte $01,$04,$ef
	.word sid+24	; band pass
	.byte $01,$05,$df
	.word sid+24	; high pass
	.byte $01,$06,$bf
	.word sid+24	; cutoff voice3
	.byte $01,$07,$7f
;
; end of table
;

;
def_sprt:
	lda #240	; will become 60
	sta temp
	jsr get_bnk
	ldy #63	; get pointer off stack first
	lda (t),y
	lsr 
	ror temp
	lsr 
	ror temp
	clc 
	adc temp+1	; add in bank
	sta temp+1
	cmp #$04	; too low?
	bcs def_2	; no
	jmp chk_err
def_2:	lda  #21
def_1:	pha	; save counter
	jsr pultop	; get row
	ldy #2	; do it in reverse order
	lda reg
	sta (temp),y
	dey 
	lda reg+1
	sta (temp),y
	dey 
	lda regb
	sta (temp),y
	dec temp
	dec temp
	dec temp	; (will not cross page boundary)
	pla 
	tax 	; counter
	dex 
	txa 
	bne def_1	; more to go
	jsr pultop	; discard pointer (read earlier)
	jmp main
;
voice:
	jsr pultop
	sta fnc_val
	stx fnc_val+1
	lda #14
	jsr chk_top
	sta function
	lda #2
	jsr chk_top
	sta voiceno
	sta temp1	; save for filter
	asl 
	asl 
	asl 	; times 8
	sec 
	sbc voiceno	; times 7
	sta voiceno
	lda function
	beq freq
	cmp #1
	beq width
	cmp #2
	beq filter
	clc 
	adc #14	; bypass 17 graphics entries ( minus 3 not in table )
vc_3:	sta  function
	asl 
	asl 
	clc 
	adc function	; times 5
	tax 
	lda g_table,x
	clc 
	adc voiceno	; low-order address
	sta temp
	lda g_table+1,x
	sta temp+1
	lda g_table+2,x	; mask
	and fnc_val
	ldy g_table+3,x	; bits to shift
	beq vc_1
vc_2:	asl
	dey 
	bne vc_2
vc_1:	sta  fnc_val
	ldy temp	; offset into sid image
	lda sid_img,y	; get previous value
	and g_table+4,x	; mask out new bits
	ora fnc_val	; new value
	sta sid_img,y	; new value
	ldy #0
	jmp gr_4	; and do sid itself
;
freq:
	ldx voiceno
	lda fnc_val
	sta sid,x
	lda fnc_val+1
	sta sid+1,x
	jmp main
;
width:
	ldx voiceno
	lda fnc_val
	sta sid+2,x
	lda fnc_val+1
	and #$f
	sta sid+3,x
	jmp main
;
filter:
	ldx temp1	; un-multiplied voice
	lda fnc_val
	and #1
	beq filt_1
	lda masks,x
filt_1:	sta  temp
	lda xmasks,x
	and sid_img+23
	ora temp
	sta sid+23
	sta sid_img+23
	jmp main
;
sound:
	sty voiceno	; not voice relative
	jsr pultop
	sta fnc_val
	stx fnc_val+1
	sty fnc_val+2
	lda #8
	jsr chk_top
	beq sound_cl
	cmp #1
	beq sound_f
	cmp #2
	beq delay
	clc 
	adc #26	; bypass 17 graphics + 12 voice - 3 not in table
	jmp vc_3	; handle with table
;
sound_cl:	ldy  #24
	lda #0
sound_c1:	sta  sid,y
	sta sid_img,y
	dey 
	bpl sound_c1
	jmp main
;
sound_f:	lda  fnc_val
	and #7
	sta sid+21
	lda fnc_val
	lsr fnc_val+1
	ror 
	lsr fnc_val+1
	ror 
	lsr fnc_val+1
	ror 
	sta sid+22
	jmp main
;
delay:
	lda cia2+14
	and #$c0
	sta cia2+14
	lda #0
	sta cia2+15
	lda fnc_val
	sta cia2+6
	lda fnc_val+1
	sta cia2+7
	lda #$66	; calibrated to give 100'ths of a second
	sta cia2+4
	lda #$26
	sta cia2+5
	lda #$59	; one shot/ count ta
	sta cia2+15	; start tb
	lda cia2+14
	ora #$11
	sta cia2+14	; start ta
del_wait:	lda  cia2+15	; finished?
	and #1
	bne del_wait	; nope
	jmp main
;
;
cur:	lda  #40
	jsr chk_top
cur1:	pha
	lda #25
	jsr chk_top
cur2:	tax
	pla 
	tay 
	clc 
	jsr plot	; set cursor position
	jmp main

;
mod:	jsr  fndsgn
	jsr divide
	lda remain
	sta res
	lda remain+1
	sta res+1
	lda remain+2
	sta res+2
	jmp div14

;
divby0:	.byte "dIVIDE BY",$be,$0d

;
div:	jsr  fndsgn
	jsr divide
div14:	jmp  fixsgn

;
divide:	lda  divisor
	ora divisor+1
	ora divisor+2
	bne div1
	lda #<divby0
	ldx #>divby0
	jmp notim1

;
div1:	jsr  zerres	; zero result
	sta remain
	sta remain+1
	sta remain+2
	lda #24
l_5:	sta  cntr
l_10:
	asl dvdn
	rol dvdn+1
	rol dvdn+2
	rol remain
	rol remain+1
	rol remain+2
	sec 
	lda remain
	sbc divisor
	tax 
	lda remain+1
	sbc divisor+1
	tay 
	lda remain+2
	sbc divisor+2
	bmi l_20
	sta remain+2
	tya 
	sta remain+1
	txa 
	sta remain
	sec 
	bcs l_30
l_20:
	clc 
l_30:
	rol res
	rol res+1
	rol res+2
	dec cntr
	bne l_10
	rts 

;
abs:	jsr  pultop
	jsr absreg
	jmp inp3

;
zerres:	lda  #0
	sta res
	sta res+1
	sta res+2
	rts 

;
absreg:	lda  regb
	bpl abs1
	sec 
	lda #0
	sbc reg
	tax 
	lda #0
	sbc reg+1
	tay 
	lda #0
	sbc regb
	jmp abs2
abs1:	ldx  reg
	ldy reg+1
	lda regb
abs2:	rts

;
fndsgn:	jsr  zerres	; zero result
	jsr pultop
	jsr pultop2
	lda regb
	and #$80
	sta rmndr
	lda reg2b
	and #$80
	eor rmndr
	sta rmndr
	jsr absreg
	sta divisor+2
	sty divisor+1
	stx divisor
	lda reg2b
	bpl mul3
	sec 
	lda #0
	sbc reg2
	tax 
	lda #0
	sbc reg2+1
	tay 
	lda #0
	sbc reg2b
	jmp mul4
mul3:	ldx  reg2
	ldy reg2+1
	lda reg2b
mul4:	stx  dvdn
	sty dvdn+1
	sta dvdn+2
	rts 

;
fixsgn:	lda  rmndr
	bpl mul7
	sec 
	lda #0
	sbc res
	tax 
	lda #0
	sbc res+1
	tay 
	lda #0
	sbc res+2
	jmp mul8
mul7:	ldx  res
	ldy res+1
	lda res+2
mul8:	jmp  inp3

;
eql:	jsr  pulboth
	cmp reg
	bne false
	txa 
	cmp reg+1
	bne false
	tya 
	cmp regb
	bne false
;
true:	lda  #1
true2:	sta  reg
	lda #0
	sta reg+1
true1:	sta  regb
	jmp mainp
;
false:	lda  #0
	sta reg
	sta reg+1
	beq true1
;
;
substk:	jsr  pulboth
	sec 
	sbc reg
	sta reg
	tay 
	txa 
	sbc reg+1
	sta reg+1
	tax 
	lda reg2b
	sbc regb
	sta regb
	rts 
;
neq:	jsr  substk
	bne true
	tya 
	bne true
	txa 
	bne true
	beq false
;
lss:	jsr  substk
	bmi true
	bpl false
;
gtr:	jsr  substk
	bmi false
	bne true
	tya 
	bne true
	txa 
	bne true
	beq false
;
geq:	jsr  substk
	bmi false
	bpl true
;
leq:	jsr  substk
	bmi true
	bne false
	tya 
	bne false
	txa 
	bne false
	beq true
;
xor:	jsr  pulboth
	eor reg
	sta reg
	txa 
	eor reg+1
	sta reg+1
	tya 
	eor regb
	jmp true1
;
orr:	jsr  pulboth
	ora reg
	sta reg
	txa 
	ora reg+1
	sta reg+1
	tya 
	ora regb
	jmp true1
;
xxxand:	jsr  pulboth
	and reg
	sta reg
	txa 
	and reg+1
	sta reg+1
	tya 
	and regb
	jmp true1
;
xxxeor:	jsr  pultop
	lda reg
	bne eor1
	lda reg+1
	bne eor1
	lda regb
	bne eor1
	jmp true
eor1:	jmp  false

;
shl:	jsr  pultop2
	and #$1f
	pha 
	jsr pultop
	pla 
	sta reg2
	beq inc1
shl1:	asl  reg
	rol reg+1
	rol regb
	dec reg2
	bne shl1
	beq inc1
;
shr:	jsr  pultop2
	and #$1f
	pha 
	jsr pultop
	pla 
	sta reg2
	beq inc1
shr1:	lsr  regb
	ror reg+1
	ror reg
	dec reg2
	bne shr1
inc1:	jmp  mainp
;
;
xxxinc:	clc
	lda (t),y
	adc #1
	sta (t),y
	iny 
	lda (t),y
	adc #0
	sta (t),y
	iny 
	lda (t),y
	adc #0
inc2:	sta  (t),y
	jmp main
;
xxxdec:	sec
	lda (t),y
	sbc #1
	sta (t),y
	iny 
	lda (t),y
	sbc #0
	sta (t),y
	iny 
	lda (t),y
	sbc #0
	jmp inc2
;
mov:	lda  (t),y
	pha 
	iny 
	lda (t),y
	pha 
	iny 
	lda (t),y
	pha 
	lda t
	sec 
	sbc #3
	sta t
	bcs mov1
	dec t+1
mov1:	pla
	sta (t),y
	dey 
	pla 
	sta (t),y
	dey 
	pla 
	sta (t),y
	jmp main
;
lodc:	jsr  getadr
lod3:	ldy  #2
lod3_a:	lda  #0
	sta reg+1
	sta regb
	lda (data),y
	sta reg
	jmp mainp
;
lod:	jsr  getadr
lod2:	ldy  #0
	lda (data),y
	sta reg
	iny 
	lda (data),y
	sta reg+1
	iny 
	lda (data),y
	jmp true1
;
ldac:	jsr  pultop
	sta data
	stx data+1
	ldy #0
	beq lod3_a
;
xxxlda:	jsr  pultop
	sta data
	stx data+1
	jmp lod2
;
getidc:
	jsr pultop2
	jsr getadr
	jmp getid2
;
getidx:
	jsr pultop2
	asl reg2
	rol reg2+1
	clc 
	adc reg2
	sta reg2
	txa 
	adc reg2+1
	sta reg2+1	; times 3
	jsr getadr
;
getid2:	lda  data
	sec 
	sbc reg2
	sta data
	lda data+1
	sbc reg2+1
	sta data+1
	rts 
;
ldic:	jsr  getidc
	jmp lod3
;
ldi:	jsr  getidx
	jmp lod2
;
stoc:	jsr  getadr
	jsr pultop
	ldy #2
sto5:	sta  (data),y
	jmp main
;
sto:	jsr  getadr
	jsr pultop
sto2:	ldy  #0
	sta (data),y
	iny 
	txa 
	sta (data),y
	lda regb
	iny 
	bne sto5
;
xxxsta:	jsr  pulboth
	ldy #0
	lda reg
	sta (reg2),y
	iny 
	lda reg+1
	sta (reg2),y
	iny 
	lda regb
sta5:	sta  (reg2),y
	jmp main

;
stac:	jsr  pultop
	jsr pultop2
	lda reg
	ldy #0
	beq sta5

;
stic:	jsr  pultop
	sta temp
	jsr getidc
	lda temp
	ldy #2
	bne sto5
;
sti:	jsr  pultop
	sta temp
	stx temp+1
	tya 
	pha 
	jsr getidx
	ldy #0
	lda temp
	sta (data),y
	lda temp+1
	iny 
	sta (data),y
	pla 
	iny 
	bne sto5
;
rtn:	lda  base
	sec 
	sbc #6
	sta work
	lda base+1
	sbc #0
	sta work+1
	ldy #0
	lda (work),y
	sta p
	iny 
	lda (work),y
	sta p+1
	lda base+1
	sta t+1
	lda base
	sta t
	sec 
	sbc #4
	sta work
	lda base+1
	sbc #0
	sta work+1
	ldy #0
	lda (work),y
	sta base
	iny 
	lda (work),y
	sta base+1
	jmp main
;
;
;
inp:
	sty sign
	sty dos_flg
	dey 
	sty running
	ldy #8
	jsr getln1
	lda #<inbuf
	sta nxtchr
	lda #>inbuf
	sta nxtchr+1
	lda inbuf
	jsr chk_kbd
	bcs inp
	cmp #'-'
	bne inp1
	sta sign
	inc nxtchr
	lda inbuf+1
inp1:
	jsr isitnm
	bcs bad_inp
inp_ok:
	jsr get_num
inp4:	bcs  bad_inp
	jsr getnext	; followed by c/r?
	and #$7f
	cmp #$0d
	bne bad_inp	; no
	ldx value
	ldy value+1
	lda value+2
inp3:	sty  reg+1
	stx reg
	ldx #12
	stx running
	jmp true1
;
bad_inp:	lda  #1
	sta dos_flg
	lda #0
	tax 
	tay 
	beq inp3

;
out:	jsr  pultop
	jsr dsp_bin
	jmp main
;
ouh:	jsr  pultop
	lda regb
	jsr prbyte
	lda reg+1
	jsr prbyte
	lda reg
	jsr prbyte
	jmp main

;
ous:	lda p
	clc 
	adc #1
	sta work
	lda p+1
	adc #0
	sta work+1
	lda (p),y
	sta count1	; no. of chars
	clc 
	adc #1
	adc p
	sta p
	bcc ous1
	inc p+1
ous1:	lda  work
	ldx work+1
	ldy count1
	jsr pt
	jmp main

;
inh:
	sty sign
	sty dos_flg
	dey 
	sty running
	ldy #6
	jsr getln1
	lda #>(inbuf-1)
	sta nxtchr+1
	lda #<(inbuf-1)
	sta nxtchr
	lda inbuf
	jsr chk_kbd
	bcs inh
	jsr isithx
	bcc inh_ok
bad_inp2:	jmp  bad_inp
inh_ok:
	jsr get_hex
	jmp inp4
;
abscll:
	sty call
	sty call+1
	jmp cll_a
;
cll:
	lda lastp
	sta call
	lda lastp+1
	sta call+1
cll_a:
	lda (p),y
	sta count1
	iny 
	clc 
	lda (p),y
	adc call
	sta call
	iny 
	lda (p),y
	adc call+1
	sta call+1
	lda p
	clc 
	adc #3
	sta p
	bcc cll4
	inc p+1
cll4:
	lda base+1
	ldx base
cll2:
	sta data+1
	stx data
	tay 
	lda count1
	beq cll3
	sec 
	txa 
	sbc #2
	sta work
	tya 
	sbc #0
	sta work+1
	ldy #0
	lda (work),y
	iny 
	tax 
	lda (work),y
	dec count1
	jmp cll2
cll3:
	lda t
	sta temp
	lda t+1
	sta temp+1
	lda data
	sta reg+1
	lda data+1
	sta regb
	lda base+1
	sta reg
	jsr pshtop
	lda base
	sta regb
	lda temp
	sta base
	lda temp+1
	sta base+1
	lda p
	sta reg
	lda p+1
	sta reg+1
	jsr pshtop
	lda call
	sta p
	lda call+1
	sta p+1
	clc 
	lda t
	adc #6
	sta t
	bcc cll5
	inc t+1
cll5:
	jmp main
;
cla:	jsr  pultop
	lda call_p
	pha 
	lda call_a
	ldx call_x
	ldy call_y
	plp 
	jsr cll_jmp
	php 
	sta call_a
	stx call_x
	sty call_y
	pla 
	sta call_p
	jmp main
cll_jmp:	jmp  (reg)

;
int:	jsr  getlit
	sec 
	lda t
	sbc reg
	sta t
	lda t+1
	sbc reg+1
	sta t+1
	cmp #$c0
	bcc int_err
	jmp main
;
int_err:
	lda #<int_errm
	ldx #>int_errm
	jmp notim1

;
int_errm:	.byte $c6,$b1,$0d	; stack full

;
xxxjmp:	jsr  getlit
	clc 
	lda reg
	adc lastp
	sta p
	lda reg+1
	adc lastp+1
	sta p+1
	jmp main
;
jmz:	jsr pultop
	lda reg
	ora reg+1
	bne nojump
	beq xxxjmp

;
nojump:	jsr  getlit
	jmp main

;
jm1:	jsr pultop
	ora reg+1
	bne xxxjmp
	beq nojump

;
inpc:	jsr  rdkey
	jsr chk_kbd
	bcs inpc
inpc1:	sta  reg
	lda #0
	sta reg+1
	jmp mainp

;
outc:	jsr  pultop
	lda reg
	jsr pc
	jmp main

;
ins:
	lda (p),y
	sta temp
	inc p
	bne ins3
	inc p+1
ins3:
	ldy temp
	jsr getln1
	lda inbuf
	jsr chk_kbd
	bcs ins3
	txa 
	clc 
	adc #1
	cmp temp
	bcc ins1
	lda temp
ins1:
	sta temp+1
	jsr getadr
	ldy #3
	ldx #0
ins2:
	dec data
	lda data
	cmp #$ff
	bne ins4
	dec data+1
ins4:
	lda inbuf,x
	sta (data),y
	inx 
	dec temp+1
	bne ins2
	jmp main
;
hplot:
	jsr pultop
	lda regb
	bne hpl_err
	lda reg
	cmp #200
	bcc hplot_1
hpl_err:	jmp  chk_err	; too big
hplot_1:	sta  ypos
	jsr pultop
	lda vic+$16
	and #$10
	sta voiceno	; multi-colour flag
	beq hplot_si
	asl reg
	rol reg+1	; double x co-ord
hplot_si:
	lda reg
	sta xposl
	tax 
	lda reg+1
	tay 
	cpx #$40
	sbc #1
	bcc hplot_2
	jmp chk_err	; too big
hplot_2:	sty  xposh
	jsr pultop
	lda reg
	and #3
	sta temp1+1	; colour
	lda ypos
	and #$f8
	sta rowl
	lda #0
	asl rowl
	rol 	; x 2
	asl rowl
	rol 	; x 4
	asl rowl
	rol 	; x 8
	sta temp+1
	ldy rowl
	sty temp	; save it
	asl rowl
	rol 	; x 16
	asl rowl
	rol 	; x 32
	clc 
	sta rowh
	lda rowl
	adc temp	; now add 8 giving 40
	sta rowl
	lda rowh
	adc temp+1
	sta rowh
;
	jsr get_bnk
	lda vic+$18
	and #$0e
	asl 
	asl 
	ora temp+1	; character base
	sta temp+1
	lda xposl
	and #$f8
	clc 
	adc rowl
	sta rowl
	lda xposh
	ora temp+1	; bank
	adc rowh
	sta temp+1
	lda ypos
	and #7
	ora rowl
	sta temp
	lda xposl
	and #7
	tax 
	lda voiceno	; multi-colour?
	beq pos_2	; no
	lda temp1+1	; colour
	ldy hshift,x	; bits to shift
	beq pos_3	; none
pos_4:	asl
	dey 
	bne pos_4
pos_3:	sta  temp1
	lda h2masks,x	; bits to mask out
	bne pos_5
;
pos_2:
	lda temp1+1	; colour
	beq pos_1
	lda hmasks,x
pos_1:	sta  temp1
	ldy #0
	lda xhmasks,x
pos_5:
	and (temp),y
	ora temp1
	sta (temp),y
	jmp main
;
hmasks:	.byte $80,$40,$20,$10,$08,$04,$02,$01
xhmasks:	.byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe
hshift:	.byte $06,$06,$04,$04,$02,$02,$00,$00
h2masks:	.byte $3f,$3f,$cf,$cf,$f3,$f3,$fc,$fc

;
tohplot:	jmp  hplot

;
adrnc:
	jsr getadr
adrnc2:
	lda data
	clc 
	adc #2
	sta data
	bcc adrn2
	inc data+1
	bcs adrn2
adrnn:
	jsr getadr
adrn2:
	lda data
	sta reg
	lda data+1
	sta reg+1
	jmp mainp
;
adran:
	jsr getidx
	jmp adrn2
;
adrac:
	jsr getidc
	jmp adrnc2

;
	brk 

	.endscope 

;***********************************************
; pascal compiler
; for commodore 64
; part 5
; authors_ nick gammon & sue gobbett
;  sym $9000
;***********************************************

	.scope 

;***********************************************
; part 1 vectors
;***********************************************

	v1 = p1
	init = v1
	getnext = v1+3
	comstl = v1+6
	isithx = v1+9
	isital = v1+12
	isitnm = v1+15
	char = v1+18
	gen2_b = v1+21
	dishx = v1+24
	error = v1+27
	getchk = v1+30
	chktkn = v1+33
	gennop = v1+36
	genadr = v1+39
	gennjp = v1+42
	gennjm = v1+45
	tknwrk = v1+48
	search = v1+51
	addsym = v1+54
	spare2 = v1+57
	fixad = v1+60
	pshwrk = v1+63
	pulwrk = v1+66
	pc = v1+69
	pt = v1+72
	pl = v1+75
	token1 = v1+78
	getans = v1+81
	putsp = v1+84
	dispad = v1+87
	crout = v1+90
	shlval = v1+93
	get_num = v1+96
	get_hex = v1+99
	fnd_enq = v1+102
	pause = v1+105
	home = v1+108
	rdkey = v1+111

;***********************************************
; part 2 vectors
;***********************************************
	v2 = p2
	tknjmp = v2+60

;***********************************************
; part 3 vectors
;***********************************************

	v3 = p3
	start = v3
	restart = v3+3
	dsp_bin = v3+6
	st_cmp = v3+9
	st_syn = v3+12
	debug = v3+15
	st_edi = v3+18
	st_fil = v3+21
	bell1x = v3+24

;***********************************************
; part 4 vectors
;***********************************************

	interj = p4

;***********************************************
; part 5 starts here
;***********************************************

	.res 6
;.org $b384 ; p5

;***********************************************
; text editor
;***********************************************

	jmp editor
	jmp getln
	jmp getlnz
	jmp getln1
	jmp put_line

gt_no_em:	.byte $b6,$cc,$0d
get_err1:	.byte "pARDON? (",$c9
	.byte " h FOR HELP )",$0d
del_err1:	.byte $b6,$d8,$0d
full_err:	.byte $bf,$b1,$0d
mod_qn1:	.byte " MODIFY "
del_qn1:	.byte " DELETE "
del_qn2:	.byte " LINES",$cb
del:	.byte " dELETED",$0d
help_1:	.byte 13
help_2:	.byte "tHE COMMANDS ARE :",$0d
help_3:	.byte 13
help_4:	.byte $d4,$0d
help_5:	.byte $db
	.byte "d>ELETE",$cd,$cc,$d8,$0d
help_5a:	.byte $db
	.byte "f>IND  ",$cd,$cc,$d8
	.byte " .",$b8
	.byte " .",$0d
help_6:	.byte $db
	.byte "i>NSERT",$cd,$cc,$0d
help_7:	.byte $db
	.byte "l>IST  ",$cd,$cc,$d8,$0d
help_8:	.byte $db
	.byte "m>ODIFY",$cd,$cc,$d8,$0d
help_9:	.byte $d7,$0d
help_9a:	.byte $db
	.byte "r>EPLACE",$cd,$cc,$d8
	.byte " .OLD.NEW.",$0d
help_10:	.byte $d5,$0d
fnd_err1:	.byte $b7,$b8,$0d

;
; editor jump table
;
ed_tbl:
	.byte 'Q'
	.word wrap
	.byte 'I'
	.word insert
	.byte 'H'
	.word help
	.byte 'M'
	.word modify
	.byte 'D'
	.word delete
	.byte 'L'
	.word listlns
	.byte 'F'
	.word listlns
	.byte 'R'
	.word listlns
	.byte 'N'
	.word nlist
	.byte 'C'
	.word st_cmp
	.byte 'S'
	.word st_syn
	.byte 0
;
editor:
	lda #<st_edi
	sta ctrlc_rt
	lda #>st_edi
	sta ctrlc_rt+1
;
get_com:
	ldx #$ff
	txs 
	jsr crout
	jsr get_c1
	jmp get_com	; back for next command
;***********************************************
get_c1:
	lda #$80
	sta running
	lda #0
	sta nln_flag
	lda #58	;colon
	jsr cout	; editor prompt
	jsr get_line
	cpx #0
	beq get_c1
	lda inbuf
	and #$7f
	sta ed_com
	ldx #<ed_tbl
	ldy #>ed_tbl
	jsr tknjmp
	lda #<get_err1
	ldx #>get_err1
	jmp pl
;
nlist:
	lda #1
	sta nln_flag
	jmp listlns
;
;
help:
;***********************************************
	lda #12
	sta ecntr
	lda #<help_1
	ldx #>help_1
prt_help:
	jsr pl
	tya 
	clc 
	adc reg2
	tay 
	lda reg2+1
	adc #0
	tax 
	tya 
	dec ecntr
	bne prt_help
	rts 
;
; read input line - tokenize - count chars to c/r
;
get_line:
	jsr getln1
	lda inbuf
	cmp #160	; if first char on line shift/space remember it
	php 	; save processor flags
;
; now tidy input line for automatic tokenisation
;
	lda #<inbuf
	ldx #>inbuf
	jsr tidy_ent	; off we go
	ldx #0
get_lin9:	lda  inbuf,x	; count up to c/r
	cmp #$0d
	beq get_lin8	; found it
	inx 
	bne get_lin9	; try again
get_lin8:	stx  in_lgth	; save length
	plp 	; back to result of earlier comparison
	rts 
;
;
insert:
;***********************************************
in_2:
	ldy #0
	jsr get_no
	bcc in_5
	rts 
in_5:
	jsr find_ln
in_10:
	jsr pt_ln_no
	jsr get_line
	beq in_11	; stay in input mode if shift/space
	cpx #0
	bne in_11
	rts 
in_11:
	jsr inst_txt
; move line from input
; buffer into text
	lda #<inbuf
	sta to
	lda #>inbuf
	sta to+1
	lda #0
	sta length+1
	lda in_lgth
	sta length
	beq in_22	; zero length - no move
	jsr mv_txt_l
in_22:
	lda pntr
	clc 
	adc length
	sta pntr
	lda pntr+1
	adc length+1
	sta pntr+1
	lda #cr
	ldy #0
	sta (pntr),y
	jsr inc_pntr
	jmp in_10
;
;
inst_txt:
; make room in text
	lda pntr
	sta from
	sta from_st
	sta epntr
	lda pntr+1
	sta from+1
	sta from_st+1
	sta epntr+1
	jsr find_end
	lda from
	clc 
	adc in_lgth
	sta to
	lda from+1
	adc #0
	sta to+1
; source too big ?
	clc 
	lda to
	adc length
	sta epntr
	lda to+1
	adc length+1
	sta epntr
	cmp #>(p1-$18)	; start of g-pascal
	bcc in_13
	lda #<full_err
	ldx #>full_err
	jsr pl
	jmp get_com
in_13:
	jsr inc_to
	jsr mv_txt_r
	lda from_st
	sta from
	lda from_st+1
	sta from+1
	rts 

;
get_no:
;***********************************************
; returns the
; number in value
	iny 
	cpy in_lgth
	bne get_no_2
	ldy #0
	sty value
	sty value+1
	clc 
	rts 
get_no_2:
	lda inbuf,y
	cmp #32
	beq get_no
	jsr isitnm
	bcc get_no_5
gt_no_er:
	lda #<gt_no_em
	ldx #>gt_no_em
	jsr pl
	sec 
	rts 
get_no_5:
	sty pntr
	pha 
	tya 
	clc 
	adc #<inbuf
	sta nxtchr
	lda #0
	sta sign
	adc #>inbuf
	sta nxtchr+1
	pla 
	jsr get_num
	bcs gt_no_er
	lda value+2
	bne gt_no_er
	rts 
;
find_ln:
;***********************************************
	lda ts
	sta pntr
	lda ts+1
	sta pntr+1
	ldy #0
	ldx #0
	sty line_no
	sty line_no+1
	lda value
	bne fl_5
	lda value+1
	bne fl_5
	rts 
fl_loop:
	jsr inc_pntr
fl_5:
	lda (pntr),y
	bne fl_10
	stx line_no
	rts 
fl_10:
	cmp #cr
	bne fl_loop
	inx 
	bne fl_15
	inc line_no+1
fl_15:
	cpx value
	bne fl_loop
	lda line_no+1
	cmp value+1
	bne fl_loop
	stx line_no
	jmp inc_pntr
;
find_end:
; returns length of text
; from 'pointer' to end
; of file in 'length'
	ldx #1
	stx ecntr
	ldy #0
	sty ecntr+1
fe_5:	lda  (epntr),y
	bne fe_10
	stx length
	ldx ecntr+1
	stx length+1
	rts 
fe_10:	inx
	bne fe_15
	inc ecntr+1
fe_15:
	inc epntr
	bne fe_5
	inc epntr+1
	jmp fe_5
;
mv_txt_l:
; note_ moving text
; from 'to' to 'from'
; positions
	jsr zro_cntr
mv_5:	lda  (to),y
	sta (from),y
	jsr inc_ecntr
	lda ecntr
	cmp length
	bne mv_10
	lda ecntr+1
	cmp length+1
	bne mv_10
	rts 
mv_10:
	iny 
	bne mv_5
	inc from+1
	inc to+1
	jmp mv_5
mv_txt_r:
; note to > from
; y zeroed in zro_cntr
	jsr zro_cntr
	lda from
	clc 
	adc length
	sta from
	lda from+1
	adc length+1
	sta from+1
	lda to
	clc 
	adc length
	sta to
	lda to+1
	adc length+1
	sta to+1
	jmp mvr_10
mvr_5:	lda  (from),y
	sta (to),y
	jsr inc_ecntr
	lda ecntr
	cmp length
	bne mvr_10
	lda ecntr+1
	cmp length+1
	bne mvr_10
	rts 
mvr_10:
	dey 
	cpy #$ff
	bne mvr_5
	dec from+1
	dec to+1
	jmp mvr_5
;
zro_cntr:
;***********************************************
	ldy #0
	sty ecntr
	sty ecntr+1
	sty val_cmp
	rts 
;
inc_ecntr:
;***********************************************
	inc ecntr
	bne inc_c_5
	inc ecntr+1
inc_c_5:
	rts 
;
inc_pntr:
;***********************************************
	inc pntr
	bne inc_5
	inc pntr+1
inc_5:	rts
;
modify:
;***********************************************
	jsr listlns
	bcc mod_10
	rts 
mod_10:
	jsr delete
	ldy #0
	jsr get_no
	dec value
	lda value
	cmp #$ff
	bne mod_20
	dec value+1
mod_20:
	lda #'I'	; fool insert
	sta ed_com
	jmp in_5
;
delete:
;***********************************************
	jsr getrange
	bcc del_20
	rts 
del_20:
	jsr dec_from
	jsr frm_addr
	lda line_no
	sta temp
	lda line_no+1
	sta temp+1
	jsr to_addr
	lda pntr
	sta epntr
	lda pntr+1
	sta epntr+1
	jsr find_end
	lda line_no
	sec 
	sbc temp
	sta num_lins
	lda line_no+1
	sbc temp+1
	sta num_lins+1
	lda num_lins
	cmp #5
	bcs del_45
	lda num_lins+1
	beq del_55
del_45:
	lda #$b9
	jsr pc
	lda #$c2
	jsr pc
	lda ed_com
	cmp #'M'
	beq del_50
	lda #<del_qn1
	ldx #>del_qn1
	bne del_52
del_50:
	lda #<mod_qn1
	ldx #>mod_qn1
del_52:
	ldy #$8
	jsr pt
	jsr put_no
	lda #<del_qn2
	ldx #>del_qn2
	ldy #7
	jsr getans
	cmp #'Y'
	beq del_55
	jmp get_com
del_55:
	jsr mv_txt_l
	lda ed_com
	cmp #'M'
	bne del_60
	rts 
del_60:
	jsr crout
	jsr put_no
	lda #<del
	ldx #>del
	jmp pl
;
put_no:
;***********************************************
	lda num_lins
	sta reg
	lda num_lins+1
	sta reg+1
	lda #0
	sta regb
	jmp dsp_bin
;
listlns:
;***********************************************
	lda inbuf+1
	cmp delimit
	beq lis_5
	lda in_lgth
	cmp #1
	bne lis_10
; print all
lis_5:	lda  #0
	tay 
	sta line_no
	sta line_no+1
	lda ts
	sta from
	lda ts+1
	sta from+1
	lda #$ff
	sta to_line
	sta to_line+1
	bne lis_33
lis_10:
	jsr getrange
	bcc lis_20
	rts 
lis_20:
	lda to
	sta to_line
	lda to+1
	sta to_line+1
	inc line_no
	bne lis_31
	inc line_no+1
lis_31:
	jsr dec_from
	jsr frm_addr
lis_33:	lda  ed_com
	cmp #'R'	; replace?
	beq lis_33a	; yes
	cmp #'F'	; find ?
	beq lis_33a	; yes
	jmp lis_55
lis_33a:
	ldy #0
	sty qt_size
	sty num_lins
	sty num_lins+1
	sty trn_flag
	sty glb_flag
	sty q_flag
lis_34:	lda  inbuf,y
	cmp delimit
	beq lis_35	; found opener
	iny 
	cpy in_lgth
	bne lis_34
lis_34a:	lda  #<fnd_err1
	ldx #>fnd_err1
	jmp getr_31
;
lis_35:
	iny 
	sty fnd_from
lis_36:	cpy  in_lgth
	beq lis_34a
	lda inbuf,y
	cmp delimit
	beq lis_37	; found closer
	iny 
	bne lis_36
;
lis_37:
	sty fnd_to
	sec 
	lda fnd_to
	sbc fnd_from
	sta rep_size
	sta fnd_len
;
	lda ed_com
	cmp #'R'	; replace?
	bne lis_45	; nope
	iny 
	sty rep_from	; start of new string
lis_38:
	cpy in_lgth
	beq lis_34a	; no delimiter
	lda inbuf,y
	cmp delimit
	beq lis_39	; end
;
; now see if new string contains spaces or dle's -
; if so we need to flag a tidy for later (to amalgamate
; multiple spaces/dle's etc.
;
	cmp #$10	; dle?
	beq lis_38a	; yes
	cmp #32	; space?
	bne lis_38b	; no - forget it
lis_38a:	inc  qt_size	; flag it
lis_38b:
	iny 
	bne lis_38
;
lis_39:
	sty rep_to
	sec 
	lda fnd_to
	sbc fnd_from
	sta fnd_len
	sec 
	lda rep_to
	sbc rep_from
	sta rep_size
	sbc fnd_len
	sta rep_len	; difference in string lengths
;
;
; now look for flag
;
lis_45:	iny
lis_46:	cpy  in_lgth	; more on line?
	beq lis_50	; no
	lda inbuf,y
	cmp #32
	beq lis_45	; ignore spaces
	and #$7f	; make sure in lower (upper?) case ...
lis_47:	cmp  #'G'	; global?
	bne lis_48	; no
	sta glb_flag	; flag it
	lda fnd_len
	beq lis_err
	bne lis_45
;
;
lis_48:	cmp  #'T'	; translate?
	bne lis_49	; no -
	sta trn_flag
	beq lis_45
lis_49:	cmp  #'Q'	;quiet?
	bne lis_err
	sta q_flag
	beq lis_45
lis_err:	jmp  lis_34a	; none - error

;
lis_50:	lda  rep_len
	sta in_lgth
	dec in_lgth
lis_55:	jsr  display
	clc 
	rts 
;
display:
; print line
	ldy #0
dis_5:
;
	jsr stop
	beq dis_rts
dis_5_ok:
	lda (from),y
	beq dis_wrap
	lda line_no+1
	cmp to_line+1
	bcc dis_15
	lda line_no
	cmp to_line
	bcc dis_15
dis_wrap:
	lda ed_com
	cmp #'R'
	beq dis_wr1
	cmp #'F'
	bne dis_rts
dis_wr1:	jmp  fnd_end
dis_rts:
	rts 
;
;
dis_15:
	lda ed_com
	cmp #'R'
	beq dis_0	; replace
	cmp #'F'
	beq dis_0	; yes
	jmp dis_4	; not find
dis_0:
	ldy #0
	sty fnd_flg	; nothing found yet on this line
dis_0a:	sty  fnd_pos
dis_1:		; here for each char
	ldy fnd_pos
	ldx fnd_from
dis_2:		; inner loop
	cpx fnd_to
	beq dis_35	; end of string - found it !!!
	lda (from),y	; char from file
	cmp #cr	; end of line?
	bne dis_2a	; no
	lda ed_com
	cmp #'F'
	beq dis_2c
	cmp #'R'
	bne dis_16j
dis_2c:	lda  fnd_flg
	beq dis_16j	; nothing replaced on this line
	jmp dis_80	; display it
dis_16j:
	jmp dis_16	; yes - don't display
dis_2a:
;
; translate to upper case if requested
;
	bit trn_flag
	bvc dis_2ab	; nope
	cmp #$c1
	bcc dis_2ab
	cmp #$db
	bcs dis_2ab
	and #$7f
dis_2ab:
	cmp inbuf,x
	bne dis_3	; no match
	iny 
	inx 
	bne dis_2	; more in string
dis_3:	inc  fnd_pos
	cmp #$10	; dle?
	bne dis_1	; no - ok to process next char
	inc fnd_pos	; bypass space count
; confused with const )
	bne dis_1	; process next char in line
;
dis_35:		; count found
	inc num_lins
	bne dis_35a
	inc num_lins+1
dis_35a:	lda  ed_com
	sta fnd_flg	; mark found
	cmp #'R'	; replace?
	bne dis_70	; no
	lda rep_len	; new string same length?
	bne dis_40	; nope - hard one
dis_36a:	ldy  fnd_pos
	ldx rep_from	; move from here
	lda #0
	sta val_cmp	; compile no longer valid
dis_36:
	cpx rep_to	; end of new string?
	beq dis_70	; yes
	lda inbuf,x
	sta (from),y
	iny 
	inx 
	bne dis_36	; back for more
;
dis_70:
	lda glb_flag	; do all on line?
	beq dis_80	; no - finished then
	lda fnd_pos	; point to end of new string
	clc 
	adc rep_size
	tay 
	jmp dis_0a	; back for another go
;
;
dis_40:		; new string different length
	bmi dis_60	; new smaller than old
;
; here if new bigger than old
;
	clc 
	lda from	; calc_ address of new text
	pha 
	adc fnd_pos
	sta pntr
	lda from+1
	pha 
	adc #0
	sta pntr+1
	jsr inst_txt	; make room
dis_45:
	pla 
	sta from+1
	pla 
	sta from
	jmp dis_36a	; now move in new string
;
dis_60:		; new smaller than old
	lda from
	pha 
	clc 
	lda fnd_pos
	adc fnd_len
	adc from
	sta to
	sta epntr	; (for find length)
	lda from+1
	pha 
	adc #0
	sta to+1
	sta epntr+1
	jsr find_end	; find length of file
	clc 
	lda to
	adc rep_len	; (which is negative)
	sta from
	lda to+1
	adc #$ff
	sta from+1
	jsr mv_txt_l	; shift text left
	jmp dis_45	; now to move in new text
;
;
dis_80:
	lda q_flag
	beq dis_4
	jmp dis_16	; no display - 'quiet' mode
;
;
dis_4:
	jsr pt_ln_no
	lda #$40	; expand reserved words but not others (not upper case)
	sta running
	lda from
	ldx from+1
	jsr pl
	lda #$80
	sta running	; back to normal
	jmp dis_17
dis_16:	inc  line_no
	bne dis_17
	inc line_no+1
;
; get posn next line
dis_17:	ldy  #0
dis_20:
	lda (from),y
	jsr inc_from
	cmp #cr
	bne dis_20
	jmp display
;
;
fnd_end:
	jsr crout
	jsr put_no
	lda ed_com
	cmp #'F'
	beq fnd_end2
	lda #<rplcd
	ldx #>rplcd
	jsr pl
	lda num_lins
	ora num_lins+1
	beq fnd_rts	; no need if nothing done
	lda qt_size	; new string contain spaces?
	beq fnd_rts	; no - no tidy needed
	jsr tidy	; clean up what we've done
fnd_rts:	rts
;
fnd_end2:
	lda #<fnd
	ldx #>fnd
	jmp pl
;
rplcd:	.byte " REPLACED",$0d
fnd:	.byte " FOUND",$0d

;
pt_ln_no:
	inc line_no
	bne pt_ln_10
	inc line_no+1
pt_ln_10:
	lda nln_flag	; line numbers?
	bne dis_4a	; nope
put_line:
	lda line_no	; entry here from compiler
	sta reg
	ldx line_no+1
	stx reg+1
	ldy #0
	sty regb
	cpx #>1000
	bcc lt_1000
	bne pt_go
	cmp #<1000
	bcs pt_go
lt_1000:	iny
	cpx #0
	bne pt_go
	cmp #100
	bcs pt_go
	iny 
	cmp #10
	bcs pt_go
	iny 
pt_go:	tya
	beq pt_fin
	pha 
	jsr putsp
	pla 
	tay 
	dey 
	bne pt_go
pt_fin:
	jsr dsp_bin
	jmp putsp

;
getrange:
;***********************************************
; get 1st no
	ldy #0
	jsr get_no
	bcc getr_5
dis_4a:	rts
getr_5:
	lda value
	sta from
	bne getr_10
	lda value+1
	beq getr_30
getr_10:
	lda value+1
	sta from+1
; get 2nd no
	ldy pntr
getr_15:
	iny 
	cpy in_lgth
	beq getr_16
	lda inbuf,y
	cmp delimit
	bne getr_20
getr_16:	lda  from
	sta to
	lda from+1
	sta to+1
	clc 
	rts 
getr_20:
	lda inbuf,y
	cmp #'0'
	bcc getr_22
	cmp #'9'+1
	bcs getr_22
	jmp getr_15
getr_22:
	jsr get_no
	bcc getr_25
	rts 
getr_25:
	lda value
	sta to
	lda value+1
	sta to+1
; check range
	lda to
	sec 
	sbc from
	lda to+1
	sbc from+1
	bmi getr_30
	clc 
	rts 
getr_30:
	lda #<del_err1
	ldx #>del_err1
getr_31:	jsr  pl
	sec 
	rts 
;
to_addr:
	lda to
	sta value
	lda to+1
	sta value+1
	jsr find_ln
	lda pntr
	sta to
	lda pntr+1
	sta to+1
	rts 

;
frm_addr:
	lda from
	sta value
	lda from+1
	sta value+1
	jsr find_ln
	lda pntr
	sta from
	lda pntr+1
	sta from+1
	rts 

;
; tidy up source file
;
; here to convert alphas to reserved words if possible
;
tidy_al:
	ldx epntr	; in quotes?
	bne tidy_ch	; yes - ignore
	ldx from
	stx nxtchr	; start of word
	ldx from+1
	stx nxtchr+1
	jsr token1
	dec tknlen
	ldy #0
	cmp #'I'	; identifier?
	bne tidy_rs	; no - must be reserved
;
; get original letter back
;
	ldx tknlen
	stx qt_size	; ignore next (n - 1) letters
	lda (from),y
	bne tidy_ch	; just copy it
;
tidy_rs:		; reserved word
	lda tknlen
	clc 
	adc from
	sta from	; add length of (token - 1)
	bcc tidy_rs1
	inc from+1
tidy_rs1:		; bypass whole word in source
	iny 
	lda (from),y	; followed by space?
	and #$7f
	cmp #32
	bne tidy_rs2	; no
	jsr inc_from	; yes - ignore space
tidy_rs2:
	lda token
	dey 	; y is now zero again
	beq tidy_ch	; use our new token
;
;
tidy:
	lda ts
	ldx ts+1
;
tidy_ent:
	sta from
	sta to
	sta reg2
	stx from+1
	stx to+1
	stx reg2+1
	lda #0
	sta qt_size	; alpha bypass size
	sta epntr	; quote flag
;
tidy_nxt:
	ldy #0
	lda (from),y
	beq tidy_end
	ldx qt_size	; bypassing a non-reserved word?
	beq tidy_nx1	; no
	dex 	; one less to worry about
	stx qt_size
	bpl tidy_ch	; still in middle - just copy character
tidy_nx1:
	cmp #13	; carriage return
	beq tidy_cr
	cmp quot_sym
	beq tidy_qt
	cmp #32	; space?
	beq tidy_spj	; yes
	cmp #160	; shift/space
	beq tidy_spj
	cmp #$10	; dle?
	beq tidy_dle
	jsr isital	; alpha?
	bcc tidy_al	; yes
;
; copy character
;
tidy_ch:	sta  (to),y
	jsr inc_to
tidy_lf:	jsr  inc_from
	jmp tidy_nxt

tidy_spj:	jmp  tidy_sp

tidy_qt:
	pha 
	eor epntr	; flip flag
	sta epntr
	pla 
	bne tidy_ch	; store it
;
tidy_cr:
	sty epntr	; clear quote flag
	sty qt_size	; and alpha bypass count
	jsr cmp_end	; at start of file?
	beq tidy_nsp	; yes - off we go

;
; drop trailing spaces
;
tidy_ok:
	jsr dec_to
	and #$7f
	cmp #32
	beq tidy_cr
;
	jsr inc_to	; back to last space
tidy_nsp:		; end of file/spaces
	lda #$0d
	bne tidy_ch
;
tidy_end:
	jsr cmp_end
	beq tidy_e2	; at start of file
	jsr dec_to	; c/r before end?
	jsr inc_to
	cmp #$0d
	beq tidy_e2	; yes
	lda #$0d	; no - put one there
	sta (to),y
	jsr inc_to
tidy_e2:
	tya 
	sta (to),y
	rts 
;
;
tidy_dle:
	sta (to),y	; copy 2 chars regardless
	jsr inc_to
	jsr inc_from
	lda (from),y
	tax 	; save temporarily
	iny 
	lda (from),y	; another dle?
	cmp #$10
	beq tidy_dl2	; yes
	and #$7f
	cmp #32	; followed by a solitary space?
	bne tidy_dl3	; no
	inx 	; yes - add 1 to space count
	jsr inc_from	; and bypass the space
tidy_dl3:	txa	; back to space count
	ldy #0
	jmp tidy_ch
;
; here for 2 dle's in a row
;
tidy_dl2:
	iny 
	lda (from),y	; second space count
	and #$7f	; strip 8 bit
	ldy #0
	clc 
	adc (from),y	; add to first space count
	tax 	; save in x
	jsr inc_from	; now bypass 2nd. dle and its count
	jsr inc_from
	bne tidy_dl3	; now copy over our new count
;
;
tidy_sp:
	lda epntr
	bne tidy_gqt	; got quote
	iny 
	lda (from),y	; 2 spaces in row?
	cmp #$10	; space followed by dle?
	beq tidy_bmp	; yes - add 1 to dle count
	and #$7f
	cmp #32
	beq tidy_spc	; yes
	dey 	; no
tidy_gqt:
	lda #32
	jmp tidy_ch
;
; here for a single space followed by a dle and a space count
; - just add 1 to the space count following and ignore the
; current space
;
tidy_bmp:
	tya 	; y contains 1
	clc 
	iny 	; now at space count
	adc (from),y
	sta (from),y
	jmp tidy_lf
;
;
tidy_spc:		; output dle
	dey 
	lda #$10
	sta (to),y
	jsr inc_to
	ldx #1	; space count
tidy_cnt:
	jsr inc_from
	lda (from),y
	and #$7f
	cmp #32	; another?
	bne tidy_dne	; nope
	inx 
	bne tidy_cnt
;
tidy_dne:
	cmp #$0d	; end of line?
	beq tidy_ign	; yes - ignore them
	cmp #$0a	; same
	beq tidy_ign
	txa 	; space count
	ora #$80	; set bit to stop confusion with c/r
	sta (to),y
	jsr inc_to
	jmp tidy_nxt	; process current char
;
tidy_ign:
	jsr dec_to	; forget dle
	jmp tidy_nxt	; process char
;
;
inc_from:
	inc from
	bne rts1
	inc from+1
rts1:	rts
;
inc_to:
	inc to
	bne rts1
	inc to+1
	rts 
;
dec_to:
	dec to
	lda to
	cmp #$ff
	bne dec_to_9
	dec to+1
dec_to_9:
	lda (to),y	; save trouble later
	rts 
;
dec_from:
	dec from
	lda from
	cmp #$ff
	bne rts1
	dec from+1
	rts 
;
cmp_end:
	lda to
	cmp reg2
	bne cmp_end9
	lda to+1
	cmp reg2+1
cmp_end9:	rts
;
;
wrap:	lda  inbuf+1
	cmp #'F'
	bne get7_a
	jmp st_fil	; qf = go to files menu
get7_a:		; ordinary quit
	jmp restart

;
getlnz:
getln:
getln1:
	ldy #0
	ldx dfltn	; input from keyboard (screen)?
	bne get1	; no - bypass fancy stuff (screws up disk)
;
; the code below ensures that we accept data from the
; column that the prompt ended, even if we change lines
;
	jsr chrin	; trigger read of line
	lda lxsp+1	; old column
	sta ch	; make new column
	cmp indx	; past logical end of line?
	bcc get4	; no - is ok
	sty crsw	; indicate line finished
	lda #$0d	; dummy up a c/r
	bne get_3
get4:	lda  #1	; make sure we keep reading line
	sta crsw
get1:	jsr  chrin
	sta inbuf,y	; save in buffer
	iny 
	and #$7f	; c/r may have 8-bit set
	cmp #$0d	; end of line?
	bne get1	; nope
	dey 
get_3:	sta  inbuf,y	; proper c/r
	iny 
	lda #0
	sta inbuf,y	; zero after line for tidy processing
	dey 
	tya 
	pha 
	jsr crout
	pla 
	tax 	; put into x
	rts 	; return

;
	brk 

	.endscope 

;***********************************************
; pascal compiler
; for commodore 64
; part 6
; authors_ nick gammon & sue gobbett
;  sym $9000
;***********************************************

	.scope 

;***********************************************
; part 1 vectors
;***********************************************

	v1 = p1
	init = v1
	getnext = v1+3
	comstl = v1+6
	isithx = v1+9
	isital = v1+12
	isitnm = v1+15
	char = v1+18
	gen2_b = v1+21
	dishx = v1+24
	error = v1+27
	getchk = v1+30
	chktkn = v1+33
	gennop = v1+36
	genadr = v1+39
	gennjp = v1+42
	gennjm = v1+45
	tknwrk = v1+48
	prbyte = v1+51
	gtoken = v1+54
	spare2 = v1+57
	fixad = v1+60
	pshwrk = v1+63
	pulwrk = v1+66
	pc = v1+69
	pt = v1+72
	pl = v1+75
	pc8 = v1+78
	getans = v1+81
	putsp = v1+84
	dispad = v1+87
	crout = v1+90
	shlval = v1+93
	get_num = v1+96
	get_hex = v1+99
	fnd_end = v1+102
	pause = v1+105
	home = v1+108
	rdkey = v1+111
	genjmp = v1+114
	genrjmp = v1+117

	.res 6
;.org $bcb8 ; p6

	jmp block

;***********************************************
; part 2 vectors
;***********************************************

	v2 = p2
	stmnt = v2
	expres = v2+3
	chklhp = v2+6
	chkrhp = v2+9
	chklhb = v2+12
	chkrhb = v2+15
	lookup = v2+18
	chkdup = v2+21
	condec = v2+24
	vardec = v2+27
	const = v2+30
	getsub = v2+33
	w_string = v2+36
	wrktkn = v2+39
	symwrk = v2+42
	wrksym = v2+45
	pshpcode = v2+48
	chk_stak = v2+51
	search = v2+54
	addsym = v2+57
	tknjmp = v2+60

;***********************************************
; part 3 vectors
;***********************************************

	chk_val = p3+403

;***********************************************

chkget:
	jsr chktkn
	jmp gtoken
;
wrk_val:
	pha 
	lda work
	sta value
	lda work+1
	sta value+1
	pla 
	rts 
;
val_wrk:
	pha 
	lda value
	sta work
	lda value+1
	sta work+1
	pla 
	rts 
;
end_wrk:
	pha 
	lda endsym
	sta work
	lda endsym+1
	sta work+1
	pla 
	rts 
;
;***********************************************
;
;
; block
;
blckt1:	.byte $82
	.word blkcns
blckt2:	.byte $83
	.word blkvar
blckt3:	.byte $86
	.word blkprc
	.byte $87
	.word blkfnc
	.byte $88
	.word blkbeg
	.byte 0

;
block:	jsr  chk_stak
	lda #0
	sta frame+1
	lda #6
	sta frame
	lda prcitm
	sta work
	ldx prcitm+1
	stx work+1
	ora prcitm+1
	beq blk1

; here check for construct_
;
; procedure abcd;
; $1234;
;
; which is the method of
; referring to external
; procedures
;
	lda token
	cmp #'N'
	bne blk1a
	ldy #0
	lda (tknadr),y
	cmp #'$'
	bne blk1a
	ldy #symdsp
	lda value
	sta (work),y
	iny 
	lda value+1
	sta (work),y
	lda #1	; flag absolute procedure
	ldy #symdat
	sta (work),y
	lda #';'
	ldx #10
	jmp getchk
;
blk1a:	ldy  #symdsp
	lda pcode
	sta (work),y
	iny 
	lda pcode+1
	sta (work),y
	lda #0
	ldy #symdat
	sta (work),y	; flag relative procedure
	jmp blk2
blk1:	lda  pcode
	sta work
	lda pcode+1
	sta work+1
blk2:	jsr  pshwrk
	jsr gennjp
	ldx #<blckt1
	ldy #>blckt1
blk4:	lda  token
	jsr tknjmp
	ldx #25
	jsr error
;
;
; constant
;
blkcns:	jsr  gtoken
blkcn1:	jsr  condec
	lda #';'
	ldx #10
	jsr chkget
	ldx #<blckt2
	ldy #>blckt2
	jsr tknjmp
	jmp blkcn1
;
; variable
;
blkvar:	lda  #0
	sta count1
blkvr1:	jsr  gtoken
blkvr6:	jsr  vardec
	inc count1
	bpl blkvr7
	jmp blkv13	; error
blkvr7:	lda  token
	cmp #','
	beq blkvr1
	lda #58
	ldx #5
	jsr chkget
	cmp #$84	; array
	beq blkvr2
	cmp #$fe	; integer
	beq blkvr8
	lda #$a1	; char
	ldx #36
	jsr chktkn
	jmp blkvr3
blkvr8:	jsr  blkvr9
blkv10:	ldy  #symprv
	lda (work),y
	tax 
	iny 
	lda (work),y
	sta work+1
	txa 
	sta work	; previous item
	ldy #symdat
	lda #0	; integer type
	sta (work),y
	lda frame
	ldy #symdsp
	sta (work),y
	iny 
	lda frame+1
	sta (work),y
	clc 
	lda frame
	adc #3
	sta frame
	bcc blkv10_a
	inc frame+1
blkv10_a:
	dec count1
	bne blkv10
	jmp blkvr3
;
; array [ n ] of ...
;
blkvr2:	jsr  chklhb
	jsr const
	lda value+2
	bne blkv13
	lda value
	clc 
	adc #1
	sta value
	lda value+1
	bmi blkv13
	adc #0
	bpl blkvr4
blkv13:	ldx  #15
	jsr error
blkvr4:	sta  value+1
	jsr val_wrk
	jsr gtoken
	jsr chkrhb
	lda #1
	sta dattyp
	lda #$85	; of
	ldx #26
	jsr chkget
	cmp #$fe	; integer
	bne blkv11
	dec dattyp
	jsr wrk_val
;
; multiply value by 3
;
	lda value
	ldx value+1
	asl value
	rol value+1
	bcs blkv13
	adc value
	sta value
	txa 
	adc value+1
	bcs blkv13
	sta value+1
	jsr val_wrk
	jmp blkv12
blkv11:	lda  #$a1	; char
	ldx #36
	jsr chktkn
blkv12:	jsr  blkvr9
	jmp blkvr5
blkvr9:
	lda frame
	sec 
	sbc count1
	sta frame
	lda frame+1
	sbc #0
	sta frame+1
	jsr wrk_val
	lda endsym
	sta work
	lda endsym+1
	sta work+1
	rts 
blkvr5:	ldy  #symprv
	lda (work),y
	tax 
	iny 
	lda (work),y
	sta work+1
	txa 
	sta work	; previous item
	ldy #symtyp
	lda #'A'
	sta (work),y
	ldy #symdsp
	lda frame
	sta (work),y
	iny 
	lda frame+1
	sta (work),y
	lda value
	clc 
	adc frame
	sta frame
	lda value+1
	adc frame+1
	sta frame+1
	ldy #symdat
	lda dattyp
	sta (work),y
	ldy #symsub
	lda value
	sta (work),y
	lda value+1
	iny 
	sta (work),y
	dec count1
	bne blkvr5
blkvr3:	lda  #';'
	ldx #10
	jsr getchk
	jsr gtoken
	ldx #<blckt3
	ldy #>blckt3
	jsr tknjmp
	lda #0
	sta count1
	jmp blkvr6
;
; procedure declaration
;
blkprc:	lda  #'I'
	ldx #4
	jsr getchk
	lda #0
	sta count1
	jsr chkdup
	lda #'P'
	jsr addsym
	inc level
	lda symitm
	sta prcitm
	lda symitm+1
	sta prcitm+1
	jmp blkpr1
;
; function declaration
;
blkfnc:	lda  #'I'
	ldx #4
	jsr getchk
	jsr chkdup
	lda #'F'
	jsr addsym
	inc level
	lda #1
	sta count1
	lda symitm
	sta prcitm
	lda symitm+1
	sta prcitm+1
	lda #'Y'
	jsr addsym
;
; procedure and function common code
;
blkpr1:	lda  count1
	sta count2
	jsr end_wrk
	jsr pshwrk
	lda frame
	sta work
	lda frame+1
	sta work+1
	jsr pshwrk
	jsr gtoken
	cmp #'('
	bne blkpr2
blkpr3:	jsr  gtoken
	jsr vardec
	inc count1
	bpl blkpr6
	jmp blkv13
blkpr6:	lda  token
	cmp #','
	beq blkpr3
	jsr chkrhp
blkpr2:	lda  prcitm
	sta work
	lda prcitm+1
	sta work+1
	ldy #symarg
	lda count1
	sec 
	sbc count2
	sta (work),y
	lda #';'
	ldx #10
	jsr chktkn
	lda count1
	beq blkpr4
	jsr end_wrk
	ldx #$fd
blkpr5:	ldy  #symprv
	lda (work),y
	pha 
	iny 
	lda (work),y
	sta work+1
	pla 
	sta work
	ldy #symdat
	lda #0
	sta (work),y
	ldy #symdsp
	txa 
	sta (work),y
	sec 
	sbc #3
	tax 
	lda #$ff
	iny 
	sta (work),y
	dec count1
	bne blkpr5
blkpr4:	jsr  gtoken
	jsr block
	dec level
	jsr pulwrk
	lda work
	sta frame
	lda work+1
	sta frame+1
	jsr pulwrk
	lda work
	sta endsym
	lda work+1
	sta endsym+1
	lda #';'
	ldx #10
	jsr chkget
	ldx #<blckt3
	ldy #>blckt3
	jmp blk4
;
; begin (compound statement)
;
blkbeg:	jsr  gtoken
	jsr pulwrk
	lda level
	bne blkb1
blkb3:	jsr  fixad
	jmp blkb2
blkb1:	jsr  wrksym
	ldy #symdsp
	lda (symitm),y
	sta work
	iny 
	lda (symitm),y
	sta work+1
	ldy #symdsp
	lda pcode
	sta (symitm),y
	lda pcode+1
	iny 
	sta (symitm),y
	jmp blkb3
blkb2:	lda  frame
	sta opnd
	lda frame+1
	sta opnd+1
	lda #59
	jsr genjmp
blkb5:	jsr  stmnt
	lda token
	cmp #';'
	bne blkb4
	jsr gtoken
	jmp blkb5
blkb4:	lda  #$89	; end
	ldx #17
	jsr chkget
	lda #41
	ldx level
	bne blkb6
	lda #17	; stop
test1:
blkb6:	jmp  gennop

;
	brk 

	.endscope 
