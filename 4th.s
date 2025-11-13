; $VER: 4th v1.016 - coded by Peter Bakota (c)1998 copyright Tenax software
;
; !!!!!!!!!!!!!!!!THIS IS NOT A FIG-FORTH IMPLEMENTATION!!!!!!!!!!!!
;
; For much of the code I took inspiration from the following sources:
; eForth 1.0 (8086) by Bill Muench and C. H. Ting, 1990
; FIG-Forth to IBM PC 1.0 by Charlie Krajewski,
;			     FIG p.o. box 1105, San Carlos, Ca. 94070

debug=0		;debug on

; AmigaDOS functions

* Exec
OpenLib		equ	-552
CloseLib	equ	-414
SetSignal	equ	-306

* DOS
Open		equ	-30
Close		equ	-36
Read		equ	-42
Write		equ	-48
WaitForChar	equ	-204

call	macro
	ifnc	"\2",""
	move.l	\2,a6
	endc
	jsr	\1(a6)
	endm

acall	macro
	move.l	a6,-(sp)
	move.l	\2,a6
	jsr	\1(a6)
	move.l	(sp)+,a6
	endm

asm	macro	* lex, name, lab ,link
	dc.w	\3	;ca
	dc.w	\4	;la
z\3:	equ	*-base
	dc.b	\1	;lex
	dc.b	\2	;name
	cnop	0,2
\3:	equ	*-base
\3_:
	endm

user_	set	4
user	macro	* lex, name, lab, link
	colon	\1,\2,\3,\4
	dc.w	douser
	dc.w	user_
user_	set	user_+2
	endm

colon	macro
	asm	\1,\2,\3,\4
	jsr	(a6)
	endm

MAXFILES EQU	10
HUNK_HDR EQU	$03F3
HUNK_CDE EQU	$03E9
HUNK_END EQU	$03F2

EM	EQU	64*1024
RP0	EQU	EM-2
SP0	EQU	RP0-4*1024
TIBB	EQU	SP0-2*1024-256
FST	EQU	TIBB-MAXFILES*4
	
IMEDD	equ	$80
COMPO	equ	$40
vocss	equ	8	;vocabulary stack depth
basee	equ	10	;default radix
crr	equ	13
lf	equ	10
bkspp	equ	8
calll	equ	$4e96	; JSR (A6) - JSR DOLIST op code

	opt	o+,ow-
	section	"4th",code
start:	bra.s	init
; the address 0
; next	jmp (a5) = jmp next
	ifeq	debug
base:	move.w	(a2)+,d7
	jmp	(a5,d7.l)
	elseif
base:	cmp.l	brkpi(pc),a2
	bne.s	1$
	nop
1$	move.w	(a2)+,d7
	jmp	(a5,d7.l)
brkpi:	dc.l	BP
	endc
init:	lea	_systemsp(pc),a2
	move.l	sp,(a2)+
	lea	_dosname(pc),a1
	moveq	#33,d0
	call	OpenLib,4.w
	move.l	d0,(a2)+
	beq.w	bye_
	lea	_condef(pc),a0
	move.l	a0,d1
	move.l	#1006,d2
	call	Open,_dosbase(pc)
	move.l	d0,(a2)+
	beq.w	bye_
	moveq	#0,d7
	lea	base(pc),a5
	move.w	UPP+2*2+0(pc),d7
	lea	(a5,d7.l),a4		;initialize SP
	move.w	UPP+2*2+2(pc),d7
	lea	(a5,d7.l),sp		;initialize RP
	lea	dolist_(pc),a6
	lea	vcold(pc),a2
	jmp	(a5)
ver:	dc.b	"$VER: 4th date: 12/26/98",13,10,0
	cnop	0,4
_dosname:
	dc.b "dos.library",0
_condef:dc.b "RAW:0/1/640/255/4TH CONSOLE",0
	cnop	0,2
_systemsp:
	dc.l	0
_dosbase:
	dc.l	0
_conhand:
	dc.l	0

; Call forth word from assembly language
; a0=forth word ptr
ccode:	move.l	a2,-(sp)
	lea	1$(pc),a2
	jmp	(a0)
1$	dc.w	2$-base
2$	move.l	(sp)+,a2
	rts
	
; xcall ( ca -- )
; Call asm subroutine from forth word. Return with RTS!

	asm	5,"xcall",xcall,0
	move.w	(a4)+,d7
	jsr	(a5,d7.l)
	jmp	(a5)

; BASIC I/O

; OPEN-FILE ( b w -- w T | F )
; Open an AmigaDOS file. Return handler # and true or false if error

	asm	9,"OPEN-FILE",opnfil,zxcall
	move.l	d2,-(sp)
	moveq	#0,d2
	move.w	(a4)+,d2
	move.w	(a4)+,d7
	lea	(a5,d7.l),a0
	move.l	a0,d1
	move.w	fstp(a5),d7
	lea	(a5,d7.l),a0
	move.l	a0,-(sp)
	moveq	#MAXFILES-1,d0
2$	tst.l	(a0)+
	dbeq	d0,2$
	bne.s	1$
	move.l	a0,-(sp)
	acall	Open,_dosbase(pc)
	move.l	(sp)+,a0
	move.l	(sp),d1
	move.l	d0,-(a0)
	beq.s	1$
	sub.l	d1,a0
	move.l	a0,d0
	lsr.l	#2,d0
	move.w	d0,-(a4)
	move.w	#-1,-(a4)
	bra.s	3$
1$	clr.w	-(a4)
3$	addq.w	#4,sp
	move.l	(sp)+,d2
	jmp	(a5)

; CLOSE-FILE ( w -- f )
; Close an AmigaDOS file. On TOS the handler #. Return false if failed.

	asm	10,"CLOSE-FILE",clofil,zopnfil
	move.w	(a4)+,d0
	add.w	d0,d0
	add.w	d0,d0
	move.w	fstp(a5),d7
	lea	(a5,d7.l),a0
	move.l	(a0,d0.w),d1
	beq.s	1$
	clr.l	(a0,d0.w)
	acall	Close,_dosbase(pc)
	moveq	#-1,d1
1$	move.w	d1,-(a4)
	jmp	(a5)

; WRITE-FILE (w b u -- f )
; Write u bytes from b to file w. Return false if failed.

	asm	10,"WRITE-FILE",wrtfil,zclofil
	movem.l	d2/d3,-(sp)
	moveq	#0,d3
	moveq	#0,d2
	move.w	(a4)+,d3
	move.w	(a4)+,d2
	move.w	(a4)+,d1
	add.w	d1,d1
	add.w	d1,d1
	move.w	fstp(a5),d7
	lea	(a5,d7.l),a0
	move.l	(a0,d1.w),d1
	beq.s	1$
	add.l	a5,d2
	acall	Write,_dosbase(pc)
	moveq	#0,d1
	tst.l	d0
	ble.s	1$
	moveq	#-1,d1
1$	move.w	d1,-(a4)
	movem.l	(sp)+,d2/d3
	jmp	(a5)

; READ-FILE ( w b u -- f )
; Read u bytes from file w to address b. Return false if failed.

	asm	9,"READ-FILE",reafil,zwrtfil
	movem.l	d2/d3,-(sp)
	moveq	#0,d3
	moveq	#0,d2
	move.w	(a4)+,d3
	move.w	(a4)+,d2
	move.w	(a4)+,d1
	add.w	d1,d1
	add.w	d1,d1
	move.w	fstp(a5),d7
	lea	(a5,d7.l),a0
	move.l	(a0,d1.w),d1
	beq.s	1$
	add.l	a5,d2
	acall	Read,_dosbase(pc)
	moveq	#0,d1
	tst.l	d0
	ble.s	1$
	moveq	#-1,d1
1$	move.w	d1,-(a4)
	movem.l	(sp)+,d2/d3
	jmp	(a5)

; closefiles ( -- )
; Close all files.

	asm	10+IMEDD,"closefiles",clofils,zreafil
	movem.l	d2/a2,-(sp)
	move.w	fstp(a5),d7
	lea	(a5,d7.l),a2
	moveq	#MAXFILES-1,d2	
1$	move.l	(a2)+,d1
	beq.s	2$
	clr.l	-4(a2)	; clear handler
	acall	Close,_dosbase(pc)
2$	dbf	d2,1$
	movem.l	(sp)+,d2/a2
	jmp	(a5)

; BYE ( -- )
; Return to CLI.

	asm	3,"BYE",bye,zclofils
	lea	_conhand+4(pc),a2
	move.l	-(a2),d1
	beq.s	1$
	call	Close,_dosbase(pc)
1$	move.l	-(a2),d1
	beq.s	2$
	move.l	d1,-(sp)
	lea	clofils_(pc),a0
	bsr.w	ccode
	move.l	(sp)+,a1
	call	CloseLib,4.w
2$	move.l	-(a2),sp
	moveq	#0,d0
	ifne	debug
	move.w	(a4)+,d0
	endc
	rts

; ?RX ( -- c T | F )
; Return input character and true, or flase if no input.

	asm	3,"?RX",qrx,zbye
	movem.l	d2/d3,-(sp)
	move.l	_conhand(pc),d1
	moveq	#1,d2
	acall	WaitForChar,_dosbase(pc)
	tst.l	d0
	bne.s	2$
	clr.w	-(a4)
	bra.s	1$
2$	clr.w	-(a4)
	move.l	a4,d2
	addq.l	#1,d2
	moveq	#1,d3
	move.l	_conhand(pc),d1
	acall	Read,_dosbase(pc)
	tst.l	d0
	ble.s	1$
	cmp.b	#$1c,1(a4)		;ctrl+\ exit!
	beq.s	bye_
	move.w	#-1,-(a4)
1$	movem.l	(sp)+,d2/d3
	jmp	(a5)

; TX! ( c -- )
; Send character c to the output device.

	asm	3,"TX!",txsto,zqrx
	movem.l	d2/d3,-(sp)
	move.l	a4,d2
	addq.l	#1,d2
	moveq	#1,d3
	move.l	_conhand(pc),d1
	acall	Write,_dosbase(pc)
	movem.l	(sp)+,d2/d3
	addq.w	#2,a4
	jmp	(a5)

; dolit ( -- w )
; Push an inline literal.

	asm	5+COMPO,"dolit",dolit,ztxsto
	move.w	(a2)+,-(a4)
	jmp	(a5)

; dolist ( a -- )
; Process colon list.

	asm	6+COMPO,"dolist",dolist,zdolit
	move.l	(sp)+,d0
	sub.l	a5,a2
	move.w	a2,-(sp)
	move.l	d0,a2
	jmp	(a5)

; next ( -- )
; Run time asm for the single index loop

	asm	4+COMPO,"next",donxt,zdolist
	subq.w	#1,(sp)
	bpl.s	xbra
	addq.w	#2,a2
	addq.w	#2,sp
	jmp	(a5)

; ?branch ( f -- )
; Branch if flag is zero.

	asm	7+COMPO,"?branch",qbran,zdonxt
	tst.w	(a4)+
	beq.s	xbra
	addq.w	#2,a2
	jmp	(a5)

; branch ( -- )
; Branch to an inline address.

	asm	6+COMPO,"branch",bran,zqbran
xbra	move.w	(a2),d7
	lea	(a5,d7.l),a2
	jmp	(a5)

; EXECUTE ( ca -- )
; Execute word at ca.

	asm	7,"EXECUTE",execu,zbran
	move.w	(a4)+,d7
	jmp	(a5,d7.l)

; EXIT ( -- )
; Terminate a colon definition.

	asm	4,"EXIT",exit,zexecu
	move.w	(sp)+,d7
	lea	(a5,d7.l),a2
	jmp	(a5)

; ! ( w a -- )
; Pop the data stack to memory.

	asm	1,"!",store,zexit
	move.w	(a4)+,d7
	move.w	(a4)+,(a5,d7.l)
	jmp	(a5)

; @ ( a -- w )
; Push memory location to the data stack.

	asm	1,"@",at,zstore
	move.w	(a4)+,d7
	move.w	(a5,d7.l),-(a4)
	jmp	(a5)

; C! ( c b -- )
; Pop the data stack to byte memory.

	asm	2,"C!",cstor,zat
	move.w	(a4)+,d7
	move.w	(a4)+,d0
	move.b	d0,(a5,d7.l)
	jmp	(a5)

; C@ ( b -- c )
; Push byte memory to the data stack.

	asm	2,"C@",cat,zcstor
	move.w	(a4)+,d7
	moveq	#0,d0
	move.b	(a5,d7.l),d0
	move.w	d0,-(a4)
	jmp	(a5)

; RP@ ( -- a )
; Push the current RP to the data stack.

	asm	3,"RP@",rpat,zcat
	move.l	a7,d0
	sub.l	a5,d0
	move.w	d0,-(a4)
	jmp	(a5)

; RP! ( a -- )
; Set the RP.

	asm	3+COMPO,"RP!",rpsto,zrpat
	move.w	(a4)+,d7
	lea	(a5,d7.l),a7
	jmp	(a5)

; R> ( -- w )
; Pop the return stack to the data stack.

	asm	2,"R>",rfrom,zrpsto
	move.w	(sp)+,-(a4)
	jmp	(a5)

; R@ ( -- w )
; Copy top of return stack to the data stack.

	asm	2,"R@",rat,zrfrom
	move.w	(sp),-(a4)
	jmp	(a5)

; >R ( w -- )
; Push the data stack to return stack.

	asm	2+COMPO,">R",tor,zrat
	move.w	(a4)+,-(sp)
	jmp	(a5)

; SP@ ( -- a )
; Push the current data stack pointer.

	asm	3,"SP@",spat,ztor
	move.l	a4,d0
	sub.l	a5,d0
	move.w	d0,-(a4)
	jmp	(a5)

; SP! ( a -- )
; Set SP.

	asm	3,"SP!",spsto,zspat
	move.w	(a4)+,d7
	lea	(a5,d7.l),a4
	jmp	(a5)

; DROP ( w -- )
; Discard tos.

	asm	4,"DROP",drop,zspsto
	addq.w	#2,a4
	jmp	(a5)

; DUP ( w -- w w )
; Duplicate the tos.

	asm	3,"DUP",dupp,zdrop
	move.w	(a4),-(a4)
	jmp	(a5)

; SWAP ( w1 w2 -- w2 w1 )
; Swap two top stack items.

	asm	4,"SWAP",swapp,zdupp
	move.l	(a4),d0
	swap	d0
	move.l	d0,(a4)
	jmp	(a5)

; OVER ( w1 w2 -- w1 w2 w1 )
; Copy second stack item to top.

	asm	4,"OVER",over,zswapp
	move.w	2(a4),-(a4)
	jmp	(a5)

; 0< ( n -- t )
; Return true if n is negative.

	asm	2,"0<",zless,zover
	tst.w	(a4)+
	smi	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; AND ( w w -- w )
; Bitwise and.

	asm	3,"AND",andd,zzless
	move.w	(a4)+,d0
	and.w	d0,(a4)
	jmp	(a5)

; OR ( w w -- w )
; Bitwise or.

	asm	2,"OR",orr,zandd
	move.w	(a4)+,d0
	or.w	d0,(a4)
	jmp	(a5)

; XOR ( w w -- w )
; Bitwise xor.

	asm	3,"XOR",xorr,zorr
	move.w	(a4)+,d0
	eor.w	d0,(a4)
	jmp	(a5)

; UM+ ( u u -- udsum )
; Add two unsigned numbers and return double sum.

	asm	3,"UM+",uplus,zxorr
	move.w	(a4)+,d1
	add.w	(a4)+,d1
	scs	d0
	and.w	#1,d0
	movem.w	d0/d1,-(a4)
	jmp	(a5)

; dovar ( -- a )
; Run time routine for VARIABLE and CREATE.

	colon	5+COMPO,"dovar",dovar,zuplus
	dc.w	rfrom
	dc.w	exit

; UP ( -- a )
; Pointer to the user area.

	colon	2,"UP",up,zdovar
	dc.w	dovar
	dc.w	UPP-base

; douser ( -- a )
; Run time routine for user variables.

	colon	6+COMPO,"douser",douser,zup
	dc.w	rfrom,at,up,at,plus,exit

; SP0 ( -- a )
; Pointer to bottom of the data stack.

	user	3,"SP0",szero,zdouser

; RP0 ( -- a )
; Pointer to bottom of the return stack.

	user	3,"RP0",rzero,zszero

; '?KEY ( -- a )
; Execution vector of ?KEY.

	user	5,"'?KEY",tqkey,zrzero

; 'EMIT ( -- a )
; Execution vector of EMIT.

	user	5,"'EMIT",temit,ztqkey

; 'EXPECT ( -- a )
; Execution vector of EXPECT.

	user	7,"'EXPECT",texpe,ztemit

; 'TAP ( -- a )
; Execution vector of TAP.

	user	4,"'TAP",ttap,ztexpe

; 'ECHO ( -- a )
; Execution vector of ECHO.

	user	5,"'ECHO",techo,zttap

; 'PROMPT ( -- a )
; Execution vector of PROMPT.

	user	7,"'PROMPT",tprom,ztecho

; BASE ( -- a )
; Storage of the radix base for numeric I/O.

	user	4,"BASE",nbase,ztprom

; tmp ( -- a )
; A temporary storage location used in parse and find.

	user	3+COMPO,"tmp",temp,znbase

; SPAN ( -- a )
; Hold character count recived by EXPECT.

	user	4,"SPAN",span,ztemp

; >INN ( -- a )
; Hold the character pointer while parsing input stream.

	user	3,">IN",inn,zspan

; #TIB ( -- a )
; Hold the current count in and address of the terminal input buffer.

	user	4,"#TIB",ntib,zinn
user_	set	user_+2		; terminal input buffer pointer

; CSP ( -- a )
; Hold the stack pointer for error checking.

	user	3,"CSP",csp,zntib

; 'EVAL ( -- a )
; Execution vector of EVAL.

	user	5,"'EVAL",teval,zcsp

; 'NUMBER ( -- a )
; Execution vector of NUMBER?.

	user	7,"'NUMBER",tnumb,zteval

; HLD ( -- a )
; Hold a pointer in building a numeric output string.

	user	3,"HLD",hld,ztnumb

; HANDLER ( -- a )
; Hold the return stack pointer for error handling.

	user	7,"HANDLER",handl,zhld

; CONTEXT ( -- a )
; A area to specify vocabulary search order.

	user	7,"CONTEXT",cntxt,zhandl
user_	set	user_+vocss*2

; CURRENT ( -- a )
; Point to the vocabulary to be extended.

	user	7,"CURRENT",crrnt,zcntxt
user_	set	user_+2

; CP ( -- a )
; Point to the top of the asm dictionary.

	user	2,"CP",cp,zcrrnt

; NP ( -- a )
; Point to the top of the name dictionary.

	user	2,"NP",np,zcp

; LAST ( -- a )
; Point to the last name in the name dictionary.

	user	4,"LAST",last,znp

; fst ( -- a )
; Point to the file tables

	user	4,"fst",fstt,zlast

; dovoc ( -- )
; Run time action of VOCABULARY's.

	colon	5+COMPO,"dovoc",dovoc,zfstt
	dc.w	rfrom,cntxt,store,exit

; FORTH ( -- )
; Make FORTH the context vocabulary.

	colon	5,"FORTH",forth,zdovoc
	dc.w	dovoc
	dc.w	0		;HEADER
	dc.w	0		;LINK

; ?DUP ( w -- w w | 0 )
; Dup tos if it's not zero.

	asm	4,"?DUP",qdup,zforth
	tst.w	(a4)
	beq.s	1$
	move.w	(a4),-(a4)
1$	jmp	(a5)

; ROT ( w1 w2 w3 -- w2 w3 w1 )
; Rot 3rd item to top.

	asm	3,"ROT",rot,zqdup
	movem.w	(a4)+,d0/d1	;d0=w3, d1=w2
	swap	d0
	move.w	(a4)+,d0	;d0=w3w1
	swap	d0
	move.w	d1,-(a4)	;d2
	move.l	d0,-(a4)	;w1w3
	jmp	(a5)

; 2DROP	( w w -- )
; Discard two items on stack.

	asm	5,"2DROP",ddrop,zrot
	addq.w	#2+2,a4
	jmp	(a5)

; 2DUP ( w1 w2 -- w1 w2 w1 w2 )
; Duplicate top two items.

	asm	4,"2DUP",ddup,zddrop
	move.l	(a4),-(a4)
	jmp	(a5)

; + ( n1 n2 -- n1+n2 )
; Add top two items.

	asm	1,"+",plus,zddup
	move.w	(a4)+,d0
	add.w	d0,(a4)
	jmp	(a5)

; NOT ( w -- w )
; One's complement of tos.

	asm	3,"NOT",inver,zplus
	not.w	(a4)
	jmp	(a5)

; NEGATE ( n -- -n )
; Two's complement of tos.

	asm	6,"NEGATE",negat,zinver
	neg.w	(a4)
	jmp	(a5)

; DNEGATE ( d -- -d )
; Two's complement of top double.

	asm	7,"DNEGATE",dnega,znegat
	neg.l	(a4)
	jmp	(a5)

; - ( n1 n2 -- n1-n2 )
; Subtraction.

	asm	1,"-",subb,zdnega
	move.w	(a4)+,d0
	sub.w	d0,(a4)
	jmp	(a5)

; 1+ ( n -- n+1 )
; Increment top stack item

	asm	2,"1+",incc,zsubb
	addq.w	#1,(a4)
	jmp	(a5)

; 1- ( n -- n-1 )
; Decrement top stack item

	asm	2,"1-",decc,zincc
	subq.w	#1,(a4)
	jmp	(a5)

; ABS ( n -- n )
; Return the aboslute value of n.

	asm	3,"ABS",abss,zdecc
	tst.w	(a4)
	bpl.s	1$
	neg.w	(a4)
1$	jmp	(a5)

; = ( w w -- t )
; Return true if top two are equal.

	asm	1,"=",equal,zabss
	cmp.w	(a4)+,(a4)+
	seq	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; <> ( w w -- t )
; Return true if top two are not equal

	asm	2,"<>",nequal,zequal
	cmp.w	(a4)+,(a4)+
	sne	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; U< ( u u -- t )
; Unsigned compare of top two items.

	asm	2,"U<",uless,znequal
	cmp.w	(a4)+,(a4)+
	slo	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; < ( u u -- t )
; Signed compare of top two items.

	asm	1,"<",nless,zuless
	cmp.w	(a4)+,(a4)+
	slt	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; 0<> ( u -- t )
; Return true if tos <> 0

	asm	3,"0<>",znequ,znless
	tst.w	(a4)+
	sne	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; MAX ( n n -- n )
; Return the greater of top stack items.

	asm	3,"MAX",max,zznequ
	move.w	(a4)+,d0
	cmp.w	(a4),d0
	ble.s	1$
	move.w	d0,(a4)
1$	jmp	(a5)


; MIN ( n n -- n )
; Return the smaller of top stack items.

	asm	3,"MIN",min,zmax
	move.w	(a4)+,d0
	cmp.w	(a4),d0
	bge.s	1$
	move.w	d0,(a4)
1$	jmp	(a5)

; WITHIN ( u ul uh -- t )
; Return true if u is within the range of ul and uh. ( ul <= u < uh )

	asm	6,"WITHIN",withi,zmin
	movem.w	(a4)+,d0/d1
	cmp.w	(a4),d1
	bgt.s	1$
	cmp.w	(a4),d0
	ble.s	1$
	move.w	#-1,(a4)
	jmp	(a5)
1$	clr.w	(a4)
	jmp	(a5)

; UM/MOD ( udl udh un -- ur uq )
; Unsigned divide of a double by a single. Return mod and quotient.

	asm	6,"UM/MOD",ummod,zwithi
	move.w	(a4)+,d0
	move.l	(a4)+,d1
	tst.w	d0
	beq.s	dbyzero
	divu.w	d0,d1
	swap	d1
	move.l	d1,-(a4)
	jmp	(a5)

; NOTE: IN ALL ROUTINES DIVISION BY ZERO RETURN -1 AS LONG!
dbyzero:move.l	#-1,-(a4)	; Division by zero
	jmp	(a5)

; M/MOD ( d n -- r q )
; Signed floored divide of double by single. Return mod and quo.

	asm	5,"M/MOD",msmod,zummod
	move.w	(a4)+,d0
	move.l	(a4)+,d1
	tst.w	d0
	beq.s	dbyzero
	divs.w	d0,d1
	move.l	d1,-(a4)
	jmp	(a5)

; /MOD ( n n -- r q )
; Signed divide return r and q.

	asm	4,"/MOD",slmod,zmsmod
	movem.w	(a4)+,d0/d1
	tst.w	d0
	beq.s	dbyzero
	divs.w	d0,d1
	move.l	d1,-(a4)
	jmp	(a5)

; MOD ( n n -- r )
; Signed divide return	mod only.

	asm	3,"MOD",modd,zslmod
	movem.w	(a4)+,d0/d1
	tst.w	d0
	beq.s	dbyzero
	divs.w	d0,d1
	swap	d1
	move.w	d1,-(a4)
	jmp	(a5)

; / ( n n -- q )
; Signed divide return quotient only.

	asm	1,"/",slash,zmodd
	movem.w	(a4)+,d0/d1
	tst.w	d0
	beq.s	dbyzero
	divs.w	d0,d1
	move.w	d1,-(a4)
	jmp	(a5)

; UM* ( u u -- ud )
; Unsigned multiply. Return double product.

	asm	3,"UM*",umsta,zslash
	move.w	(a4)+,d0
	mulu.w	(a4)+,d0
	move.l	d0,-(a4)
	jmp	(a5)

; * ( u u -- n )
; Signed multiply. Return single product.

	asm	1,"*",star,zumsta
	move.w	(a4)+,d0
	muls.w	(a4)+,d0
	move.w	d0,-(a4)
	jmp	(a5)

; M* ( n n -- d )
; Signed multiply. Return double product.

	asm	2,"M*",mstar,zstar
	move.w	(a4)+,d0
	muls.w	(a4)+,d0
	move.l	d0,-(a4)
	jmp	(a5)

; */MOD ( n1 n2 n3 -- r q )
; Mutiply n1 and n2, then divide by n3. Return mod and quotient.

	asm	5,"*/MOD",ssmod,zmstar
	movem.w	(a4)+,d0/d1
	muls.w	(a4)+,d1
	tst.w	d0
	beq.w	dbyzero
	divs.w	d0,d1
	move.l	d1,-(a4)
	jmp	(a5)

; */ ( n1 n2 n3 -- q )
; Multiply n1 by n2, then divide by n3. Return quotient only.

	asm	2,"*/",stasl,zssmod
	movem.w	(a4)+,d0/d1
	muls.w	(a4)+,d1
	tst.w	d0
	beq.w	dbyzero
	divs.w	d0,d1
	move.w	d1,-(a4)
	jmp	(a5)

; CELL+ ( a -- a )
; Add cell size in bytes to address.

	asm	5,"CELL+",cellp,zstasl
	addq.w	#2,(a4)
	jmp	(a5)

; CELL- ( a -- a )
; Substract cell size in bytes to address.

	asm	5,"CELL-",cellm,zcellp
	subq.w	#2,(a4)
	jmp	(a5)

; CELLS ( n -- n )
; Multiply tos by cell size in bytes.

	asm	5,"CELLS",cells,zcellm
	lsl.w	(a4)		; *2!
	jmp	(a5)

;  ALIGNED ( b -- a )
; Align address to the cell boundary.

	asm	7,"ALIGNED",algnd,zcells
	btst	#0,1(a4)
	beq.s	1$
	addq.w	#1,(a4)
1$	jmp	(a5)

; BL ( -- 32 )
; Return 32 the blank character.

	asm	2,"BL",bl,zalgnd
	move.w	#$20,-(a4)
	jmp	(a5)

; >CHAR ( c -- c )
; Filter non printing characters.

	colon	5,">CHAR",tchar,zbl
	dc.w	dolit,$7f,andd,dupp	; mask msb
	dc.w	bl,dolit,$7f,withi	; check for printable
	dc.w	inver
	dc.w	qbran,1$-base
	dc.w	drop,dolit,'_'		; replace non printable
1$	dc.w	exit

; DEPTH ( -- n )
; Return the depth of the data stack.

	colon	5,"DEPTH",depth,ztchar
	dc.w	szero,at,spat,subb
	dc.w	dolit,2,slash,exit

; PICK ( ... +n -- ... w )
; Copy the nth stack item to tos.

	colon	4,"PICK",pick,zdepth
	dc.w	incc,cells
	dc.w	spat,plus,at,exit

; +! ( n a -- )
; Add n to the contents at address a.

	asm	2,"+!",pstor,zpick
	move.w	(a4)+,d7
	move.w	(a4)+,d0
	add.w	d0,(a5,d7.l)
	jmp	(a5)

; 2! ( d a -- )
; Store the double integer to address a.

	asm	2,"2!",dstor,zpstor
	move.w	(a4)+,d7
	move.l	(a4)+,(a5,d7.l)
	jmp	(a5)

; 2@ ( a -- d )
; Fetch double integer from address a.

	asm	2,"2@",dat,zdstor
	move.w	(a4)+,d7
	move.l	(a5,d7.l),-(a4)
	jmp	(a5)

; COUNT ( b -- b +n )
; Return count byte of a string and add 1 to the byte address.

	asm	5,"COUNT",count,zdat
	move.w	(a4),d7
	addq.w	#1,(a4)
	moveq	#0,d0
	move.b	(a5,d7.l),d0
	move.w	d0,-(a4)
	jmp	(a5)

; HERE ( -- a )
; Return the top of the code dictionary.

	colon	4,"HERE",here,zcount
	dc.w	cp,at,exit

; PAD ( -- a )
; Return the address of the text buffer above the code dictionary.

	colon	3,"PAD",pad,zhere
	dc.w	here,dolit,80,plus,exit

; TIB ( -- a )
; Return the address of terminal input buffer.

	colon	3,"TIB",tib,zpad
	dc.w	ntib,cellp,at,exit

; @EXECUTE ( a -- )
; Execute vector stored in address a.

	asm	8,"@EXECUTE",atexe,ztib
	move.w	(a4)+,d7
	beq.s	1$
	move.w	(a5,d7.l),d7
1$	jmp	(a5,d7.l)


; CMOVE ( b1 b2 u -- )
; Copy u bytes from b1 to b2.

	asm	5,"CMOVE",cmove,zatexe
	moveq	#0,d0
	move.w	(a4)+,d0
	move.w	(a4)+,d7
	lea	(a5,d7.l),a0
	move.w	(a4)+,d7
	lea	(a5,d7.l),a1
2$	subq.l	#1,d0
	bmi.s	1$
	move.b	(a1)+,(a0)+
	bra.s	2$
1$	jmp	(a5)

; FILL ( b u c -- )
; Fill u bytes of character c to area begining at b.

	asm	4,"FILL",fill,zcmove
	moveq	#0,d1
	movem.w	(a4)+,d0
	move.w	(a4)+,d1
	move.w	(a4)+,d7
	lea	(a5,d7.l),a0
2$	subq.l	#1,d1
	bmi.s	1$
	move.b	d0,(a0)+
	bra.s	2$
1$	jmp	(a5)

; -TRAILING ( b u -- b u )
; Adjust the count to eliminate trailing white spaces.

	colon	9,"-TRAILING",dtrai,zfill
	dc.w	tor
	dc.w	bran,1$-base
2$	dc.w	bl,over,rat,plus,cat,nless
	dc.w	qbran,1$-base
	dc.w	rfrom,incc,exit
1$	dc.w	donxt,2$-base
	dc.w	dolit,0,exit

; PACK$ ( b u a -- a )
; Build a counted string with u characters from b.

	colon	5,"PACK$",packs,zdtrai
	; Changed from the original to AMIGA
	dc.w	algnd,over,over,cstor,dupp,tor,incc,swapp,cmove,rfrom,exit

; DIGIT ( u -- c )
; Convert digit u to a character.

	asm	5,"DIGIT",digit,zpacks
	move.w	(a4)+,d0
digit2:	cmp.w	#9,d0		;?0..9
	bls.s	1$
	addq.w	#7,d0		;convert to A..Z
1$	add.w	#$30,d0
	move.w	d0,-(a4)	;c
	jmp	(a5)

; EXTRACT ( n base -- n c )
; Extract the least significant digit from n.

	asm	7,"EXTRACT",extrc,zdigit
	moveq	#0,d0
	move.w	(a4)+,d1
	move.w	(a4)+,d0
	tst.w	d1
	beq.w	dbyzero
	divu.w	d1,d0
	move.w	d0,-(a4)	;n
	swap	d0
	bra.s	digit2
	
; <# ( -- )
; Initiate the numeric output process.

	colon	2,"<#",bdigs,zextrc
	dc.w	pad,hld,store,exit

; HOLD ( c -- )
; Insert a character into the numeric output string.

	colon	4,"HOLD",hold,zbdigs
	dc.w	hld,at,decc
	dc.w	dupp,hld,store,cstor,exit

; # ( u -- u )
; Extract one digit from u and append the digit to output string.

	colon	1,"#",dig,zhold
	dc.w	nbase,at,extrc,hold,exit

; #S ( u -- 0 )
; Convert u until all digits are added to the output string.

	colon	2,"#S",digs,zdig
1$	dc.w	dig,dupp
	dc.w	qbran,2$-base
	dc.w	bran,1$-base
2$	dc.w	exit

; SIGN ( n -- )
; Add a minus sign to the numeric output string.

	colon	4,"SIGN",sign,zdigs
	dc.w	zless
	dc.w	qbran,1$-base
	dc.w	dolit,'-',hold
1$	dc.w	exit

; #> ( w -- b u )
; Prepare the output string to be TYPE'd.

	colon	2,"#>",edigs,zsign
	dc.w	drop,hld,at
	dc.w	pad,over,subb,exit

; str ( w -- b u )
; Convert a signed integer to a numeric string.

	colon	3,"str",str,zedigs
	dc.w	dupp,tor,abss
	dc.w	bdigs,digs,rfrom
	dc.w	sign,edigs,exit

; HEX ( -- )
; Use radix 16 as base for numeric conversions.

	colon	3,"HEX",hex,zstr
	dc.w	dolit,16,nbase,store,exit

; DECIMAL ( -- )
; Use radix 10 as base for numeric conversions.

	colon	7,"DECIMAL",decim,zhex
	dc.w	dolit,10,nbase,store,exit

; DIGIT? ( c base -- u t )
; Convert a character to its numeric value. A flag indicates success.

	asm	6,"DIGIT?",digtq,zdecim
	movem.w	(a4)+,d0/d1	;D0:BASE, D1:ASCII CODE
	sub.w	#$30,d1
	bmi.s	1$
	cmp.w	#$9,d1
	bls.s	2$
	subq.w	#$7,d1
	cmp.w	#10,d1		;?in range 'A' thru 'Z'
	blo.s	1$
2$	cmp.w	d0,d1
	bhs.s	1$
	move.w	d1,-(a4)	;u
	move.w	#-1,-(a4)	;T
	jmp	(a5)
1$	clr.w	-(a4)		;F
	jmp	(a5)

; NUMBER? ( a -- n T | a F )
; Convert a number string to integer. Push a flag on the tos.

	colon	7,"NUMBER?",numbq,zdigtq
	dc.w	nbase,at,tor,dolit,0,over,count
	dc.w	over,cat,dolit,'$',equal
	dc.w	qbran,1$-base
	dc.w	hex,swapp,incc
	dc.w	swapp,decc
1$	dc.w	over,cat,dolit,'-',equal,tor
	dc.w	swapp,rat,subb,swapp,rat,plus,qdup
	dc.w	qbran,6$-base
	dc.w	decc,tor
2$	dc.w	dupp,tor,cat,nbase,at,digtq
	dc.w	qbran,4$-base
	dc.w	swapp,nbase,at,star,plus,rfrom
	dc.w	incc
	dc.w	donxt,2$-base
	dc.w	rat,swapp,drop
	dc.w	qbran,3$-base
	dc.w	negat
3$	dc.w	swapp
	dc.w	bran,5$-base
4$	dc.w	rfrom,rfrom,ddrop,ddrop,dolit,0
5$	dc.w	dupp
6$	dc.w	rfrom,ddrop
	dc.w	rfrom,nbase,store,exit

; ?KEY ( -- c T | F )
; Return input character and true, or false if no input.

	colon	4,"?KEY",qkey,znumbq
	dc.w	tqkey,atexe,exit

; KEY ( -- c )
; Wait for and return an input character.

	colon	3,"KEY",key,zqkey
1$	dc.w	qkey
	dc.w	qbran,1$-base
	dc.w	exit

; EMIT ( c -- )
; Send a character to the output device.

	colon	4,"EMIT",emit,zkey
	dc.w	temit,atexe,exit

; NUF? ( -- t )
; Return false if no input, else pause and if CR return true.

	colon	4,"NUF?",nufq,zemit
	dc.w	qkey		;get key
	dc.w	dupp
	dc.w	qbran,1$-base	;?no input
	dc.w	ddrop
	dc.w	key		;wait for key
	dc.w	dolit,crr,equal	;?cr
1$	dc.w	exit

; PACE ( -- )
; Send a pace character for the file downloading process.

	colon	4,"PACE",pace,znufq
	dc.w	dolit,11,emit,exit

; SPACE ( -- )
; Send the blank character to the output device.

	colon	5,"SPACE",space,zpace
	dc.w	bl,emit,exit

; SPACES ( +n -- )
; Send n spaces to the output device.

	colon	6,"SPACES",spacs,zspace
	dc.w	dolit,0,max,tor
	dc.w	bran,1$-base
2$	dc.w	space
1$	dc.w	donxt,2$-base
	dc.w	exit

; TYPE ( b u -- )
; Output u characters from b.

	colon	4,"TYPE",types,zspacs
	dc.w	tor
	dc.w	bran,2$-base
1$	dc.w	dupp,cat,emit
	dc.w	incc
2$	dc.w	donxt,1$-base
	dc.w	drop,exit

; CR ( -- )
; Output a carriage return and a line feed.

	colon	2,"CR",cr,ztypes
	dc.w	dolit,lf,dolit,crr,emit,emit,exit

; do$ ( -- a )
; Return the address of a compiled string.

	colon	3+COMPO,"do$",dostr,zcr
	dc.w	rfrom,rat,rfrom,count,plus
	dc.w	algnd,tor,swapp,tor,exit

; $"| ( -- a )
; Run time routine compiled by $" . Return address of a compiled string.

	colon	3+COMPO,'$"|',strqp,zdostr
	dc.w	dostr,exit	; Force to call do$

; ."| ( -- )
; Run time routine of ." . Output a compiled string.

	colon	3+COMPO,'."|',dotqp,zstrqp
	dc.w	dostr,count,types,exit

; .R ( n +n -- )
; Display an integer in a field of n columns, right justified.

	colon	2,".R",dotr,zdotqp
	dc.w	tor,str,rfrom,over,subb
	dc.w	spacs,types,exit

; U.R ( u +n -- )
; Display an unsigned integer in n column, right justified.

	colon	3,"U.R",udotr,zdotr
	dc.w	tor,bdigs,digs,edigs
	dc.w	rfrom,over,subb
	dc.w	spacs,types,exit

; U. ( u -- )
; Display an unsigned integer in free format.

	colon	2,"U.",udot,zudotr
	dc.w	bdigs,digs,edigs
	dc.w	space,types,exit

; . ( w -- )
; Display an integer in free format, preceeded by a space.

	colon	1,".",dot,zudot
	dc.w	nbase,at,dolit,10,xorr	;?decimal
	dc.w	qbran,1$-base
	dc.w	udot,exit		;no, display unsigned
1$	dc.w	str,space,types,exit	;yes, display signed

; ? ( a -- )
; Display a contents in a memory cell.

	colon	1,"?",quest,zdot
	dc.w	at,dot,exit

; parse ( b u c -- b u delta ; <string> )
; Scan string delimited by c. Return found string and its offset.

	asm	5+COMPO,"parse",pars,zquest
	move.l	d2,-(sp)
	move.w	(a4)+,d1
	move.w	(a4)+,d2
	move.w	(a4)+,d7
	move.w	d7,-(sp)
	bra.s	1$
2$	addq.w	#1,d7
1$	cmp.b	(a5,d7.l),d1
	beq.s	2$
	move.w	d7,d0
	sub.w	(sp),d0
	sub.w	d0,d2
	ble.s	3$
	move.w	d7,-(sp)	; <string>
	bra.s	4$
5$	addq.w	#1,d7
4$	cmp.b	(a5,d7.l),d1
	dbeq	d2,5$
	seq	d2
	ext.w	d2
	move.w	d7,d0
	move.w	d7,d1
	move.w	(sp)+,d7
	sub.w	d7,d0
	move.w	d7,-(a4)	;b
	move.w	d0,-(a4)	;u
	sub.w	(sp)+,d1
	sub.w	d2,d1
	move.w	d1,-(a4)	;delta
	bra.s	6$
3$	move.w	(sp)+,-(a4)	;b
	clr.l	-(a4)		;u delta
6$	move.l	(sp)+,d2
	jmp	(a5)

; PARSE ( c -- b u ; <string> )
; Scan input stream and return counted string delimited by c.

	colon	5,"PARSE",parse,zpars
	dc.w	tor,tib,inn,at,plus	;current input buffer pointer
	dc.w	ntib,at,inn,at,subb	;remaining count
	dc.w	rfrom,pars,inn,pstor,exit

; .( ( -- )
; Output following string up to next ) .

	colon	2+IMEDD,".(",dotpr,zparse
	dc.w	dolit,')',parse,types,exit

; ( ( -- )
; Ignore following string up to next ) . A comment.

	colon	1+IMEDD,"(",paren,zdotpr
	dc.w	dolit,')',parse,ddrop,exit

; \ ( -- )
; Ignore following text till the end of line.

	colon	1+IMEDD,"\",bksla,zparen
	dc.w	ntib,at,inn,store,exit

; CHAR ( -- c )
; Parse next word and return its first character.

	colon	4,"CHAR",char,zbksla
	dc.w	bl,parse,drop,cat,exit

; TOKEN ( -- a ; <string> )
; Parse a word from input stream and copy it to name dictionary.

	colon	5,"TOKEN",token,zchar
	dc.w	bl,parse,dolit,31,min
	dc.w	np,at			;,over,subb,cellm
	dc.w	packs,exit

; WORD ( c -- a ; <string> )
; Parse a word from input stream and copy it to the code dictionary.

	colon	4,"WORD",wordd,ztoken
	dc.w	parse,here,packs,exit

; NAME> ( na -- ca )
; Return a code address given a name address.
	
	colon	5,"NAME>",namet,zwordd
	dc.w	cellm,cellm,at,exit

; SAME? ( a a u -- a a f \ -0+ )
; Compare u cells in two strings. Return 0 if identical.

	asm	5,"SAME?",sameq,znamet
	move.w	(a4)+,d0
	move.w	(a4)+,d7
	lea	(a5,d7.l),a0
	move.w	(a4)+,d7
	lea	(a5,d7.l),a1
	subq.w	#1,d0
1$	cmp.w	(a0)+,(a1)+
	dbne	d0,1$
	sne	d0
	ext.w	d0
	move.w	d0,-(a4)
	jmp	(a5)

; find ( a va -- ca na | a F )
; Search a vocabulary for a string. Return ca and na if succeeded.

	asm	4,"find",find,zsameq
	move.w	(a4)+,d7	;vocabulary
	move.w	(a5,d7.l),d7
	moveq	#0,d1
	move.w	(a4)+,d1	;string address
	lea	(a5,d1.l),a0
2$	lea	(a5,d7.l),a1
	moveq	#$1f,d0
	and.b	(a1),d0
	cmp.b	(a0),d0		;?lengths matches
	beq.s	1$
3$	move.w	-(a1),d7	;get link
	bne.s	2$		;?start of vocabulary
	move.w	d1,-(a4)
	clr.w	-(a4)		;a F
	jmp	(a5)
1$	movem.l	d0/a0/a1,-(sp)
	addq.w	#1,a0
	addq.w	#1,a1
	subq.w	#1,d0
4$	cmp.b	(a0)+,(a1)+	;compare name
	dbne	d0,4$
	movem.l	(sp)+,d0/a0/a1
	bne.s	3$		;nat match, so try next
	move.w	-4(a1),-(a4)	;ca
	move.w	d7,-(a4)	;na
	jmp	(a5)

; NAME? ( a -- ca na | a F )
; Search all context vocabularies for a string.

	colon	5,"NAME?",nameq,zfind
	dc.w	cntxt,dupp,dat,xorr
	dc.w	qbran,1$-base
	dc.w	cellm
1$	dc.w	tor
2$	dc.w	rfrom,cellp,dupp,tor
	dc.w	at,qdup
	dc.w	qbran,3$-base
	dc.w	find,qdup
	dc.w	qbran,2$-base
	dc.w	rfrom,drop,exit
3$	dc.w	rfrom,drop
	dc.w	dolit,0,exit

; ^H ( bot eot cur -- bot eot cur )
; Backup the cursor by one character.

	colon	2,"^H",bksp,znameq
	dc.w	tor,over,rfrom,swapp,over,xorr
	dc.w	qbran,1$-base
	dc.w	dolit,bkspp,techo,atexe,decc
	dc.w	bl,techo,atexe
	dc.w	dolit,bkspp,techo,atexe
1$	dc.w	exit

; TAP ( bot eot cur c -- bot eot cur )
; Accept and echo the key stroke and bump the cursor.

	colon	3,"TAP",tap,zbksp
	dc.w	dupp,techo,atexe
	dc.w	over,cstor,incc,exit

; kTAP ( bot eot cur c -- bot eot cur )
; Process a key stroke, CR of backspace.

	colon	4,"kTAP",ktap,ztap
	dc.w	dupp,dolit,crr,xorr
	dc.w	qbran,2$-base
	dc.w	dolit,bkspp,xorr
	dc.w	qbran,1$-base
	dc.w	bl,tap,exit
1$	dc.w	bksp,exit
2$	dc.w	drop,swapp,drop,dupp,exit

; accept ( b u -- b u )
; Accept character to input buffer. Return with actual count.

	colon	6,"accept",accep,zktap
	dc.w	over,plus,over
1$	dc.w	ddup,xorr
	dc.w	qbran,4$-base
	dc.w	key,dupp
	dc.w	bl,dolit,127,withi
	dc.w	qbran,2$-base
	dc.w	tap
	dc.w	bran,3$-base
2$	dc.w	ttap,atexe
3$	dc.w	bran,1$-base
4$	dc.w	drop,over,subb,exit

; EXPECT ( b u -- )
; Accept input stream and store count in SPAN.

	colon	6,"EXPECT",expec,zaccep
	dc.w	texpe,atexe,span,store,drop,exit

; QUERY ( -- )
; Accept input stream to terminal input buffer.

	colon	5,"QUERY",query,zexpec
	dc.w	tib,dolit,80,texpe,atexe,ntib,store
	dc.w	drop,dolit,0,inn,store,exit

; CATCH ( ca -- 0 | err# )
; Execute word at ca and set up an error frame for it.

	colon	5,"CATCH",catch,zquery
	dc.w	spat,tor,handl,at,tor	;save error frame
	dc.w	rpat,handl,store,execu	;execute 
	dc.w	rfrom,handl,store	;restore error frame
	dc.w	rfrom,drop,dolit,0,exit	;no error

; THROW ( err# -- err# )
; Reset system to current local error frame an update error flag.

	colon	5,"THROW",throw,zcatch
	dc.w	handl,at,rpsto		;restore return stack
	dc.w	rfrom,handl,store	;restore handler frame
	dc.w	rfrom,swapp,tor,spsto	;restore data stack
	dc.w	drop,rfrom,exit

; NULL$ ( -- a )
; Return address of a null string with zero count.

	colon	5,"NULL$",nulls,zthrow
	dc.w	dovar			;emulate create
	dc.w	0
	dc.b	"coyote"
	cnop	0,2

; ABORT ( -- )
; Reset data stack and jump to quit.

	colon	5,"ABORT",abort,znulls
	dc.w	nulls,throw

; abort" ( f -- )
; Run time routine of ABORT" . Abort with message.

	colon	6+COMPO,'abort"',aborq,zabort
	dc.w	qbran,1$-base		;text flag
	dc.w	dostr,throw		;pass error string
1$	dc.w	dostr,drop,exit		;drop error

; $INTERPRET (a -- )
; Interpret a word. If failed, try to convert it to an integer.

	colon	10,"$INTERPRET",inter,zaborq
	dc.w	nameq,qdup
	dc.w	qbran,1$-base
	dc.w	cat,dolit,COMPO,andd	;?compile only lexicon bits
	dc.w	aborq
	dc.b	13," compile only"
	cnop	0,2
	dc.w	execu,exit		;execte defined word
1$	dc.w	tnumb,atexe		;convert a number
	dc.w	qbran,2$-base
	dc.w	exit
2$	dc.w	strqp
	dc.b	1,"?"			;error message
	cnop	0,2
	dc.w	throw			;error

; [ ( -- )
; Start the text interpreter.

	colon	1+IMEDD,"[",lbrac,zinter
	dc.w	dolit,inter,teval,store,exit

; .OK ( -- )
; Display 'ok' only while interpreting.

	colon	3,".OK",dotok,zlbrac
	dc.w	dolit,inter,teval,at,equal
	dc.w	qbran,1$-base
	dc.w	dotqp
	dc.b	3," ok"
	cnop	0,2
1$	dc.w	cr,exit

; ?STACK ( -- )
; Abort if the data stack underflows.

	colon	6,"?STACK",qstac,zdotok
	dc.w	depth,zless		;check only for underflow
	dc.w	aborq
	dc.b	10," underflow"
	cnop	0,2
	dc.w	exit

; EVAL ( -- )
; Intrpret the input stream.

	colon	4,"EVAL",eval,zqstac
1$	dc.w	token,dupp,cat		;?input stream empty
	dc.w	qbran,2$-base
	dc.w	teval,atexe,qstac	;evaluate input, check stack
	dc.w	bran,1$-base
2$	dc.w	drop,tprom,atexe,exit	;prompt

; PRESET ( -- )
; Reset data stack pointer and terminal input buffer (TIB)

	colon	6,"PRESET",prese,zeval
	dc.w	szero,at,spsto
	dc.w	dolit,TIBB,ntib,cellp,store,exit

; xio ( a a a -- )
; Reset the I/O vectors 'EXCEPT, 'TAP, 'ECHO and 'PROMPT

	colon	3+COMPO,"xio",xio,zprese
	dc.w	dolit,accep,texpe,dstor
	dc.w	techo,dstor,exit

; HAND ( -- )
; Select I/O/ vectors for terminal input

	colon	4,"HAND",hand,zxio
	dc.w	dolit,dotok,dolit,emit
	dc.w	dolit,ktap,xio,exit

; I/O ( -- a )
; Array to store default I/O vectors

	colon	3,"I/O",islo,zhand
	dc.w	dovar			;emulate CREATE
	dc.w	qrx,txsto		;default I/O vectors

; CONSOLE ( -- )
; Initiate terminal interface

	colon	7,"CONSOLE",conso,zislo
	dc.w	islo,dat,tqkey,dstor	;restore default I/O device
	dc.w	hand,exit		;keyboard input

; QUIT ( -- )
; Reset return stack pointer and start text interpreter.

	colon	4,"QUIT",quit,zconso
	dc.w	rzero,at,rpsto		;reset return stack pointer
1$	dc.w	lbrac			;start interpreter mode
2$	dc.w	query			;get input
	dc.w	dolit
	dc.w	eval
	dc.w	catch			;evaluate input
	dc.w	qdup
	dc.w	qbran,2$-base		;continue till error
	dc.w	space,count,types	;error message
	dc.w	dotqp
	dc.b	2,"? "			;error prompt
	cnop	0,2
	dc.w	bran,1$-base		;go back

;;================================================== THE COMPILER

; ' ( -- ca )
; Search context vocabulariesfor the next word in input stream.

	colon	1,"'",tick,zquit
	dc.w	token,nameq		;?defined
	dc.w	qbran,1$-base
	dc.w	exit			;yes, push code address
1$	dc.w	strqp
	dc.b	11,"{not found}"
	cnop	0,2
	dc.w	throw			;no, error

; ALLOT ( n -- )
; Allocate n bytes to the code dictionary.

	colon	5,"ALLOT",allot,ztick
	dc.w	cp,pstor,exit		;adjust code pointer

; , ( w -- )
; Compile an integer into the code dictionary.

	colon	1,$2c,comma,zallot
	dc.w	here,dupp,cellp		;cell boundary
	dc.w	cp,store,store,exit	;adjust code pointer an compile

; C, ( b -- )
; Compile a char into the code dictionary (Be carefull with odd length! )

	dc.w	ccom
	dc.w	zcomma
zccom:	equ	*-base
	dc.b	2
	dc.b	"C,"
	cnop	0,2
ccom:	equ	*-base
ccom_:	jsr	(a6)
;	colon	2,"C,",ccom,zcomma
	dc.w	here,dupp,incc,cp,store		; CP 1+
	dc.w	cstor,exit			; C!

; [COMPILE] ( -- ; <string> )
; Compile the next immediate word into code dictionary.

	colon	9+IMEDD,"[COMPILE]",bcomp,zccom
	dc.w	tick,comma,exit

; COMPILE ( -- )
; Compile the next address in colon list to code dictionary.

	colon	7+COMPO,"COMPILE",compi,zbcomp
	dc.w	rfrom,dupp,at,comma	;compile address
	dc.w	cellp,tor,exit		;adjust return address

; LITERAL ( w -- )
; Compile tos to code dictionary as an inline integer literal.

	colon	7+IMEDD,"LITERAL",liter,zcompi
	dc.w	compi,dolit,comma,exit

; $," ( -- )
; Compile a literal string up to next " .

	dc.w	strcq
	dc.w	zliter
zstrcq:	equ	*-base
	dc.b	3
	dc.b	'$,"'
	cnop	0,2
strcq:	equ	*-base
strcq_:	jsr	(a6)
;	colon	3,'$,"',strcq,zliter
	dc.w	dolit,'"',wordd		;move string to code dictionary
	dc.w	count,plus,algnd	;calculate aligned end of string
	dc.w	cp,store,exit		;adjust the code pointer

; RECURSE ( -- )
; Make the current word available for compilation.

	colon	7+IMEDD,"RECURSE",recur,zstrcq
	dc.w	last,at,namet,comma,exit

;;================================================== STRUCTURES

; FOR ( -- a )
; Start a FOR-NEXT loop structure in a colon definition.

	colon	3+IMEDD,"FOR",for,zrecur
	dc.w	compi,tor,here,exit

; BEGIN ( -- a )
; Start an infinite or indefinite loop structure.

	colon	5+IMEDD,"BEGIN",begin,zfor
	dc.w	here,exit

; NEXT ( a -- )
; Terminate a FOR-NEXT loop.

	colon	4+IMEDD,"NEXT",next,zbegin
	dc.w	compi,donxt,comma,exit

; UNTIL ( a -- )
; Terminate a BEGIN-UNTIL indefinite loop.

	colon	5+IMEDD,"UNTIL",until,znext
	dc.w	compi,qbran,comma,exit

; AGAIN ( a -- )
; Terminate a BEGIN-AGAIN infinite loop.

	colon	5+IMEDD,"AGAIN",again,zuntil
	dc.w	compi,bran,comma,exit

; IF ( -- A )
; Begin a conditional branch structure.

	colon	2+IMEDD,"IF",iff,zagain
	dc.w	compi,qbran,here
	dc.w	dolit,0,comma,exit

; AHEAD ( -- A )
; Compile a forward branch instruction.

	colon	5+IMEDD,"AHEAD",ahead,ziff
	dc.w	compi,bran,here,dolit,0,comma,exit

; REPEAT ( A a -- )
; Terminate a BEGIN-WHILE-REPEAT indefinite loop.

	colon	6+IMEDD,"REPEAT",repea,zahead
	dc.w	again,here,swapp,store,exit

; THEN ( A -- )
; Terminata a conditional branch structure.

	colon	4+IMEDD,"THEN",thenn,zrepea
	dc.w	here,swapp,store,exit

; AFT ( a -- a A )
; Jump to then in a FOR-AFT-THEN-NEXT loop the first time through.

	colon	3+IMEDD,"AFT",aft,zthenn
	dc.w	drop,ahead,begin,swapp,exit

; ELSE ( A -- A )
; Start the flase clause in an IF-ELSE-THEN structure.

	colon	4+IMEDD,"ELSE",elsee,zaft
	dc.w	ahead,swapp,thenn,exit

; WHILE ( a -- A a )
; Conditional branch out of BEGIN-WHILE-REPEAT loop.

	colon	5+IMEDD,"WHILE",while,zelsee
	dc.w	iff,swapp,exit

; ABORT" ( -- ; <string> )
; Conditional abort with an error message.

	colon	5+IMEDD,'ABORT"',abrtq,zwhile
	dc.w	compi,aborq,strcq,exit

; $" ( -- ; <string> )
; Compile an inline string literal.

	colon	2+IMEDD,'$"',strq,zabrtq
	dc.w	compi,strqp,strcq,exit

; ." ( -- ; <string> )
; Compile an inline string literal to be typed out at run time.

	colon	2+IMEDD,'."',dotq,zstrq
	dc.w	compi,dotqp,strcq,exit

;;===================================================== NAME COMPILER

; ?UNIQUE ( a -- a )
; Display a warning message if the word already exists.

	colon	7,"?UNIQUE",uniqu,zdotq
	dc.w	dupp,nameq		;?name exists
	dc.w	qbran,1$-base
	dc.w	dotqp			;redefinitions are OK
	dc.b	7," reDef "		;but the user should be warned
	dc.w	over,count,types	;just in case its not planned
1$	dc.w	drop,exit

; $,n ( na -- )
; Build a new dictionary name using the string at na.

; : $,n
; DUP C@					     \ ?null input
; IF
;    ?UNIQUE 					     \ check for name
;    HERE ALIGNED SWAP OVER CELL+ CURRENT @ @ OVER ! \ get link
;    CELL+ OVER C@ 1+ OVER + ALIGNED >R 	     \ save new CP for later
;    OVER C@ 1+ OVER LAST ! 			     \ last word ptr
;    CMOVE 					     \ move string
;    R@ SWAP ! R> CP ! 				     \ set new CP
;    EXIT
; THEN
;    $" name"					     \ error
;    THROW
; ;
	dc.w	sname
	dc.w	zuniqu
zsname:	equ	*-base
	dc.b	3
	dc.b	"$,n"
	cnop	0,2
sname:	equ	*-base
sname_:	jsr	(a6)
;	colon	3,'$,n',sname,zuniqu
	dc.w	dupp,cat
	dc.w	qbran,1$-base
	dc.w	uniqu
	dc.w	here,algnd,swapp,over
	dc.w	cellp,crrnt,at,at,over,store,cellp
	dc.w	over
	dc.w	cat,incc,over,plus
	dc.w	algnd,tor
	dc.w	over,cat,incc
	dc.w	over,last,store
	dc.w	cmove
	dc.w	rat,swapp,store,rfrom,cp,store
	dc.w	exit
1$	dc.w	strqp
	dc.b	5," name"
	cnop	0,2
	dc.w	throw

;;======================================================== FORTH COMPILER

; $COMPILE ( a -- )
; Compile next word to code dictionary as a token or literal

	colon	8,"$COMPILE",scomp,zsname
	dc.w	nameq,qdup		;?defined
	dc.w	qbran,2$-base
	dc.w	cat,dolit,IMEDD,andd	;?immediate
	dc.w	qbran,1$-base
	dc.w	execu,exit		;its immediate, execute
1$	dc.w	comma,exit		;its not immedaiate, compile
2$	dc.w	tnumb,atexe		;try to convert to number
	dc.w	qbran,3$-base
	dc.w	liter,exit		;compile number as integer
3$	dc.w	strqp
	dc.b	5," what"		;error
	dc.w	throw

; OVERT ( -- )
; Link a new word into the current vocabulary.

	colon	5,"OVERT",overt,zscomp
	dc.w	last,at,crrnt,at,store,exit

; ; ( -- )
; Terminate a colon definition

	colon	1+IMEDD+COMPO,";",semis,zovert
	dc.w	compi,exit,lbrac,overt
	dc.w	here,algnd,cp,store		;changed from original
	dc.w	exit

; ] ( -- )
; Start compiling the words in the input stream

	colon	1,"]",rbrac,zsemis
	dc.w	dolit,scomp,teval,store,exit

; call, ( -- )
; Assemble a call instruction to ca /on AMIGA this is a JSR (A6)/

	dc.w	callc_-base
	dc.w	zsemis
zcallc:	equ	*-base
	dc.b	5
	dc.b	"call,"
	cnop	0,2
callc:	equ	*-base
callc_:	jsr	(a6)
;	colon	5,'call,',callc,zrbrac
	dc.w	dolit,calll,comma,exit

; : ( -- ; <string> )
; Start a new colon definition using next word as its name

	colon	1,":",colo,zcallc
	dc.w	token,sname
	dc.w	callc,rbrac,exit

; IMMEDIATE ( -- )
; Make the last compiled word an immediate word

	colon	9,"IMMEDIATE",immed,zcolo
	dc.w	dolit,IMEDD,last,at,at,orr
	dc.w	last,at,store,exit

; FORGET ( -- )
; Forget words from word

	colon	6+IMEDD,"FORGET",forg,zimmed
	dc.w	token,nameq,qdup
	dc.w	qbran,1$-base
	dc.w	swapp,drop,cellm	;LFA
	dc.w	dupp,cellm,swapp	;CFA
	dc.w	at,last,store		;make it last
	dc.w	cp,store		;restore CP
	dc.w	overt,exit		;repeair links, then exit
1$	dc.w	strqp
	dc.b	14," {forget what}"
	cnop	0,2
	dc.w	throw

;;==================================================== DEFINING WORDS

; USER (u -- ; <string> )
; Compile a new user variable

	colon	4,"USER",userr,zforg
	dc.w	token,sname,overt
	dc.w	callc
	dc.w	dolit,douser,comma
	dc.w	comma,exit

; CREATE ( -- ; <string> )
; Compile a new array entry without allocating code space

	colon	6,"CREATE",creat,zuserr
	dc.w	token,sname,overt
	dc.w	callc
	dc.w	dolit,dovar,comma,exit

; VARIABLE ( -- ; <string> )
; Compile a new variable initialized to 0

	colon	8,"VARIABLE",varia,zcreat
	dc.w	creat,dolit,0,comma,exit

; $40      CONSTANT GADGETUP ( INTUITION gadgetup )
; $14      CONSTANT CLASS    ( IMSG class )
; CLASS 4+ CONSTANT CODE     ( IMSG code  )

; Run time code:
; dc.w dolit,<value>,exit

; : CONSTANT ( w -- ) TOKEN $,n OVERT call, LITERAL COMPILE exit ;

; CONSTANT ( -- ; <string> )
; Compile a constant

	colon	8,"CONSTANT",const,zvaria
	dc.w	token,sname,overt
	dc.w	callc
	dc.w	liter		;put dolit,<value>
	dc.w	compi,exit	;put <exit>
	dc.w	exit
	
;;==================================================== TOOLS

; _TYPE ( b u -- )
; Display a string. Filter non-printing characters.

	colon	5,"_TYPE",utype,zconst
	dc.w	tor			;start count down loop
	dc.w	bran,2$-base		;skip first pass
1$	dc.w	dupp,cat,tchar,emit	;display only printable
	dc.w	incc			;increment address
2$	dc.w	donxt,1$-base		;loop till done
	dc.w	drop,exit

; dm+ ( a u -- a )
; Dump u bytes from a, leaving a+u on the stack.

	colon	3,"dm+",dumpp,zutype
	dc.w	over,dolit,4,udotr	;display address
	dc.w	space,tor		;start count down loop
	dc.w	bran,2$-base		;skip first pass
1$	dc.w	dupp,cat,dolit,3,udotr	;display numeric data
	dc.w	incc			;increment address
2$	dc.w	donxt,1$-base		;loop till done
	dc.w	exit

; DUMP ( a u -- )
; Dump u bytes from a, in a formated manner.

	colon	4,"DUMP",dump,zdumpp
	dc.w	nbase,at,tor,hex	;save radix set hex
	dc.w	dolit,16,slash		;change count to lines
	dc.w	tor			;start count down loop
1$	dc.w	cr,dolit,16,ddup,dumpp	;display numerics
	dc.w	rot,rot
	dc.w	dolit,2,spacs,utype	;display printable characters
	dc.w	nufq,inver		;user control
	dc.w	qbran,2$-base
	dc.w	donxt,1$-base		;loop till done
	dc.w	bran,3$-base
2$	dc.w	rfrom,drop		;cleanup loop stack, early exit
3$	dc.w	drop,rfrom,nbase,store	; restore radix
	dc.w	exit

; .S ( ... -- ... )
; Display the context of the data stack

	colon	2,".S",dots,zdump
	dc.w	cr,depth		;stack depth
	dc.w	tor			;start count down loop
	dc.w	bran,2$-base		;skip first pass
1$	dc.w	rat,pick,dot		;index stack, display context
2$	dc.w	donxt,1$-base		;loop till done
	dc.w	dotqp
	dc.b	5," <tos"
	cnop	0,2
	dc.w	exit

; .ID ( na -- )
; Display the name at address.

	colon	3,".ID",dotid,zdots
	dc.w	qdup			;if zero no name
	dc.w	qbran,1$-base
	dc.w	count,dolit,$1f,andd	;mask lexicon bits
	dc.w	utype,exit		;display name string
1$	dc.w	dotqp
	dc.b	9," {noName}"
	cnop	0,2
	dc.w	exit

; !CSP ( -- )
; Save stack pointer in CSP for error checking

	colon	4,"!CSP",stcsp,zdotid
	dc.w	spat,csp,store,exit	;save pointer

; ?CSP ( -- )
; Abort if stack pointer differs from that saved in CSP

	colon	4,"?CSP",qcsp,zstcsp
	dc.w	spat,csp,at,xorr	;compare pointers
	dc.w	aborq			;abort if different
	dc.b	8,"{stacks}"
	cnop	0,2
	dc.w	exit

; >NAME ( ca -- na | F )
; Convert code address to a name address

	colon	5,">NAME",tname,zqcsp
	dc.w	cntxt,at,at
	dc.w	dolit,1$-base,xcall
	dc.w	exit
1$	move.w	(a4)+,d7	;context ptr
	move.w	(a4)+,d0	;ca
3$	lea	(a5,d7.l),a0	;context
	cmp.w	-4(a0),d0	;?found ca
	beq.s	2$
	move.w	-(a0),d7
	bne.s	3$
2$	move.w	d7,-(a4)	:na | F
	rts

; SEE ( -- ; <string> )
; A simple decompiler

	colon	3,"SEE",see,ztname
	dc.w	tick			;starting address
	dc.w	cr
	dc.w	dupp
	dc.w	at,dolit,calll,xorr	;?colon definition
	dc.w	aborq
	dc.b	10,"{asm code}"
	cnop	0,2
1$	dc.w	cellp,dupp,at,dupp
	dc.w	dolit,exit,xorr
	dc.w	qbran,4$-base		;end if 'exit' reached
	dc.w	tname			;try to find name
	dc.w	qdup			;name address or zero
	dc.w	qbran,2$-base
	dc.w	space,dotid		;display name
	dc.w	bran,3$-base
2$	dc.w	dupp,at,udot		;display a number
3$	dc.w	nufq			;user control
	dc.w	qbran,1$-base
4$	dc.w	drop,exit
		
; WORDS ( -- )
; Display the names in the context vocabulary.

	colon	5,"WORDS",words,zsee
	dc.w	cr,cntxt,at		;only in context
1$	dc.w	at,qdup			;?at end of list
	dc.w	qbran,2$-base
	dc.w	dupp,space,dotid	;display a name
	dc.w	cellm,nufq		;user control
	dc.w	qbran,1$-base
	dc.w	drop
2$	dc.w	exit

; EXEHDR ( -- )
; Executable header.

	colon	6,"EXEHDR",exehdr,zwords
	dc.w	dovar			;Emulate create
exehdr2:equ	*-base
	dc.w	0,HUNK_HDR		;+0
	dc.w	0,0			;+4
	dc.w	0,1			;+8
	dc.w	0,0			;+12
	dc.w	0,0			;+16
	dc.w	0,$4000			;+20	Total code size = 64K
	dc.w	0,HUNK_CDE		;+24
csize:	equ	*-base+2
	dc.w	0,0			;+28
	dc.w	$6000+init-start-4	;+32
huend:	equ	*-base
	dc.w	0,HUNK_END		;+34
	
; SAVE-FORTH ( -- )
; Save forth system.

	colon	10+IMEDD,"SAVE-FORTH",savfor,zexehdr
	dc.w	cr
	dc.w	dotqp
	dc.b	10,"FILE NAME>"
	cnop	0,2
	dc.w	pad,dolit,32,expec,cr,span,at		;get a file name
	dc.w	qbran,1$-base
	dc.w	pad,span,at,plus,dolit,0,swapp,cstor 	;null terminate
	dc.w	pad,dolit,1006,opnfil			;try to open a new file
	dc.w	qbran,2$-base
	dc.w	decim					;save system in decimal number base
	dc.w	here,algnd,cellp
	dc.w	dolit,0,dolit,4,ummod,swapp
	dc.w	qbran,4$-base
	dc.w	cellp					;longword aligned
4$	dc.w	dupp,dolit,csize,store			;store size in exehdr
	dc.w	dolit,4,umsta,drop,tor			;convert to bytes
	dc.w	dupp,exehdr,dolit,huend-exehdr2,wrtfil,drop	;write hdr
	dc.w	dupp,dolit,0,rfrom,dolit,2,subb,wrtfil,drop		;write code
	dc.w	dupp,dolit,huend,dolit,4,wrtfil		;write hunk_end
	dc.w	qbran,3$-base
	dc.w	clofil
1$	dc.w	exit
2$	dc.w	strqp
	dc.b	24," {Could not create file}"
	cnop	0,2
	dc.w	throw
3$	dc.w	clofil
	dc.w	strqp
	dc.b	14," {Write error}"
	cnop	0,2
	dc.w	throw

; hi ( -- )
; Display the sign-on message of 4th.

	colon	2,"hi",hi,zsavfor
	dc.w	cr,dotqp
	dc.b	24,"4th V1.016 AMIGA version"
	cnop	0,2
	dc.w	cr,exit

; 'BOOT ( -- a )
; The application startup vector.

	colon	5,"'BOOT",tboot,zhi
	dc.w	dovar
	dc.w	hi			;application to boot

; COLD ( -- )
; The hilevel cold start sequence.

lastn:	equ	*+4-base

	colon	4,"COLD",cold,ztboot
vcold:	dc.w	prese			;initialize data stack and TIB
	dc.w	rzero,at,rpsto		;initialize return stack
	dc.w	tboot,atexe
	dc.w	forth			;make FORTH as a context
	dc.w	cntxt,at,dupp,crrnt	;vocabulary
	dc.w	dstor,overt		;link last word to context
	dc.w	quit			;start command line interpreter
	dc.w	bran,vcold-base		;just in any case...

;; =========================================== USER VARIABLES

UPP:	ds.w	2		;reserved space
	dc.w	SP0		;SP0
	dc.w	RP0		;RP0
	dc.w	qrx		;'?KEY
	dc.w	txsto		;'EMIT
	dc.w	accep		;'EXPECT
	dc.w	ktap		;'TAP
	dc.w	txsto		;'ECHO
	dc.w	dotok		;'PROMPT
	dc.w	basee		;BASE
	dc.w	0		;tmp
	dc.w	0		;SPAN
	dc.w	0		;>IN
	dc.w	0		;#TIB
	dc.w	TIBB		;TIB
	dc.w	0		;CSP
	dc.w	inter		;'EVAL
	dc.w	numbq		;'NUMBER
	dc.w	0		;HLD
	dc.w	0		;HANDLER
	dc.w	0		;CONTEXT pointer
	ds.w	vocss		;vocabulary stack
	dc.w	0		;CURRENT pointer
	dc.w	0		;vocabulary link pointer
	dc.w	CTOP-base	;CP
	dc.w	SP0		;NP
	dc.w	lastn		;LAST
fstp:	equ	*-base
	dc.w	FST		;FILES-TAB

CTOP:
code_len=*-start
	ds.b	EM-code_len
	
	END

( SAVE EXECUTABLE CODE BY PETER BAKOTA 98/26/12)
( 4TH VERSION 1.016                            )

$03F3 CONSTANT HUNK_HEADER
$03E9 CONSTANT HUNK_CODE
$03F2 CONSTANT HUNK_END

CREATE EXEHDR
 0 , HUNK_HEADER ,	\ HEADER CODE
 0 , 0 ,		\ HEADER NAME LENGTH
 0 , 0 ,		\ FIRST
 0 , 1 ,		\ LAST
 0 , $4000 ,		\ CODE SIZE 64K
 0 , HUNK_CODE ,	\ CODE CODE
 0 , 0 ,		\ CODE SIZE
 $6006 ,		\ BRA.S INIT
 0 , HUNK_END ,		\ HUNK END
 
EXEHDR 30 + CONSTANT CSIZE
EXEHDR 34 + CONSTANT HUEND

: SAVE-FORTH ( -- )
CR ." FILE NAME>" PAD 32 EXPECT CR SPAN @
IF
 PAD SPAN @ + 0 SWAP C! 		( NULL TERMINATED)
 PAD 1006 OPEN-FILE    			( TRY TO OPEN A NEW FILE)
 IF
  DECIMAL				( save system in decimal number base)
  HERE ALIGNED CELL+
  0 4 UM/MOD SWAP			( convert to longs)
  0<> IF
   CELL+				( LONG WORD ALIGNED)
  THEN
  DUP CSIZE !
  4 UM* DROP >R
  DUP EXEHDR 36     WRITE-FILE DROP	( Write header)
  DUP 0      R> 2 - WRITE-FILE DROP	( Write code)
  DUP HUEND  4      WRITE-FILE		( Write HUNK_END)
  IF
   CLOSE-FILE
   EXIT
  ELSE
   CLOSE-FILE
   ABORT" {WRITE ERROR}"
  THEN
 ELSE
  ABORT" CAN'T OPEN FILE!"
THEN
; 

( SAVE-FORTH like SAVE-SYSTEM but, makes executable)

