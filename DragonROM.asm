;
; DragonROM.asm	Disasembily of Dragon 32/64 Basic ROM.
;
; Disasembled 2006-08-14, P.Harvey-Smith.
;
; From this file you can assemble any of the Dragon basic ROMS.
;
; Define DRAGON64 	to assemble for Dragon 64, 32K rom
; Define DRAGON64RAM	to assemble for Dragon 64, 64K rom
;
; If neither are defined, a Dragon32 ROM will be generated.
;
; Changes :
;
; 2006-08-15, P.Harvey-Smith.
;	Merged Dragon 32, 64 and 64 ram mode disassemblies, this file
;	can now build to any of the Dragon ROMS.
;
;	Re-wrote all CMPX, BRN, LDA, LDX, TST instruction hiding instructions
;	using defines from romdefs.asm.
;
;	Began systematic replacement of system variable reffrences in the 
;	$0000-$03FF reigon with symbolic names from romdefs.asm, this should 
;	make understanding  the code easier.
;
; 2012-10-08, P.Harvey-Smith.
;	Updated all unknown data tables to indicate their function by cross
;	refferencing with "Colour Basic Unravelled" and "Extended Colour Basic Unravelled".
;
; 2020-03-02, P.Harvey-Smith.
; 	Finished commenting the entire file.
;


        ifdef	Dragon64ram
	ORG	$C000				; Dragon 64 ram mode
	else
	ORG     $8000				; Dragon 32/64 ROM mode
	endc
	
	use	romdefs.asm
	use	dgndefs.asm
	use	BasicTokens.asm
	use	samdefs.asm
	use	ascii.asm
	use	cpudefs.asm

; low RAM defs, not defined in romdefs.asm

Eval37	equ	$37
Eval38	equ	$38
Eval39	equ	$39
Eval3A	equ	$3A
Eval3B	equ	$3B
Eval3C	equ	$3C
Eval3E	equ	$3E
Eval3F	equ	$3F

Eval40	equ	$40
Eval41	equ	$41
Eval42	equ	$42
Eval43	equ	$43
Eval44	equ	$44
Eval45	equ	$45
Eval46	equ	$46
Eval47	equ	$47
Eval48	equ	$48
Eval49	equ	$49
Eval4A	equ	$4A
Eval4B	equ	$4B
Eval4C	equ	$4C
Eval4D	equ	$4D
Eval4E	equ	$4E

EvalCF	equ	$CF

EvalD1	equ	$D1
EvalD2	equ	$D2
EvalD3	equ	$D3
EvalD4	equ	$D4
EvalD5	equ	$D5
EvalD6	equ	$D6
EvalD7	equ	$D7
EvalD8	equ	$D8
EvalD9	equ	$D9
EvalDA	equ	$DA
EvalDB	equ	$DB
EvalDC	equ	$DC


RomBase:				; lable to mark beginning of ROM
;
; Basic vector table
;

BasicHWInit:
L8000   JMP     >DoHWInit		; Hardware init, routine to chain to in Y

BasicSWInit:
L8003   JMP     >DoSWInit		; Software Init

BasicKbdIn:
L8006   JMP     >TextScanKbd 		; Scan keyboard, if a key pressed return in A

BasicCursorB:
L8009   JMP     >TextUpdateCurs 	; Update cursor (blink)	

BasicScreenOut:
L800C   JMP     >TextVDUOut 		; Output char in A (to screen)

BasicPrintOut:
L800F   JMP     >PrinterOut 		; Output char in A to printer

BasicJoyIn:
L8012   JMP     >SysReadJoystick 	; Read joysticks

BasicCassOn:
L8015   JMP     >CasMotorOn 		; Turn on cassette motor

BasicCassOff:
L8018   JMP     >CasMotorOff 		; Turn off cassette motor

CasWriteLeader:
L801B   JMP     >LBE68			; Write leader to tape

BasicCassByOut:
L801E   JMP     >CasByteOut 		; Output a byte in A to tape 

BasicCassOnRd:
L8021   JMP     >CasReadLeader 		; Read leader and get ready top read data

BasicCassByIn:
L8024   JMP     >CasByteIn 		; Read a byte in from tape

BasicCassBitIn:
L8027   JMP     >CasBitIn 		; Read a bit from tape

BasicSerIn:
L802A   JMP     >DoSerialIn		; Read a byte from serial port (D64 only)

BasicSerOut:
L802D   JMP     >DoSerialOut		; Send a byte to serial port (D64 only)

BasicSetBaud:
L8030   JMP     >DoSetBaud		; Set baud rate (D64 only)

;
; Basic keyword table (commands).
;

BasCommandWords
	FCS	/FOR/
        FCS     /GO/
        FCS     /REM/
	FCS	/'/
        FCS     /ELSE/
        FCS     /IF/
        FCS     /DATA/
        FCS     /PRINT/
        FCS     /ON/
        FCS     /INPUT/
        FCS     /END/
        FCS     /NEXT/
        FCS     /DIM/
        FCS     /READ/
        FCS     /LET/
        FCS     /RUN/
        FCS     /RESTORE/
        FCS     /RETURN/
        FCS     /STOP/
        FCS     /POKE/
        FCS     /CONT/
        FCS     /LIST/
        FCS     /CLEAR/
        FCS     /NEW/
        FCS     /DEF/
        FCS     /CLOAD/
        FCS     /CSAVE/
        FCS     /OPEN/
        FCS     /CLOSE/
        FCS     /LLIST/
        FCS     /SET/
        FCS     /RESET/
        FCS     /CLS/
        FCS     /MOTOR/
        FCS     /SOUND/
        FCS     /AUDIO/
        FCS     /EXEC/
        FCS     /SKIPF/
        FCS     /DEL/
        FCS     /EDIT/
        FCS     /TRON/
        FCS     /TROFF/
        FCS     /LINE/
        FCS     /PCLS/
        FCS     /PSET/
        FCS     /PRESET/
        FCS     /SCREEN/
        FCS     /PCLEAR/
        FCS     /COLOR/
        FCS     /CIRCLE/
        FCS     /PAINT/
        FCS     /GET/
        FCS     /PUT/
        FCS     /DRAW/
        FCS     /PCOPY/
        FCS     /PMODE/
        FCS     /PLAY/
        FCS     /DLOAD/
        FCS     /RENUM/
        FCS     /TAB(/
        FCS     /TO/
        FCS     /SUB/
        FCS     /FN/
        FCS     /THEN/
        FCS     /NOT/
        FCS     /STEP/
        FCS     /OFF/
	FCS	/+/
	FCS	/-/
	FCS	/*/
	FCS	"/"
	FCS	/^/
        FCS     /AND/
        FCS     /OR/
	FCS	/>/
	FCS	/=/
	FCS	/</
        FCS     /USING/

;
; Keyword dispatch table (commands).
;
BasCommandDisp
        FDB     CmdFor
        FDB     CmdGo
        FDB     CmdREM
        FDB     CmdREM
        FDB     CmdREM
        FDB     CmdIF
        FDB     CmdData
        FDB     CmdPrint
        FDB     CmdON
        FDB     CmdInput
        FDB     CmdEnd
        FDB     CmdNext
        FDB     CmdDim
        FDB     CmdRead
        FDB     CmdLet
        FDB     CmdRun
        FDB     CmdRestore
        FDB     CmdReturn
        FDB     CmdStop
        FDB     CmdPoke
        FDB     CmdCont
        FDB     CmdList
        FDB     CmdClear
        FDB     CmdNew
        FDB     CmdDef
        FDB     CmdCload
        FDB     CmdCsave
        FDB     CmdOPEN
        FDB     LB64C
        FDB     CmdLList
        FDB     CmdSET
        FDB     CmdRESET
        FDB     CmdCLS
        FDB     CmdMOTOR
        FDB     CmdSOUND
        FDB     CmdAudio
        FDB     CmdExec
        FDB     CmdSKIPF
        FDB     CmdDelete
        FDB     CmdEdit
        FDB     CmdTron
        FDB     CmdTroff
        FDB     CmdLine
        FDB     CmdPCls
        FDB     CmdPset
        FDB     CmdPReset
        FDB     CmdScreen
        FDB     CmdPClear
        FDB     CmdColor
        FDB     CmdCircle
        FDB     CmdPaint
        FDB     CmdGet
        FDB     CmdPut
        FDB     GrDraw
        FDB     CmdPcopy
        FDB     CmdPmode
        FDB     CmdPlay
        FDB     CmdDload
        FDB     CmdRenum

;
; Basic keyword table (functions).
;

BasFunctionWords
        FCS     /SGN/
        FCS     /INT/
        FCS     /ABS/
        FCS     /POS/
        FCS     /RND/
        FCS     /SQR/
        FCS     /LOG/
        FCS     /EXP/
        FCS     /SIN/
        FCS     /COS/
        FCS     /TAN/
        FCS     /ATN/
        FCS     /PEEK/
        FCS     /LEN/
        FCS     /STR$/
        FCS     /VAL/
        FCS     /ASC/
        FCS     /CHR$/
        FCS     /EOF/
        FCS     /JOYSTK/
        FCS     /FIX/
        FCS     /HEX$/
        FCS     /LEFT$/
        FCS     /RIGHT$/
        FCS     /MID$/
        FCS     /POINT/
        FCS     /INKEY$/
        FCS     /MEM/
        FCS     /VARPTR/
        FCS     /INSTR/
        FCS     /TIMER/
        FCS     /PPOINT/
        FCS     /STRING$/
        FCS     /USR/
        
;
; Keyword dispatch table (functions).
;
BasFunctionDisp
	FDB     CmdSGN
        FDB     CmdINT
        FDB     CmdABS
        FDB     CmdPOS
        FDB     CmdRND
        FDB     CmdSQR
        FDB     CmdLOG
        FDB     CmdEXP
        FDB     CmdSIN
        FDB     CmdCOS
        FDB     CmdTAN
        FDB     CmdATN
        FDB     CmdPeek
        FDB     CmdLEN
        FDB     CmdSTRS
        FDB     CmdVAL
        FDB     CmdASC
        FDB     CmdCHRS
        FDB     CmdEOF
        FDB     CmdJoystk
        FDB     CmdFIX
        FDB     CmdHexS
        FDB     CmdLeftS
        FDB     CmdRightS
        FDB     CmdMidS
        FDB     CmdPoint
        FDB     CmdINKEYS
        FDB     CmdMEM
        FDB     CmdVarptr
        FDB     CmdInstr
        FDB     CmdTimer
        FDB     CmdPPoint
        FDB     CmdStringS
        FDB     CmdUSR

;
; this table contains precedences and dispatch addresses for arithmetic
; and logical operators - the negation operators do not act on two operands
; so they are not listed in this table. they are treated separately in the
; expression evaluation routine. they are:
; unary negation (-), precedence &7d and logical negation (not), precedence $5a
; the relational operators < > = are also not listed, precedence $64.
; a precedence value of zero indicates end of expression or parentheses
;
BasOperatorTable   
	FCB     $79
        FDB     CmdPlus
	FCB	$79
        FDB     CmdMinus
        FCB     $7B
        FDB     CmdMultiply
	FCB	$7B
        FDB     CmdDivide
        FCB     $7F
	FDB	CmdExponet
        FCB     $50
        FDB     CmdAND
        FCB     $46
	FDB	CmdOR

BasErrorCodeTable:
D82A9   FCC     "NF"		
	FCC	"SN"		 
	FCC	"RG"
	FCC	"OD"
	FCC	"FC"
	FCC	"OV"
	FCC	"OM"
	FCC	"UL"
	FCC	"BS"
	FCC	"DD"
	FCC	"/0"
	FCC	"ID"
	FCC	"TM"
	FCC	"OS"
	FCC	"LS"
	FCC	"ST"
	FCC	"CN"
	FCC	"UF"
	FCC	"FD"
	FCC	"AO"
	FCC	"DN"
	FCC	"IO"
	FCC	"FM"
	FCC	"NO"
	FCC	"IE"
	FCC	"DS"
	FCC	"NE"

; offets into above table......
ErrNF	EQU	$00
ErrSN	EQU	$02		 
ErrRG	EQU	$04
ErrOD	EQU	$06
ErrFC	EQU	$08
ErrOV	EQU	$0A	
ErrOM	EQU	$0C
ErrUL	EQU	$0E
ErrBS	EQU	$10
ErrDD	EQU	$12
ErrD0	EQU	$14
ErrID	EQU	$16
ErrTM	EQU	$18
ErrOS	EQU	$1A
ErrLS	EQU	$1C
ErrST	EQU	$1E
ErrCN	EQU	$20
ErrUF	EQU	$22
ErrFD	EQU	$24
ErrAO	EQU	$26
ErrDN	EQU	$28
ErrIO	EQU	$2A
ErrFM	EQU	$2C
ErrNO	EQU	$2E
ErrIE	EQU	$30
ErrDS	EQU	$32
ErrNE	EQU	$34


MessError
	FCC	/ ERROR/
	FCB     $00
MessIn
        FCC     / IN /
	FCB     $00

MessOK
	FCB     $0D
        FCC     /OK/
        FCB     $0D,$00

MessBreak
        FCB     $0D
        FCC     /BREAK/
        FCB     $00

; search the stack for `gosub/return' or `for/next' data.
; the `for/next' index variable descriptor address being
; sought is stored in vardes. each block of for/next data is 18
; bytes with a $80 leader byte and the gosub/return data is 5 bytes
; with an $a6 leader byte.  the first non "for/next" data
; is considered `gosub/return'

L82F7   LEAX    4,S			; point x to 3rd address on stack

L82F9   LDB     #$12			; $12 bytes on stack for each for loop	
        STX     <BasTempPtr		; save pointer
        LDA     ,X			; get first byte
        SUBA    #$80			; check for type of stack jump found
        BNE     L8318			; branch if not for/next

        LDX     1,X			; get index variable descriptor pointer
        STX     <BasTempPtr1		; save it
        LDX     <BasTempVarDesc		; get index var being searched for
        BEQ     L8314			; branch if default index variable, use var on stack
	
        CMPX    <BasTempPtr1		; does the stack indexed variable match the one being searched for?
        BEQ     L8318			; yes!
 
        LDX     <BasTempPtr		; restore initial pointer 	
        ABX				; add $12 bytes to it 
        BRA     L82F9			; look for next block of data

L8314   LDX     <BasTempPtr1		; get 1st index var found and 
        STX     <BasTempVarDesc		; save it as 'next' index		
L8318   LDX     <BasTempPtr		; point X to start of for/next data
        TSTA				; set zero flag for for/next data
        RTS

; check memory space for new top of arrays, and move array data

BasChkArrSpaceMv:
L831C   BSR     L8335			; D= new bottom of free ram, is there room for stack?

; Move bytes from Eval43 (X) to Eval41 (U) until X= Eval47
; save final value in Eval45

L831E   LDU     <Eval41			; point U to destination address (in Eval 41).
        LEAU    1,U			; add 1 to compensate for first phsu
        LDX     <Eval43			; point X at source address (in Eval 43).
        LEAX    1,X			; add 1 compensate for first lda ,-x

L8326   LDA     ,-X			; get a byte from source	
        PSHU    A			; save in destination
        CMPX    <Eval47			; done all yet?
        BNE     L8326			; nope : keep moving
        STU     <Eval45			; save result.
L8330   RTS

; check to see if there is room to store 2xB bytes in RAM

BasChkB2Free:
L8331   CLRA				; Convert B to a word.....
        ASLB				; and * by 2
        ADDD    <BasVarEnd		; end of variables and programs
L8335   ADDD    #StackBuf		; add stack buffer size, is there room for stack? 
        BCS     BasOMError 		; nope : error
	
        STS     <BasBotStack		; save stack pointer for compare
        CMPD    <BasBotStack		; are we going to be below the current stack?
        BCS     L8330			; yes : no error

BasOMError:
L8342   LDB     #ErrOM			; OM error code

SysErr:
L8344   JSR     VectUserError 		; hook user error handler
        JSR     VectSysError 		; hook system error handler
        JSR     >BasicCassOff 		; turn off tape
        JSR     >SndDisable 		; disable sound output
        JSR     >BasResetStack 		; reset basic stack, string stack, continue pointer
        CLR     <TextDevN		; set device number to screen
        JSR     >L90A5			; send a CR to screen
        JSR     >TextOutQuestion 	; send a '?' to screen
        LDX     #BasErrorCodeTable 	; point to code table

SysErr2:
        ABX				; adjust for error code in B
        BSR     L839E			; get char from X, send to screen 			
        BSR     L839E			; get char from X, send to screen
        LDX     #MessError-1		; Point to ' error?' message
L8366   JSR     >TextOutString 		; send it to screen
        LDA     <BasCurrentLine		; get current line number
        INCA				; test for direct mode (-1)
        BEQ     BasCmdMode 		; yes, just enter command mode
        JSR     >L9573			; print ' in ' linenumber.

;
; Print OK and enter command mode
;

BasCmdMode:
L8371   JSR     >L90A5			; send CR to screen
        LDX     #MessOK			; Point to 'OK' message
        JSR     >TextOutString 		; send to screen

;
; Enter command mode
;

BasCmdMode2:        
L837A   JSR     >LB5C6			; get a line of input (from console)
        LDU     #$FFFF                  ; Set current line  to -1, to flag command mode
        STU     <BasCurrentLine
        BCS     BasCmdMode2		; loop again if terminated by break
        
        TST     <CasEOFFlag             ; End of file ?
        LBNE    LB6FD			; branch if buffer empty, close input file
        
        STX     <BasAddrSigByte		; save X as current input pointer, this will enable direct mode.
					; the line just entered will be interpreted
        JSR     <BasChrGet              ; read next basic byte
        BEQ     BasCmdMode2		; no line input, get another!
        
        BCS     L83A3			; branch if numeric, there was a line number entered, add to program.
        
        LDB     #ErrDS			; direct statement in file error			
        TST     <TextDevN		; check for input from console
        BNE     SysErr  		; Not console : Error direct statement in file.
        JSR     >L8F67			; Go and crunch (tokenize) the line
        JMP     >L84D6			; Go execute the statement if in direct mode.

L839E   LDA     ,X+			; get a character
        JMP     >L90FA			; send to console output
	
; Take a line from the line input program and add it to the basic program.

L83A3   JSR     >BasGetLineNo 		; convert line no to binary
L83A6   LDX     <BasTempLine		; get converted line no			
        STX     BasLinInpHead 		; store in input line header
        JSR     >L8F67			; go tokenize the line
	
        STB     <BasGenCount		; save the line length	
        BSR     BasFindLineNo 		; find out where to insert the line	
        BCS     L83C6			; branch if new line (does not exist).
	
        LDD     <Eval47			; get absolute address of line number	
        SUBD    ,X			; subtract the address of the next line no 
        ADDD    <BasVarSimpleAddr	; add to current length of program	
        STD     <BasVarSimpleAddr	; that will remove length of current line no from program.
        LDU     ,X			; point U to address of next line no

; delete old line from basic program
	
L83BE   PULU    A			; get a byte from old location
        STA     ,X+			; store in new location
        CMPX    <BasVarSimpleAddr	; end of program ?
        BNE     L83BE			; nope : keep going
	
L83C6   LDA     BasLinInpBuff 		; check to see if there is a line in the buffer
        BEQ     L83E7			; nope !
	
        LDD     <BasVarSimpleAddr	; save current end of program in Eval43		
        STD     <Eval43
        ADDB    <BasGenCount		; Add the length of the crunched line				
        ADCA    #$00			; propogate carry to MSB
        STD     <Eval41			; save new end in Eval41
        JSR     >BasChkArrSpaceMv 	; check space available and move vars up
        LDU     #BasLinInpHead-2	; Point U at line to be inserted
	
L83DB   PULU    A			; transfer bytes of line
        STA     ,X+
        CMPX    <Eval45			; done all bytes?
        BNE     L83DB			; nope, keep going
	
        LDX     <Eval41			; get and save new end of program
        STX     <BasVarSimpleAddr
	
L83E7   BSR     BasVect1 		; Reset input pointer, clear variables and init
        BSR     BasVect2 		; Adjust start of next line addr
        BRA     BasCmdMode2		; re-enter input loop
	
; calculate the start of line addresses for the basic program.
; This searches through the text of the basic program and re-generates the
; line linked list.

BasVect2:
L83ED   LDX     <BasStartProg		; get the address of the start of the program
L83EF   LDD     ,X			; get the link address
        BEQ     L8414			; zero : end of program, finished
        LEAU    4,X			; point U to beginning of this line's text (past line no & link)
L83F5   LDA     ,U+			; get a byte from line
        BNE     L83F5			; End of line? (zero byte). nope loop again
	
        STU     ,X			; save link to next line in current line
        LDX     ,X			; follow link to next line
        BRA     L83EF			; loop to scan next line

; Find a line number in the basic program (in BasTempLine), return with
; carry set if line not found

BasFindLineNo:
L83FF   LDD     <BasTempLine		; get line to search for
        LDX     <BasStartProg		; point to start of program
L8403   LDU     ,X			; get address of next line in U 
        BEQ     L8410			; end of program if zero, exit
	
        CMPD    2,X			; compare searched line no with this line's number
        BLS     L8412			; Found it : exit
        LDX     ,X			; follow link to next line
        BRA     L8403			; continue search

L8410   ORCC    #FlagCarry		; flag line no not found
L8412   STX     <Eval47			; save line pointer
L8414   RTS				; return

CmdNew:
L8415   BNE     L8412			; branch if param given, causes ?SN error
BasNew:
L8417   LDX     <BasStartProg		; get start of prog in X	
        CLR     ,X+			; clear first line link address, to flag end of program
        CLR     ,X+
        STX     <BasVarSimpleAddr	; setup simple var pointer after end of prog
BasVect1:
L841F   LDX     <BasStartProg		; get start of prog in X	
        JSR     >BasSetProgPtrX 	; put pointer one before start of basic
        
; Erase all variables
; $8424
BasEraseVars   
	JSR     VectResetBasMem 	; call trap vector
        LDX     <AddrFWareRamTop        ; get top of RAM used by basic
        STX     <BasVarStrTop           ; set top of strings space this 
        JSR     >CmdRestore 		; do a RESTORE
        
        LDX     <BasVarSimpleAddr	; Get start of simple vars,
        STX     <BasVarArrayAddr	; set start of arrays to be the same
        STX     <BasVarEnd		; and end of arrays, so no vars
BasResetStack:
L8434   LDX     #BasStrDescStack	; restet string stack pointer to bottom
        STX     <BasStrFirstFreeTemp	; of string stack
        LDX     ,S			; get return address off stack
        LDS     <AddrStack		; reset stack pointer
        CLR     ,-S			; put zero byte on stack
        CLR     <BasOldInputPtr         ; reset 'CONT' address
        CLR     <BasOldInputPtr+1                       
        CLR     <BasDisArraySearch      ; Clear the array disable flag        
        JMP     ,X                      ; return to caller

; FOR command
;
; the for command will store 18 bytes on the stack for
; each for-next loop which is being processed. these
; bytes are defined as follows: 
;	0- 	$80 (for flag);
;   	1,2	index variable descriptor pointer 
;	3-7	floating point value of step;
;     	8	step direction: $ff if negative, 0 if zero, 1 if positive
;  	9-13	floating point value of `to' parameter
; 	14,15	current line number
;	16,17	ram address of the end
CmdFor:
L8448   LDA     #$80			; save disable array search in BasDisArraySearch			
        STA     <BasDisArraySearch
        JSR     >CmdLet 		; set index variable to initial value
        JSR     >L82F7			; search stack for for/next data
        LEAS    2,S			; purge the return address from the stack
        BNE     L845A			; branch if index variable not already used
        LDX     <BasTempPtr		; get address+18 of matched for/next data
        
; move the stack pointer to the beginning of the matched `for/next' data so the new data will
; overlay the old data. this will also destroy all of the `return' and `for/next' data below
; this point on the stack			
	LEAS    B,X			
	
L845A   LDB     #$09			; check for room for 18 bytes
        JSR     >BasChkB2Free 		
        JSR     >L861B			; get address of end of subline in X
        LDD     <BasCurrentLine		; save line address and number on stack
        PSHS    D,X
	
        LDB     #DTokTO			; Token for TO	
        JSR     >VarCKChar 		; Syntax check for TO
        JSR     >VarGetExprCC 		; TM error if index set to string
        JSR     >L8872			; evaluate expression
        LDB     <FP0SGN			; get floating point mantissa sign
        ORB     #$7F			; form a mask to save data bits of high order mantissa
        ANDB    <FPA0			; put the mantissa sign in bit 7 of high order mantissa	
        STB     <FPA0			; save the packed high order mantissa
        LDY     #L8480			; setup return address in Y
        JMP     >L891B			; Push Floating point accumulator onto stack

L8480   LDX     #FPOnePointZero		; load address of FP 1.00 into X (default step value)			
        JSR     >XtoFPA0			; move (x) to floating point accumulator
        JSR     <BasChrGetCurr		; get current char from basic
        CMPA    #DTokSTEP		; STEP token?
        BNE     L8491			; no, skip
	
        JSR     <BasChrGet		; skip over step token 	
        JSR     >L8872			; Evaluate numeric expression (step value)
L8491   JSR     >TestFPA0			; check status of FPA
        JSR     >L8917			; Save FPA and status on stack
        LDD     <BasTempVarDesc		; Get var descriptor pointer for the step variable
        PSHS    D			; save on stack	
        LDA     #$80			; get FOR flag and save on stack
        PSHS    A

; Main command interpretation loop

BasRun:
L849F   JSR     VectGetNextCmd 		; call get next command hook
        ANDCC   #IntsEnable		; enable IRQ,FIRQ
        BSR     BasPollKeyboard 	; scan keyboard
        LDX     <BasAddrSigByte		; get basic input pointer
        STX     <BasDirectTextPtr	; save it
        LDA     ,X+			; get a byte from basic, move pointer
        BEQ     L84B5			; End of line? Yep skip it
	
        CMPA    #':'			; colon ?
        BEQ     L84D6			; Yep, deal with it
JSNerror
	JMP     >BasSNError 		; Else Syntax error

L84B5   LDA     ,X++			; get MSB of address of next line
        STA     <BasBreakFlag		; save in stop/end flag 
					; cause a stop if addr < $8000
					; cause and end if addr > $8000
        LBEQ    L8545			; branch to stop
        LDD     ,X+			; get current line number
        STD     <BasCurrentLine		; save current line no
        STX     <BasAddrSigByte		; save address of first byte of line
        LDA     <BasTronFlag		; get TRON flag	
        BEQ     L84D6			; not tracing skip
        
	LDA     #'['			; print '['
        JSR     >TextOutChar 	
        LDA     <BasCurrentLine		; get line number (other half still in B).
        JSR     >TextOutNum16 		; print it
        LDA     #']'			; print closing ']'
        JSR     >TextOutChar 
	
L84D6   JSR     <BasChrGet		; get a char from basic	
        BSR     L84DC			; go execute command
;84DA
BasBRARun:
        BRA     BasRun  		; and then go back to main loop

L84DC   BEQ     L851A			; return if end of line		
        JSR     VectCmdInterp 		; call basic interpret ram hook
        TSTA				; check for token, bit 7 set -ve
        LBPL    CmdLet  		; no token do a LET by default (MS basic default).
        CMPA    #DTokRENUM              ; beyond last command token handled by us?
        BHI     L84F5                   ; yep : pass it on
        LDX     BasAddrCmdDisp		; point to dispatch table
BasDoDipatch:
        ASLA                            ; calculate offset in table of command handler address
        TFR     A,B                     ; add to base of table
        ABX
        JSR     <BasChrGet              ; get next character from basic
L84F3   JMP     [,X]                    ; Jump to command handler

L84F5   CMPA    #$FF			; Check for secondary token
        BEQ     L8501			; Yep : deal with it
        CMPA    #DTokLastC		; higher than last command?
        BLS     JSNerror		; no. error 	
        JMP     [BasAddrDskCmdDisp] 	; yes, jump to secondary basic command handler (normally dos).

L8501   JSR     <BasChrGet		; skip over secondary token marker
        CMPA    #DTokMIDS		; Token is MID$ ?			
        LBEQ    DoMIDS			; yes, jump to it
	
        CMPA    #DTokTIMER		; Token is TIMER
        LBEQ    L9D51			; yes, jump to it
	
        JSR     VectAccessScreen 	; call vector	
        BRA     JSNerror		; generate SN error

CmdRestore:
L8514   LDX     <BasStartProg		; point at beginning of program -1
        LEAX    -1,X
L8518   STX     <BasVarDataAddr		; set basic data pointer to beginning
L851A   RTS				; return

BasPollKeyboard:
L851B   JSR     >BasicKbdIn 		; Get a keystroke, if any?
        BEQ     L852A			; return if no key pressed
L8520   CMPA    #$03			; check for break?
        BEQ     CmdStop 		; yes, break program.
        CMPA    #$13			; check for pause? (CTRL-S)
        BEQ     TextWaitKey 		; yes wait.....
        STA     <TextLastKey		; save pressed key
L852A   RTS				; return

TextWaitKey:
L852B   JSR     >BasicKbdIn 		; read keyboard
        BEQ     TextWaitKey 		; keep waiting if no key pressed
        BRA     L8520			; re-enter poll loop

CmdEnd:
L8532   JSR     >LB65C			; Close files
        JSR     <BasChrGetCurr		; get current input char
        BRA     L853B			

CmdStop:
L8539   ORCC    #FlagCarry              ; set carry flag
L853B   BNE     L8570			
        LDX     <BasAddrSigByte         ; Save current pos of basic input pointer
        STX     <BasDirectTextPtr
L8541   ROR     <BasBreakFlag           ; Rotate carry into break flag
        LEAS    2,S                     ; drop return address
L8545   LDX     <BasCurrentLine         ; Get current line no
        CMPX    #$FFFF                  ; Direct mode ?
        BEQ     L8552                   ; Yep : skip ahead
        
        STX     <BasContLine            ; Save current line for CONT
        LDX     <BasDirectTextPtr       ; save Basic text pointer
        STX     <BasOldInputPtr

L8552   CLR     <TextDevN               ; Make console default device no
        LDX     #MessBreak-1            ; point to break message
        TST     <BasBreakFlag           ; branch to main loop of basic if end
        LBPL    BasCmdMode L8371                        
        JMP     >L8366                  ; basic's main loop if stop

CmdCont:
L8560   BNE     L8570			; return if argument given
        LDB     #ErrCN			; can't continue error			
        LDX     <BasOldInputPtr		; get continue address (input pointer)
        LBEQ    SysErr  		; CN error if continue address is 0
        STX     <BasAddrSigByte		; reset basic's input pointer
        LDX     <BasContLine		; get continue line number
        STX     <BasCurrentLine		; set current line number to continue line no
L8570   RTS

CmdClear:
L8571   BEQ     L859F			; Check for parameters, nope skip on
        JSR     >L8B23			; get string space to clear
        PSHS    D			; save on stack
        LDX     <AddrFWareRamTop	; Get top of RAM as default cleared area
        JSR     <BasChrGetCurr		; get next character
        BEQ     L858A			; none, no top of basic ram specified
        
        JSR     >VarCKComma 		; Check for comma, error if not
        JSR     >VarGet16Bit 		; get ramtop spec
        LEAX    -1,X			; decrement by 1
        CMPX    <AddrRamTop		; Greater than physical RAM?
        BHI     L85A2			; yep : OM Error?
	
L858A   TFR     X,D			; get RamTop into D
        SUBD    ,S++			; adjust for string space
        BCS     L85A2			; Generate OM error if < 0 bytes
	
        TFR     D,U			; U = bottom of cleared space
        SUBD    #StackBuf		; subtract stack buffer
        BCS     L85A2			; Generate OM error if < 0 bytes
        SUBD    <BasVarSimpleAddr	; subtract start of variables
        BCS     L85A2			; Generate OM error if < 0 bytes
	
        STU     <AddrStack              ; Address of stack base
        STX     <AddrFWareRamTop        ; Address of top of RAM used by basic
L859F   JMP     >BasEraseVars

L85A2   JMP     >BasOMError 		; generate out of memory error

CmdRun:
L85A5   JSR     VectRunLink 		; call ram vector
        JSR     >InitSndGraph		; Init sound and graphics constants	
        JSR     >LB65C			; close any open files
        JSR     <BasChrGetCurr		; get current input character
        LBEQ    BasVect1 		; if no line no reset vars etc
        JSR     >BasEraseVars			
        BRA     L85D2			; skip ahead

CmdGo:
L85B9   TFR     A,B			; save input char in b
L85BB   JSR     <BasChrGet		; get next char
        CMPB    #DTokTO			; is it a GOTO?
        BEQ     L85D7			; yes : deal with it
        CMPB    #DTokSUB		; is it a GOSUB?
        BNE     L860A			; no error
	
        LDB     #$03			; Check we have 3*2 bytes free (on stack)
        JSR     >BasChkB2Free 
        LDU     <BasAddrSigByte		; get current input pointer	
        LDX     <BasCurrentLine		; get current line no
        LDA     #DTokSUB		; sub token
        PSHS    A,X,U			; push basic subroutine return address/pointer
	
L85D2   BSR     L85D7			; Do a 'goto' 
        JMP     >BasRun 		; jump back to basic's main loop

L85D7   JSR     <BasChrGetCurr		; get current input char
        JSR     >BasGetLineNo 		; get line number from program
        BSR     L861E			; advance pointer to end of current line
	
        LEAX    1,X			; point to start of next line
        LDD     BasTempLine		; get the destination line no	
        CMPD    <BasCurrentLine		; is it the current line?
        BHI     L85E9			; if destination is higher than current, search from current
	
BasSkipLineNo:
        LDX     <BasStartProg		; otherwise start from begiining of program
L85E9   JSR     >L8403			; go find line number
        BCS     BasULError		; undefined line number
BasSetProgPtrX:
L85EE   LEAX    -1,X			; move to just before start of line
        STX     <BasAddrSigByte		; set basic pointer to destination line
L85F2   RTS				; goto it!

CmdReturn:
L85F3   BNE     L85F2			; error : argument given
        LDA     #$FF			; put an illgal value in vardes, 
        STA     <BasTempVarDesc		; so ignore for/next data on stack.
        JSR     >L82F7			; check for return data on stack
        TFR     X,S			; reset stack pointer, purge 2 return addresses
        CMPA    #(DTokSUB-DTokFirstC)	; Sub token			
        BEQ     L860D			; Yep, do return 
D8602   LDB     #ErrRG			; 'return without gosub' error

        FCB     Skip2			; skip 2 bytes

BasULError   
	LDB     #ErrUL			; 'undefined line' error
        JMP     >SysErr 		; jump to error handler	

L860A   JMP     >BasSNError 		; Syntax error jump

L860D   PULS    A,X,U			; restore text pointer and line number 
        STX     <BasCurrentLine		; restore line no
        STU     <BasAddrSigByte		; and text pointer
CmdData:
L8613   BSR     L861B			; move input pointer to end of line or subline
        FCB	Skip2			; skip 2 bytes
CmdREM
L8616	BSR	L861E			; move basic's pointer to end of line
        STX     <BasAddrSigByte
L861A   RTS

L861B   LDB     #':'			; colon : subline terminator		

        FCB     Skip1LD			; skip one byte

L861E   CLRB				; zero line delimiter
        STB     <BasDelim1		; save primary terminator
D8621   CLRB					
        LDX     <BasAddrSigByte		; Get basics input pointer into X
L8624   TFR     B,A			; change terminator character
        LDB     <BasDelim1
        STA     <BasDelim1
L862A   LDA     ,X			; get next input character
        BEQ     L861A			; return if end of line (0)
        PSHS    B			; save terminator on stack
        CMPA    ,S+			; compare to input character
        BEQ     L861A			; return if equal to terminator
        LEAX    1,X			; increment input pointer
        CMPA    #'"'			; check for double quotes
        BEQ     L8624			; branch if ", toggle terminator character 
	
        INCA				; Check for $FF (secondary token marker)
        BNE     L863F			; branch if not
        LEAX    1,X			; increment input pointer
L863F   CMPA    #DTokIF+1		; token for if ? (DTokIf+1, because of INCA above).	
        BNE     L862A			; no, loop again, gext next char
	
        INC     <BasIfCount		; increment nested if counter
        BRA     L862A			; loop again

CmdIF:
L8647   JSR     >L8872			; evaluate numeric expression
        JSR     <BasChrGetCurr		; get current input char
        CMPA    #DTokGO			; treat go like then			
        BEQ     L8655			; go : skip ahead
        LDB     #DTokTHEN		; token for then
        JSR     >VarCKChar 		; syntax check A
	
L8655   LDA     <FP0EXP			; check for true / false (fp0 exponent=0)
        BNE     L866C			; condition true do THEN clause.....
        CLR     <BasIfCount		; clear flag to keep track of which else in nested if
L865B   BSR     CmdData 		; move basic's pointer to end of subline	

        TSTA				; test if end of line or end of subline
        BEQ     L861A			; end of line : return
	
        JSR     <BasChrGet		; get next character
        CMPA    #DTokELSE		; is it an ELSE token?
        BNE     L865B			; no : ignore
        DEC     <BasIfCount		; dec else counter
        BPL     L865B			; look for next else, if counter <>0
	
        JSR     <BasChrGet		; get character from bas	ic	
L866C   JSR     <BasChrGetCurr		; and get it
        LBCS    L85D7			; branch to GOTO if numeric 
        JMP     >L84DC			; else return to main interpritation loop
	
CmdON:
L8675   JSR     >VarGet8Bit 		; evaluate expression, 8 bit result
        LDB     #DTokGO			; token for GO			
        JSR     >VarCKChar 		; Syntax check for GO
        PSHS    A			; save new token TO or SUB
        CMPA    #DTokSUB		; token for SUB
        BEQ     L8687			; yes 
        CMPA    #DTokTO			; token for TO
L8685   BNE     L860A			; no : syntax error if neither

L8687   DEC     <FPA0+3			; Decrement byte of the mantissa, this is the argument byte of ON
        BNE     L8690			; branch if not at correct line number (in line number list)
		
        PULS    B			; get back the token following GO 
        JMP     >L85BB			; either GOTO or GOSUB as needed

L8690   JSR     <BasChrGet		; get a character from basic
        BSR     BasGetLineNo 		; convert next line number in list to binary
        CMPA    #','			; followed by a comma?
        BEQ     L8687			; yes loop again, check if correct line no.
        PULS    B,PC			; no : fall through to next command, on expression 
					; outside range of count of list of line numbers

BasGetLineNo:
L869A   LDX     <DBZero	; default line no of zero	
        STX     BasTempLine
	
L869E   BCC     L8704			; return if not numeric 
        SUBA    #'0'			; convert number to binary
        STA     <BasDelim1		; save digit
        LDD     BasTempLine		; get accumulated line no value
        CMPA    #$18			; highest line no is $F9FF (63999)
					; (24*256+255)*10+9
        BHI     L8685
        ASLB				; D=D*4
        ROLA
        ASLB
        ROLA
        ADDD    BasTempLine		; Add again, effectively *5	
        ASLB				; Multiply by 2, *10
        ROLA
        ADDB    <BasDelim1		; add next digit
        ADCA    #$00			; propogte carry
        STD     BasTempLine
        JSR     <BasChrGet		; get next char
        BRA     L869E			; loop for next

CmdLet:
L86BC   JSR     >VarGetVar 		; Find target variable descriptor
        STX     <BasTempVarDesc		; save discriptor address
        LDB     #DTokEQUAL		; token for =
        JSR     >VarCKChar 		; syntax check it
	
        LDA     <BasVarType		; get variable type, and save on stack
        PSHS    A
        JSR     >VarGetStr 		; evaluate expression on RHS 
        PULS    A			; restore target type
        RORA				; set carry if target is string 
        JSR     >L8879			; type check, TM error if not both same type
	
        LBEQ    L93DE			; Go put BasVarFPAcc1 into variable if numeric

; move a string whose descriptor is located at BasVarFPAcc1+2 into the string space. transfer the
; descriptor address to the address in vardes don't move the string if it is already in the
; string space. remove descriptor from string stack if it is last one on the stack	

L86D7   JSR     VectAssignStr 		; Call assign string RAM hook
        LDX     <BasVarAssign16		; point X at descriptor of replacement string
        LDD     <AddrStack		; load D with address of string space
        CMPD    2,X			; Is the string in string space already?
        BCC     L86F4			; branch if not
	
        CMPX    <BasVarSimpleAddr	; compare descriptor address to start of vars
        BCS     L86F4			; branch if descriptor address not in vars
	
        LDB     ,X			; get length of replacement string
        JSR     >L8C50			; reserve B bytes of string space
	
        LDX     <Eval4D			; get descriptor address back
        JSR     >L8D89			; move string into string sapce
        LDX     #StrDesc		; point X at temp string descriptor
L86F4   STX     <Eval4D			; save it
        JSR     >L8DBB			; remove string descriptor if last on string stack
        LDU     <Eval4D			; point U at replacement descriptor address
        LDX     <BasTempVarDesc		; get descriptor target address
        PULU    D,Y			; get length and start of replacement string
        STA     ,X			; save length & start in target descriptor
        STY     2,X
L8704   RTS

MessRedo
        FCC     /?REDO/			; Redo message zero terminated.
        FCB     $0D
        FCB     $00

L870C   LDB     #ErrFD			; Bad file data error
        TST     <TextDevN		; is device number screen?
        BEQ     L8715			; yes....skip on
	
L8712   JMP     >SysErr 		; no : generate error

L8715   LDA     <BasInputFlag		; get input flag
        BEQ     L8720			; branch if we are in input....
	
        LDX     <BasVarDataLine		; get error line no
        STX     <BasCurrentLine		; set current line to error line
        JMP     >BasSNError 		; generate syntax error

L8720   LDX     #MessRedo-1		; point at redo message
        JSR     >TextOutString 		; output to screen
        LDX     <BasDirectTextPtr	; get absolute address of input pointer
        STX     <BasAddrSigByte		; and restore it
        RTS

CmdInput:
L872B   JSR     >BasChkDirect		; check if direct mode, ID error if so 		
        BSR     L8733			; go get some input	
        CLR     <TextDevN		; set device no to screen
        RTS				; return

L8733   CMPA    #'#'			; check for '#' for device no
        BNE     L8740			; nope skip on
	
        JSR     >LB7D7			; check syntax and get device no
        JSR     >LB623			; check for open file
        JSR     >VarCKComma 		; check for comma
	
L8740   CMPA    #'"'			; check for prompt string delimiter (double quote)
        BNE     L874F			; no prompt string, skip on
	
        JSR     >L8975			; put prompt string on string stack
        LDB     #';'			; semicolon
        JSR     >VarCKChar 		; syntax check for semicolon
        JSR     >L90E8			; print message to console
	
L874F   LDX     #BasLinInpBuff 		; point at basic input buffer
        CLR     ,X			; make string zero length
        TST     <TextDevN		; check for console input
        BNE     CmdReadFromX 		; not screen, go read from file...
        BSR     L8760			; go read a line of input from screen
	
        LDB     #','			; insert a comma at end of input
        STB     ,X
        BRA     CmdReadFromX 		

L8760   JSR     >TextOutQuestion 	; output a ? to screen
        JSR     >TextOutSpace 		; and a space
L8766   JSR     >LB5C6			; go read a basic line
        BCC     L8770			; branch if ended with enter key
        LEAS    4,S			; purge two return addresses
        JMP     >L8541			; go do a 'STOP' if break key ended input

L8770   LDB     #ErrIE			; input past end error			
        TST     <CasEOFFlag		; are we at end of file?
        BNE     L8712			; yes : generate error 
        RTS

CmdRead:
L8777   LDX     <BasVarDataAddr		; get read start address
        
	FCB	Skip1LD			; skip one byte
CmdReadFromX:
L877A   CLRA				; input entry point, input flag = 0

        STA     <BasInputFlag		; set input flag 0 = input, <>0 = read
        STX     <TextKbdBuffAddr	; save input/read buffer address	

L877F   JSR     >VarGetVar 		; evaluate variable
        STX     <BasTempVarDesc		; save variable's descriptor
        LDX     <BasAddrSigByte		; get input pointer
        STX     BasTempLine		; and save it
        LDX     <TextKbdBuffAddr	; get read address from read/input buffer pointer
        LDA     ,X			; get a character from input stream
        BNE     L879A			; branch if not EOL
	
        LDA     <BasInputFlag		; get input / read flag
        BNE     L87EA			; branch if 'read'
	
        JSR     VectReReqestIn		; hook into ram input vector 		
        JSR     >TextOutQuestion 	; output a question mark if input
        BSR     L8760			; fill buffer from console
	
L879A   STX     <BasAddrSigByte		; reset basic's input pointer
        JSR     <BasChrGet		; get a character from basic
        LDB     <BasVarType		; check variable type
        BEQ     L87C9			; branch if numeric
	
        LDX     <BasAddrSigByte		; get basic input pointer
        STA     <BasDelim1		; save current input character
        CMPA    #'"'			; check for string delimiter (double quote)
        BEQ     L87BC			; yes : branch
	
        LEAX    -1,X			; back up pointer
        CLRA				; zero = EOL character
        STA     <BasDelim1		; save as terminator
        JSR     >SetPRINTParams		; setup print parameters
        TST     <CasIOFlag		; check if cassette input
        BNE     L87BC			; if so branch, use two zeros as delimiter
	
        LDA     #':'			; end of subline character ':' 
        STA     <BasDelim1		; save it in delimiter 1
        LDA     #','			; comma
L87BC   STA     <BasDelim2		; save as delimiter 2
        JSR     >L8C61			; strip a string from the input buffer
        JSR     >L897A			; move input pointer to end of string
        JSR     >L86D7			; put the string into the string space if needed
        BRA     L87CF			; check for another data item

; save a numeric input in 'INPUT' or 'READ'
L87C9   JSR     >L94BD			; convert ascii string to numeric number
        JSR     >L93DE			; pack BasVarFPAcc1 and store in variable descriptor
					; input or read current item
L87CF   JSR     <BasChrGetCurr		; get next character from input
        BEQ     L87D9			; branch if EOL
	
        CMPA    #','			; check for comma
        LBNE    L870C			; Bad file data, error or retry.
	
L87D9   LDX     <BasAddrSigByte		; get current input pointer (used as data pointer)
        STX     <TextKbdBuffAddr	; and save it
        LDX     BasTempLine		; reset input pointer or read statement
        STX     <BasAddrSigByte	
        JSR     <BasChrGetCurr		; get current character
        BEQ     L8806			; branch if EOL exit command
	
        JSR     >VarCKComma 		; check for comma
        BRA     L877F			; get another input or read item.

; search from address in X for the first occorance of token for data
L87EA   STX     <BasAddrSigByte		; reset basic's input pointer
        JSR     >L861B			; search for end of current line or subline
        LEAX    1,X			; move X one past EOL
        TSTA				; check for end of line
        BNE     L87FE			; branch if end of subline
	
        LDB     #ErrOD			; out of data error 
        LDU     ,X++			; get next two characters
        BEQ     L883B			; OD error if end of program (double zero byte terminator).
        LDD     ,X++			; get basic line number
        STD     <BasVarDataLine		; save it in data line number
L87FE   LDA     ,X			; get input character from basic line
        CMPA    #DTokDATA		; data token ?
        BNE     L87EA			; nope : keep looking
        BRA     L879A			; yep : process it

; exit read and input commands
L8806   LDX     <TextKbdBuffAddr	; get data pointer	
        LDB     <BasInputFlag		; check input / read flag
        LBNE    L8518			; save new data pointer if it was a read
        LDA     ,X			; check next character in input buffer
        BEQ     L8818			; return if no more data for input
        LDX     #MessExtraIgnored-1	; point at extra ignored message
        JMP     >TextOutString 		; go print it and exit

L8818   RTS

MessExtraIgnored
        FCC     /?EXTRA IGNORED/
        FCB     $0D
        FCB     $00
	
; NEXT command
CmdNext:
L8829   BNE     L882F			; branch if argument given
        LDX     <DBZero	; X = 0, no argument
        BRA     L8832			


L882F   JSR     >VarGetVar 		; evaluate variable expression
L8832   STX     <BasTempVarDesc		; save variable descriptor
        JSR     >L82F7			; scan for FOR/NEXT data on stack
        BEQ     L883D			; data found : go handle it
	
        LDB     #ErrNF			; next without for error			
L883B   BRA     L8884			; go generate it

L883D   TFR     X,S			; point S to start of FOR/NEXT data
        LEAX    3,X			; point X to FP value of step
        JSR     >XtoFPA0			; copy FP number from (X) to FPA
	
        LDA     8,S			; get step direction	
        STA     <FP0SGN			; update sign of FPA
        LDX     <BasTempVarDesc		; point X at variable descriptor (from next)
        JSR     >AddXtoFPA0		; Add (X) to BasVarFPAcc1 (STEP to INDEX)
        JSR     >L93DE			; pack BasVarFPAcc1 and store in address 
					; stored in BasTempVarDesc
        LEAX    9,S			; point X to terminal value of FOR loop
        JSR     >L9441			; Compare current index to terminal value of FOR
        SUBB    8,S			; accb = 0 if terminal value=current value and step=0 or if
					; step is positive and current value>terminal value or
					; step is negative and current value<terminal value	
        BEQ     L8865			; Branch if FOR/NEXT done
        LDX     14,S			; get line number 
        STX     <BasCurrentLine
        LDX     $10,S			; and basic pointer 
        STX     <BasAddrSigByte		; of statement following for statement
L8862   JMP     >BasRun 		; jump back to command interpreter loop

; for next loop done
L8865   LEAS    $12,S			; pull FOR/NEXT data off stack
        JSR     <BasChrGetCurr		; get next character
        CMPA    #','			; check for a ',' : check for another argment to NEXT
        BNE     L8862			; return if none
        JSR     <BasChrGet		; get next character from basic
        BSR     L882F			; bsr simulates a call to next from command loop
	
; Evaluate a numeric expression	
L8872   BSR     VarGetStr 		; Evaluate expression and do a type check for numeric

VarGetExprCC:
L8874   ANDCC   #~FlagCarry		; Clear carry

        FCB	Skip2TST		; skip two bytes
VarGetExpr:	
L8877   ORCC	#FlagCarry		; Set carry

; string type mode check - if entered at VarGetExpr then valtyp plus is 'TM' error
; numeric type mode check - if entered at VarGetExprCC then valtyp minus is 'TM' error
; if entered at L8879, a type check is done on valtyp
; if entered with carry set, then 'TM' error if numeric
; if entered with carry clear, then 'TM' error if string.

L8879   TST     <BasVarType		; test type flag, do not change carry
        BCS     L8880			; branch if string
        BPL     L8818			; return on plus

        FCB	Skip2			; skip two bytes
L8880   BMI	L8818

BasTMError:
        LDB     #ErrTM			; generate TM error
L8884   JMP     >SysErr 

VarGetStr:
L8887   BSR     L88F7			; backup input pointer
L8889   CLRA				; end of operation precidence flag
     
	FCB	Skip2			; skip 2 bytes
L888B	PSHS	B			; save relational operator flag	   			
        PSHS    A			; save operator precidence flag
	
        LDB     #$01			; check for room for 1 word (count in b)
        JSR     >BasChkB2Free 	
        JSR     >L8954			; go evaluate expression
        CLR     <BasTempRelateFlag	; reset relational operator flag
	
L8899   JSR     <BasChrGetCurr		; get current input byte

; Check for relational operators
L889B   SUBA    #DTokGREATER		; Token for >
        BCS     L88B2			; branch if less than relational operators
	
        CMPA    #(DTokUSING-DTokGREATER)
        BCC     L88B2			; branch if greater than relational operators
	
        CMPA    #(DTokEQUAL-DTokGREATER) ; set carry if operator is '>'
        ROLA				; carry to bit 0
        EORA    <BasTempRelateFlag	; carry set if BasTempRelateFlag = A		
        CMPA    <BasTempRelateFlag
        BCS     L8910			; Branch if syntax error : << == >>
        STA     <BasTempRelateFlag	; bit 0 <, bit 1 =, bit 2 > save desired relational comparison
        JSR     <BasChrGet		; get an input character
        BRA     L889B			; check for another relational operator

L88B2   LDB     <BasTempRelateFlag	; get relational operator flag	
        BNE     L88E9			; branch if relational comparison
	
        LBCC    L8925			; branch if > relational operator
        
	ADDA    #$07			; seven arithmetic / logical operators
        BCC     L8925			; branch if not arithmetic / logical operator
        
	ADCA    <BasVarType		; add carry, numeric flag and modified token number
        LBEQ    L8D55			; branch if valtype = $FF, and A = '+' token
					; concatinate two strings
        ADCA    #$FF			; restore arithmetic / logical operator number
        PSHS    A			; store operator number on stack
        ASLA				; multiply operator no by 2
        ADDA    ,S+			; and add itself (effective multiply by 3)
        LDX     #BasOperatorTable	; point at operator table
        LEAX    A,X			; point to required table entry
	
L88D0   PULS    A			; recover presidence flag from stack
        CMPA    ,X			; compare to current operator
        BCC     L892B			; branch if stack operator > current operator
        BSR     VarGetExprCC 		; 'TM' error if variable type is string
	
; operation being processed is of higher precedence than previous operaion	
L88D8   PSHS    A			; save precidence flag
        BSR     L8905			; push operator routine address and BasVarFPAcc1 onto stack
	
        LDX     <RelPTR			; get arithmetic/logical table pointer for last calculated operation
        PULS    A			; get prescedence flag of previous operation
        BNE     L88FF			; branch if not end of operation
	
        TSTA				; check type of precedence flag
        LBEQ    L8951			; branch if end of expression (or sub-expression).
        BRA     L8934			; evaluate an operation

; Do a relational comparison here
L88E9   ASL     <BasVarType		; bit 7 of vartype to carry	
        ROLB				; shift relational flag left, valtype to bit 0
        BSR     L88F7			; move the input pointer back one
        
	LDX     #D88FC			; move x to point to relational op jump table
        STB     <BasTempRelateFlag	; save relational comparison data
        CLR     <BasVarType		; set var type to numeric
        BRA     L88D0			; perform operation or save stack

L88F7   LDX     <BasAddrSigByte		; get basic's input pointer and
        JMP     >BasSetProgPtrX 	; move it back one

; Relational comparison jump table !
D88FC   FCB     $64			; Relational comparison flag
	FDB	L8A31			; jump address
;        FCB     $8A
;        FCB     $31

L88FF   CMPA    ,X			; compare presidence of last done operation, with next to be done operation
        BCC     L8934			; evaluate expression if lower precidence
        BRA     L88D8			; push operation on stack if higher precidence

; Push operator evaluation address and FPA0 onto stack and evaluate another expression
L8905   LDD     1,X			; get address of operator routine	
        PSHS    D			; save on stack
        BSR     L8913			; push FPA on stack
        LDB     <BasTempRelateFlag	; get relational operator flag	
        LBRA    L888B			; evaluate another expression

L8910   JMP     >BasSNError 		; generate syntax error

; push fpa0 onto the stack.  ,s = exponent
; 1-2,s =high order mantissa  3-4,s = low order mantissa
; 5,s = sign  return with precedence code in acca
L8913   LDB     <FP0SGN			; get sign of mantissa			
        LDA     ,X			; get presidence code to A
L8917   PULS    Y			; get return address from stack into Y
        PSHS    B			; save sign of mantissa (B) on stack
L891B   LDB     <FP0EXP			; get FPA0 into regs		
        LDX     <FPA0
        LDU     <FPA0+2
        PSHS    B,X,U			; push it onto the stack
        JMP     ,Y			; jump to return address

; branch here if non-operator character found - usually `)` or end of line
L8925   LDX     <DBZero	; point X at dummy value (0)
        LDA     ,S+			; get presidence flag from stack
        BEQ     L8951			; branch if end of expression
	
L892B   CMPA    #$64			; check for relational comparison flag
        BEQ     L8932			; and branch if found
        
	JSR     >VarGetExprCC 		; TM error if variable type is string
L8932   STX     <RelPTR			; save pointer top operator routine		
L8934   PULS    B			; get relational operator from stack
        CMPA    #$5A			; check for 'NOT' operator
        BEQ     L8953			; return if 'NOT', no relational comparison
        CMPA    #$7D			; check for negation flag
        BEQ     L8953			; return if negation, no relational comparison

; evaluate an operation. eight bytes will be stored on stack, first six bytes
; are a temporary floating point result then the address of routine which
; will evaluate the operation. the rts at end of routine will vector
; to evaluating routine.	
        LSRB				; rotate valtype bit into carry
        STB     <BasRelateFlag		; flag and save new relflag
        PULS    A,X,U			; pull the FP value off the satack
        STA     <FP1EXP			; save it in FPA1
        STX     <FPA1
        STU     <FPA1+2
        PULS    B			; get mantissa sign 
        STB     <FP1SGN			; save it
        EORB    <FP0SGN
        STB     <ResSGN			; save result in sign byte
L8951   LDB     <FP0EXP			; get exponent of fpa0
L8953   RTS				

L8954   JSR     VectEvaluateExpr 	; hook into ram
        CLR     <BasVarType		; init flag to numeric
        JSR     <BasChrGet		; get an input char
        BCC     L8960			; convert ascii string to floating point
L895D   JMP     >L94BD			; return result in FPA0

; process non numeric first character
L8960   JSR     >CheckAAlpha			; set carry if not alpha
        BCC     L89C1			; branch if alpha character
        
	CMPA    #'.'			; is it '.', decimal point
        BEQ     L895D			; convert ASCII string to floating point
	
        CMPA    #DTokMINUS		; Minus token ?
        BEQ     L89B9			; yes: go process
        
	CMPA    #DTokPLUS		; Plus token?
        BEQ     L8954			; yes: get another character
	
        CMPA    #'"'			; string delimiter '"' ?
        BNE     L897F			; no 
	
L8975   LDX     <BasAddrSigByte		; get current basic pointer
        JSR     >L8C5B			; save string on string stack
L897A   LDX     <CoefPTR		; get address of end of string
        STX     <BasAddrSigByte		; update basic's input pointer
        RTS

L897F   CMPA    #DTokNOT		; 'NOT' token?
        BNE     L8990			; no: skip
	
        LDA     #$5A			; 'NOT' presidence flag
        JSR     >L888B			; process operation following not
        JSR     >INTConv		; convert FPA0 to integer	
        COMA				; 'NOT' the integer
        COMB
        JMP     >VarAssign16Bit2 	; convert back to float in FPA0

L8990   CMPA    #DTokFN			; check for 'FN' token			
        LBEQ    L9CC4			; yep : handle it
	
        CMPA    #'&'			; check for '&' prefix for hex and octal.
        LBEQ    L9C1B			; yep handle it
	
        INCA				; check for $FF token prefix
        BEQ     L89CD			; yes: handle it
	
L899F   BSR     VarCKOpBrac 		; syntax check for '('	
        JSR     >VarGetStr 		; evaluate expression within brackets	
VarCKClBrac:
L89A4   LDB     #')'			; syntax check for ')' close bracket


        FCB	Skip2			; skip two bytes	
VarCKOpBrac:
L89A7   LDB	#'('			; syntax check for '(' open bracket

        FCB	Skip2			; skip two bytes	

VarCKComma:
L89AA   LDB	#','			; syntax check for ',' comma

VarCKChar:
L89AC   CMPB    [BasAddrSigByte] 	; syntax check for character in B
        BNE     BasSNError 		; not same, generate syntax error
        JMP     <BasChrGet		; get next character	

BasSNError:
L89B4   LDB     #ErrSN			; Syntax error
        JMP     >SysErr 		; generate it

;process the minus unary operator
L89B9   LDA     #$7D			; unarry minus presidence flag	
        JSR     >L888B			; process the operation following unarry negation
        JMP     >ChangeFPA0Sign			; change sign of FPA0 mantissa

; evaluate alpha expression
L89C1   JSR     >VarGetVar 		; find the descriptor address of variable 
D89C4   STX     <BasVarAssign16		; save descriptor address
        LDA     <BasVarType		; test if variable type
        BNE     L8953			; return if string
        JMP     >XtoFPA0			; copy FP number from (X) to FPA0

; evaluate a secondary token (following an $FF token).
L89CD   JSR     <BasChrGet		; get the token
        TFR     A,B			; save it in B
        ASLB				; multiply by 2 and drop bit 7
        JSR     <BasChrGet		; gext next character	
        CMPB    #DTokCountF*2		; 33 secondary tokens
        BLS     L89DC			; yep one of ours.
        JMP     [BasAddrDskFuncDisp] 	; Else call "dos" basic stub handler

; Because of the above manipulation of the token byte when comparing for tokens we need
; to compare for (TokenValue-128) * 2.	
L89DC   PSHS    B			; save token offset on stack	
        CMPB    #(DTokLEFTS&$7F)*2	; LEFTS  			
        BCS     L8A04			; Do secondaries LEFT$ or less
        CMPB    #(DTokINKEYS&$7F)*2	; INKEYS
        BCC     L8A06			; Do secondaries INKEY$ or more
	
        BSR     VarCKOpBrac 		; syntax check for open bracket		
        LDA     ,S			; get token number from stack
        CMPA    #(DTokPOINT&$7F)*2	; check for a point command
        BCC     L8A06			; do point
	
; do secondaries LEFT$, RIGHT$, MID$
        JSR     >VarGetStr 		; get first string of command
        BSR     VarCKComma 		; syntax check for a comma
        JSR     >VarGetExpr 		; 'TM' error if numeric variable
        PULS    A			; get token off stack 
        LDU     <BasVarAssign16		; point to string descriptor
        PSHS    A,U			; save token and descriptor address
        JSR     >VarGet8Bit 		; evaluate first numeric argument
        PULS    A			; get token offset from stack
        PSHS    D			; resave token offset + numeric argument
        
	FCB	Skip2LDX		; skip 2 bytes
L8A04   
	BSR	L899F			; syntax check for a '('
L8A06   PULS    B			; get token offset
        LDX     BasAddrFuncDisp 	; get function jump table address	
        ABX				; add in command offset
        JSR     [,X]			; go do function
        JMP     >VarGetExprCC 		; 'TM' error if variable type = string

; logical OR jumps herre
CmdOR:
L8A11   FCB	Skip1LD			; skip 1 byte

; logical AND jumps herre
CmdAND:
L8A12	CLRA				; AND flag = 0
        STA     <BasGenCount		; save AND/OR flag
        JSR     >INTConv		; convert FPA0 into an integer in D
        STD     <BasDelim1		; save it
        JSR     >FPA1toFPA0			; move FPA1 to FPA0
	
        JSR     >INTConv		; convery FPA0 into an integer in D
        TST     <BasGenCount		; test AND / OF flag
        BNE     L8A2A			; branch if 'OR'
        ANDA    <BasDelim1		; AND two numbers together
        ANDB    <BasDelim2
        BRA     L8A2E			; covert result to FP number

L8A2A   ORA     <BasDelim1		; OR two numbers together
        ORB     <BasDelim2
L8A2E   JMP     >VarAssign16Bit2 	; return result of operatiion as FP

L8A31   JSR     >L8879			; Type check
        BNE     L8A46
        LDA     <FP1SGN			; get sign byte
        ORA     #$7F			; make +ve???
        ANDA    <FPA1			
        STA     <FPA1
        LDX     #FP1EXP 		; Point to FPA1
        JSR     >L9441			; compare to FPA0
        BRA     L8A7C

; Relational comparison of strings
L8A46   CLR     <BasVarType		; set variable type to numeric
        DEC     <BasTempRelateFlag	; remove string type flag (bit0) from the relational
					; comparison data
        JSR     >L8D9D			; get length and address of string who's descriptor 
					; address is in the botom of FPA0
        STB     <StrDesc		; save in tempory descriptor
        STX     <StrDesc+2
        LDX     <FPA1+2			; get length and address of string who's descriptor 
					; address is in the botom of FPA1+2
        JSR     >VarDelVar 
        LDA     <StrDesc		; get length of string b in A
        PSHS    B			; save length of string a
        SUBA    ,S+			; subtract length a from length b
        BEQ     L8A65			; strings equal length : skip
	
        LDA     #$01			; true flag
        BCC     L8A65			; true if length b > length a
        LDB     <StrDesc		; load B with length of string b	
        NEGA				; set flag false
	
L8A65   STA     <FP0SGN			; save true / false flag
        LDU     <StrDesc+2		; point U at start of string
        INCB				; compensate for decb below....

; enter with accb containing the length of the shorter string	
L8A6A   DECB				; decrement shorter string length
        BNE     L8A71			; branch if all of string not compared
	
        LDB     <FP0SGN			; get reue / false flag
        BRA     L8A7C			; check truth of relational comparison

L8A71   LDA     ,X+			; get a byte from string a
        CMPA    ,U+			; compare to byte in string b
        BEQ     L8A6A			; check another character if equal
	
        LDB     #$FF			; false flag if string a > b
        BCC     L8A7C			; branch if string a > b
        NEGB				; set flag true

; determine the truth of comparison result in FPA0	
L8A7C   ADDB    #$01			; convert $FF, 0, 1 to 0,1,2
        ROLB				; now it's 1,2,4 for >, =, <
        ANDB    <BasRelateFlag		; and the actual comparison with the desired comparison
        BEQ     L8A85			; branch if false, no matching bits
        LDB     #$FF			; true flag
L8A85   JMP     >L9427			; convert B into fp number in fpa0

; DIM command
L8A88   JSR     >VarCKComma 		; syntax check for comma
CmdDim:
L8A8B   LDB     #$01			; dimension flag 
        BSR     L8A97			; save array space for this variable
        JSR     <BasChrGetCurr		; read next input character
        BNE     L8A88			; keep dimensioning id not end of line
        RTS

; Evaluate a variable - return x and varptr pointing to variable descriptor.
; Each variable requires 7 bytes : 
;	the first two bytes are the variable name and 
; 	the next 5 bytes are the descriptor. 
; if bit 7 of the first byte of varlable name is set, the  variable is a def fn variable. 
; if bit 7 of the second byte of variable name is set the variable is a string
; otherwise the variable is numeric.
; if the variable is not found, a zero variable is inserted into the variable space
VarGetVar:
L8A94   CLRB				; clear dimension flag
        JSR     <BasChrGetCurr		; get current input character
L8A97   STB     <BasArrayEval		; save array flag
L8A99   STA     <BasVarLastInUse	; save input character

; entry point for a DEF FN search
        JSR     <BasChrGetCurr		; get current input character	
        BSR     CheckAAlpha		; set carry if not alpha
        LBCS    BasSNError 		; syntax error if not alpha
	
        CLRB				; default second variable character to zero
        STB     <BasVarType		; set variable type to numeric
        JSR     <BasChrGet		; get another char from basic
        BCS     L8AAE			; branch if numeric, second character in var may be numeric
        BSR     CheckAAlpha		; check to see if it is alpha
        BCS     L8AB8			; branch if not alpha
	
L8AAE   TFR     A,B			; save second char in B

; read input characters until a non alpha or non numeric is found :
; 	ignore all characters in variable name after the 1st two
L8AB0   JSR     <BasChrGet		; get next character
        BCS     L8AB0			; branch if numeric
        BSR     CheckAAlpha		; check alphabetic
        BCC     L8AB0			; branch if alpha
	
L8AB8   CMPA    #'$'			; check for a string variable '$'
        BNE     L8AC2			; branch if not string
	
        COM     <BasVarType		; set variable type to string	
        ADDB    #$80			; set bit 7 of second char, flag as string
        JSR     <BasChrGet		; get an input character
	
L8AC2   STB     <BasVarLastInUse+1	; save second character in var last in use
        ORA     <BasDisArraySearch	; or in array disable flag, if $80, don't search in arrays
        SUBA    #$28			; is this an array variable (first char is '(')
        LBEQ    L8B44			; yes: branch
	
        CLR     <BasDisArraySearch	; reset array search disable flag
        LDX     <BasVarSimpleAddr	; get address of simple variables
        LDD     <BasVarLastInUse	; get the variable name we are looking for	
L8AD2   CMPX    <BasVarArrayAddr	; end of simple variables?
        BEQ     L8AE8			; yes : skip
        CMPD    ,X++			; compare searched for variable to current variable
        BEQ     L8B19			; branch if they match
	
        LEAX    5,X			; move to next variable descriptor
        BRA     L8AD2			; loop again

; Set carry if A not uppercase alphabetic
CheckAAlpha   
	CMPA    #'A'			; character before 'A'
        BCS     L8AE7			; yes : exit
        SUBA    #$5B			; A = A - 'Z'+1 character above 'Z'
        SUBA    #$A5			; A = A - -('Z'+1)
L8AE7   RTS

; put a new variable in the table of variables.
L8AE8   LDX     #DBZero 	; point X to zero location
        LDU     ,S			; get current return addresss
        CMPU    #D89C4			; did we come from 'evaluate alpha expression' ?
        BEQ     L8B1B			; yes return a zero value
	
        LDD     <BasVarEnd		; get end of arrays and save to temp var (Eval43).
        STD     <Eval43
        ADDD    #$0007			; add 7 to end of arrays (each variable is 7 bytes).
        STD     <Eval41			; save at Eval41
        LDX     <BasVarArrayAddr	; get end of simple variables
        STX     <Eval47			; save at Eval47
        JSR     >BasChkArrSpaceMv 	; make a 7 byte slot at top of variables for new variable
	
        LDX     <Eval41			; get new end of arrays
        STX     <BasVarEnd		; update
        LDX     <Eval45			; get new end of simple variables		
        STX     <BasVarArrayAddr	; save it
        LDX     <Eval47			; get old end of variables
        LDD     <BasVarLastInUse	; get variable name
        STD     ,X++			; save variable name in new variable
        CLRA				; CLRD
        CLRB
        STD     ,X			; zero out the FP value of a numeric or the
        STD     2,X			; length and address of a string
        STA     4,X		
L8B19   STX     <BasVarPtrLast		; store the address of the variable
L8B1B   RTS				

SmallestInt
	FCB     $90,$80,$00,$00,$00	; -32768, smallest signed 2 byte integer

L8B21   JSR     <BasChrGet		; get an input character from basic
L8B23   JSR     >L8872			; go evaluate a numeric expression
L8B26   JSR     >VarGetExprCC 		; 

VarGetUsr:
L8B29   LDA     <FP0SGN			; get FPA0 sign bit
        BMI     BasFCError 		; FC error if -ve

; convert fpa0 to a signed two byte integer; return value in accd	
INTConv JSR     >VarGetExprCC 		; 'TM' error if string variable	
        LDA     <FP0EXP			; get FPA0 exponent		
        CMPA    #$90			; compare to 32768, largest integer exponent
        BCS     L8B3E			; branch if FPA0 < 32768
	
        LDX     #SmallestInt		; point X to FP value of -32768	
        JSR     >L9441			; compare to FPA0
        BNE     BasFCError 		; 'FC' error if not same
	
L8B3E   JSR     >L9473			; convert FPA0 to 2 byte integer
        LDD     <BasVarAssign16		; return in in D
        RTS

; evaluate an array variable
L8B44   LDB     <BasArrayEval		; Get array flag and variable type
        LDA     <BasVarType		; 
        PSHS    D			; save on stack
        CLRB				; reset dimension counter
	
L8B4B   LDX     <BasVarLastInUse	; get variable name
        PSHS    B,X			; save name & dim counter
        BSR     L8B21			; evaluate expression (dimension length)
        PULS    B,X,Y			; restore variable name, dimension counter, array flag
        STX     <BasVarLastInUse	; save variable name and type
        LDU     <BasVarAssign16		; get dimension length
        PSHS    Y,U			; save dimension length array flag, variable type
        INCB				; increase dimension counter
        JSR     <BasChrGetCurr		; get next character
	
        CMPA    #','			; check for another dimension ',' 
        BEQ     L8B4B			; branch if more
	
        STB     <BasGenCount		; save dimension counter
        JSR     >VarCKClBrac 		; syntax check for closing brackets
	
        PULS    D			; restore variable type and array flag, dim len still stacked
        STA     <BasVarType		; 
        STB     <BasArrayEval
        LDX     <BasVarArrayAddr	; get start of arrays

L8B6D   CMPX    <BasVarEnd		; reached end of arrays?
        BEQ     L8B92			; yes : no match found
	
        LDD     <BasVarLastInUse	; get searched for variable name
        CMPD    ,X			; compare to current variable name
        BEQ     L8B7E			; found : branch 
        LDD     2,X			; get offset to next array variable
        LEAX    D,X			; follow link
        BRA     L8B6D			; keep searching

L8B7E   LDB     #ErrDD			; 'redimensioned array' error
        LDA     <BasArrayEval		; test array flag, if not zero trying to redimension array. 
        BNE     L8B8F			; yep : error
	
        LDB     <BasGenCount		; get number of dimensions in array
        CMPB    4,X			; compare to this array's dimensions
        BEQ     L8BE3			; branch if same			
BasBSError   
	LDB     #ErrBS			; bad subscript

        FCB     Skip2			; skip 2 bytes

BasFCError:
L8B8D   LDB     #ErrFC			; FC error 
L8B8F   JMP     >SysErr 

; Insert a new array into array variables each set of array variables is preceeded
; by a descriptor block composed of 5+2*n bytes where n is the number of dimensions 
; in the array. 
; The block is defined as follows 
;	bytes 0,1 	variable's name 
;	bytes 2,3 	total length of array items and descriptor 
;	byte 4		number of dimenisions 
;	bytes 5,6	length of dimension 1
;	bytes 7,8	length of dimension 2
;	bytes 4+n,5+n	length of dimension n.
;
L8B92   LDD     #$0005			; 5 bytes / array entry
        STD     <CoefPTR		; save it
        LDD     <BasVarLastInUse	; get name of array 
        STD     ,X			; save it in first byte of descriptor
        LDB     <BasGenCount		; get number of dimensions
        STB     4,X			; save in 4th byte of descriptor
        JSR     >BasChkB2Free 		; check for space for descriptor in free RAM
        STX     <Eval41			; temp save descriptor address
	
L8BA4   LDB     #$0B			; default dimension value X(10)
        CLRA				
        TST     <BasArrayEval		; test array flag
        BEQ     L8BB0			; branch if not dimensioning
	
        PULS    D			; get dimension length
        ADDD    #$0001			; add 1: X(0) has a length of 1
L8BB0   STD     5,X			; save length of array dimension
        BSR     L8C11			; multiply addumulator array size by 
					; length of dimension
        STD     <CoefPTR		; save result
        LEAX    2,X			; bump up pointer by 2
        DEC     <BasGenCount		; decrement dimension counter
        BNE     L8BA4			; branch if more to do
	
        STX     <BasTempPtr		; save address of (end of array descriptor - 5)
        ADDD    <BasTempPtr		; add total size of new array
        LBCS    BasOMError 		; 'OM' Error if > $FFFF
        TFR     D,X			; save end of array in X
        JSR     >L8335			; check enough free memory?
        SUBD    #(StackBuf-5)		; subtrackt stack buffer - 5			
        STD     <BasVarEnd		; save new end of arrays
        CLRA				; zero terminator byte
L8BCF   LEAX    -1,X			; store 2 terminator bytes at end of array descriptor
        STA     5,X
        CMPX    <BasTempPtr		; 
        BNE     L8BCF
	
        LDX     <Eval41			; get address of start of descriptor
        LDA     <BasVarEnd		; get MSB of start of arrays, LSB already in B
        SUBD    <Eval41			; subtract address of start of descriptor
        STD     2,X			; save total array+descriptor length in descriptor
        LDA     <BasArrayEval		; get array flag 
        BNE     L8C10			; branch back if still dimensioning

; Calculate pointer to correct element	
L8BE3   LDB     4,X			; get the number of dimensions
        STB     <BasGenCount		; temp save 
        CLRA				; D=0
        CLRB

L8BE9   STD     <CoefPTR		; save accumulated pointer	
        PULS    D			; pull dimension argument off stack
        STD     <BasVarAssign16		; save it 
        CMPD    5,X			; compare to saved 'DIM' argument
        BCC     L8C2E			; 'BS' error if >= to saved dimension
	
        LDU     <CoefPTR		; get accumulated pointer
        BEQ     L8BFC			; branch if first dimension
	
        BSR     L8C11			; multiply accumulated pointer and dimension length
        ADDD    <BasVarAssign16		; and add to current argument
	
L8BFC   LEAX    2,X			; move pointer to next dimension
        DEC     <BasGenCount		; decrement dimension counter
        BNE     L8BE9			; branch if dimensions left

; Multiply D by 5 - 5 bytes / array value	
        STD     ,--S			; save initial value on stack for add at end
        ASLB				
        ROLA				; 2 times
        ASLB
        ROLA				; 4 times
        ADDD    ,S++			; add initial value
        LEAX    D,X			; add offset to start of array
        LEAX    5,X			; adjust pointer for size of descriptor
        STX     <BasVarPtrLast		; save pointer to array value
L8C10   RTS				; return

; multiply 2 byte number in 5,x by the 2 byte number in coefpt. 
; return result in accd, bs error if > $ffff
L8C11   LDA     #$10			; 16 shifts to do a multiply
        STA     <Eval45			; shift counter
        LDD     5,X			; get size of dimension
        STD     <BasBotStack		; and save it
        CLRA				; D=0
        CLRB
	
L8C1B   ASLB				; shift B left one bit
        ROLA
        BCS     L8C2E			; 'BS' error if carry
        ASL     <CoefPTR+1		; shift multiplicand left one bit
        ROL     <CoefPTR		; add multiplier to accumulator
        BCC     L8C29			; skip if no carry
	
        ADDD    <BasBotStack		; add multiplier to D
        BCS     L8C2E			; 'BS' error if carry (>$FFFF)
	
L8C29   DEC     <Eval45			; dec shift counter
        BNE     L8C1B			; keep going if more shifts
        RTS

L8C2E   JMP     >BasBSError		; jump to BS error

;
; MEM command
; This is not a true indicator of free memory because basic requires a 
; StackBuf size buffer for the stack for which mem does not allow.

CmdMEM:
L8C31   TFR     S,D			; get address of stack top

	ifdef	Dragon64
	JMP     >L9FCE			; Dragon 64 fixup for > 32K
	else
        SUBD    <BasVarEnd		; subtract end of variables
VarAssign16Bit:
L8C35   FCB     Skip1			; skip one byte
	endc

; convert the value in B to an FP number in FPA0
VarAssign8Bit:
L8C36   CLRA				; zero MSB

; convert the value in D to an FP number in FPA0
VarAssign16Bit2:
L8C37   CLR     <BasVarType		; sat variable type to numeric
        STD     <FPA0			; save D to first bytes of FPA0 mantissa
        LDB     #$90			; exponent required if top 2 bytes of FPA0
					; are to be treated as an integer
        JMP     >L942D			; convert rest of FPA0 to integer

CmdSTRS:
L8C40   JSR     >VarGetExprCC 		; 'TM' error if string
        LDU     #BasBuffer+2			
        JSR     >L958A			; convert FP number to ascii string in the string buffer
        LEAS    2,S			; purge return address from stack
        LDX     #BasBuffer+1		; point X to string buffer			
        BRA     L8C5B			; copy string to string space

; Reserve B bytes of string space. return start address in (X) and frespc

L8C50   STX     <Eval4D			; save X in temp
BasResStr:
L8C52   BSR     BasResStr2 		; reserve B bytes in string space
L8C54   STX     <StrDesc+2		; save address of new space
        STB     <StrDesc		; save address of reserved block
        RTS

L8C59   LEAX    -1,X			; move pointer back one

; scan a line from (X) until an end of line flag (zero) or either of the two terminators 
; stored in charac or endchr is matched.
; the resulting string is stored in the string space only if the start of the 
; string is <= strbuf+2
L8C5B   LDA     #'"'			; initialize terminators to " 
L8C5D   STA     <BasDelim1
        STA     <BasDelim2
	
L8C61   LEAX    1,X			; move pointer up one
        STX     <ResSGN			; temporary save start of string
        STX     <StrDesc+2			
        LDB     #$FF			; init character counter to -1
	
L8C69   INCB				; increment character counter
        LDA     ,X+			; get character
        BEQ     L8C7A			; zero? : terminate, end of line
        
	CMPA    <BasDelim1		; starting terminator?
        BEQ     L8C76			; yes : check for "
        CMPA    <BasDelim2		; ending terminator?
        BNE     L8C69			; no : loop again
	
L8C76   CMPA    #'"'			; check for " string terminator
        BEQ     L8C7C			; yes : don't move ptr back
	
L8C7A   LEAX    -1,X			; move pointer back one
L8C7C   STX     <CoefPTR		; save end of string address
        STB     <StrDesc		; save length of string	in temp descriptor
        JSR     VectResetBasMem 	; call ram hook
	
        LDU     <ResSGN			; get initial string start
        CMPU    #BasBuffer+2		; compare to start of string buffer
        BHI     L8C92			; branch if > than start of buffer
        BSR     L8C50			; go reserve space for the string
        LDX     <ResSGN			; point X to beginning of string
        JSR     >L8D8B			; move B bytes from (X) to [FRESPC], move the string
	
; Put direct page string descriptor buffer data on the string stack. 
; Set variable type to string
L8C92   LDX     <BasStrFirstFreeTemp	; get next available string stack descriptor	
        CMPX    #CasFNameLen 		; compare to top of string descriptor stack
        BNE     L8C9E			; formular OK.
	
        LDB     #ErrST			; 'Too complex' error.
L8C9B   JMP     >SysErr 		

L8C9E   LDA     <StrDesc		; get length of string

	ifdef	Dragon64
	STA	,X			; and save it in byte 0 of descriptor
	else
        STA     0,X
	endc
	
        LDD     <StrDesc+2		; get address of string  
        STD     2,X			; save it in bytes 1,2 of descriptor
        LDA     #$FF			; set variable type to string
        STA     <BasVarType		
        STX     <BasStrLastUsedTemp	; save start of string descriptor
        STX     <BasVarAssign16		; in last used & fpa0
        LEAX    5,X			; move to next descriptor
        STX     <BasStrFirstFreeTemp	; save next free
        RTS

; Reserve B bytes in string storage space
; return with the starting address of the reserved string space in (X) and frespc
BasResStr2:
L8CB3   CLR     <BasGarbageFlag		; clear garbage collection flag
L8CB5   CLRA				; push length of string on stack
        PSHS    D
	
        LDD     <BasVarStrTop		; get start of string variables	
        SUBD    ,S+			; subtract string length
        CMPD    <AddrStack		; compare to start of string storage
        BCS     L8CCB			; if below start the reorganize......

        STD     <BasVarStrTop		; save new start of string variables
        LDX     <BasVarStrTop		; get start of string variables
        LEAX    1,X			; add one
        STX     <BasStrUtil		; save start address of newly reserved space
        PULS    B,PC			; restore length & return

L8CCB   LDB     #ErrOS			; out of string space error			
        COM     <BasGarbageFlag		; toggle garbage collect flag
        BEQ     L8C9B			; error if recent garbage collect....
					; there really is no free space.....
        BSR     VarGarbageCollect 	; go collect the garbage...stringspace defrag.
        PULS    B			; get back the number of bytes to reserve
        BRA     L8CB5			; try to reserve bytes again

; garbage collect the string space.
VarGarbageCollect:
L8CD7   LDX     <AddrFWareRamTop	; get the top of the string space
L8CD9   STX     <BasVarStrTop		; save top of unorganized string space
        CLRA				; D=0
        CLRB
        STD     <Eval4B			; reset variable pointer to 0
        LDX     <AddrStack		; point to start of string space
        STX     <Eval47			; save it
        LDX     #BasStrDescStack 	; point x to start of string descriptor stack
L8CE6   CMPX    <BasStrFirstFreeTemp	; compare to address of next available descriptor
        BEQ     L8CEE			; branch if top of string stack
        BSR     L8D1E			; check for string in unorganized space
        BRA     L8CE6			; keep checking variables

L8CEE   LDX     <BasVarSimpleAddr	; get the end of basic program / beginning of simple vars	

L8CF0   CMPX    <BasVarArrayAddr	; compare to end of simple vars
        BEQ     L8CF8			; branch if at end of vars
        BSR     L8D18			; check for string in unorganized string space
        BRA     L8CF0			; keep checking variables

L8CF8   STX     <Eval41			; save address of end of variables

L8CFA   LDX     <Eval41			; get current array pointer
L8CFC   CMPX    <BasVarEnd		; compare to end of arrays
        BEQ     L8D35			; branch if at end of arrays
	
        LDD     2,X			; get length of array and descriptor
        ADDD    <Eval41			; add to current array pointer
        STD     <Eval41			; and re-save it
	
D8D06   LDA     1,X			; get first character of variable name
        BPL     L8CFA			; branch if numeric array
	
        LDB     4,X			; get the number of dimensions of this array
        ASLB				; multiply by 2
        ADDB    #$05			; add 5 bytes for name, length and no of dimensions
        ABX				; x now points to start of array elements
	
L8D10   CMPX    <Eval41			; at end of array?
        BEQ     L8CFC			; yes: check for another array
        BSR     L8D1E			; check for string located in un-organized string space
        BRA     L8D10			; keep checking elements in this array

L8D18   LDA     1,X			; get the first byte of variable name
        LEAX    2,X			; move X to point to the descriptor
        BPL     L8D32			; branch if numeric

; search for string - enter with x pointing to the string descriptor. 
; if string is stored between v47 and strtab, save descriptor pointer
; in v4b and reset v47 to string address

L8D1E   LDB     ,X			; get the length of the string
        BEQ     L8D32			; branch if zeo length string, move to next
	
        LDD     2,X			; get starting address of the string
        CMPD    <BasVarStrTop		; compare to start of string variables 
        BHI     L8D32			; branch if not stored in string space
	
        CMPD    <Eval47			; compare to start of string space
        BLS     L8D32			; branch if not stored in string space
	
        STX     <Eval4B			; save variable pointer if store in string space
        STD     <Eval47			; save string starting address
L8D32   LEAX    5,X			; move to next variable descriptor
L8D34   RTS

L8D35   LDX     <Eval4B			; get address of descriptor for string in unorganized
					; space with the highest address
        BEQ     L8D34			; branch if none found and garbage collection done
	
        CLRA				; clear MSB of length
        LDB     ,X			; get length of string
        DECB				; subtract 1
        ADDD    <Eval47			; add length of string to it's starting address
        STD     <Eval43			; save as move starting address
        LDX     <BasVarStrTop		; point x at start of organized strings	
        STX     <Eval41			; save as move ending address
        JSR     >L831E			; move string from current position to top
					; of unorganized string space
        LDX     <Eval4B			; get address of descriptor
        LDD     <BasVarFPAcc4		; get new starting address of string
        STD     2,X			; update descriptor with new start address
        LDX     <BasVarFPAcc4		; get new top of unorganized space
        LEAX    -1,X			; move back one
        JMP     >L8CD9			; and do some more

; concatinate two strings
L8D55   LDD     <BasVarAssign16		; get descriptor address of string a
	PSHS    D			; save it
        JSR     >L8954			; get descriptor address of string b
        JSR     >VarGetExpr 		; 'TM' error if numeric variable
	
        PULS    X			; recover descriptor address of string a
        STX     <ResSGN			; and save it
	
        LDB     ,X			; get length of string a
        LDX     <BasVarAssign16		; point x to descriptor of string b
        ADDB    ,X			; add length of string b
        BCC     L8D70			; less than max length, continue
	
        LDB     #ErrLS			; string too long error
        JMP     >SysErr 		

L8D70   JSR     >L8C50			; reserve room in string space for new string
        LDX     <ResSGN			; get descriptor of string a
        LDB     ,X			; get length of string a
        BSR     L8D89			; move string a into reserved buffer
	
        LDX     <Eval4D			; get descriptor of string b 
        BSR     VarDelVar 		; get length and address of string b		
        BSR     L8D8B			; move b into reserved buffer
        LDX     <ResSGN			; get descriptor pointer of string a in x
        BSR     VarDelVar 		; delete string a if last string on string stack
        JSR     >L8C92			; put descriptor on string stack
        JMP     >L8899			; branch back to expression evaluation

; move B bytes from 2,X to FreSpec
L8D89   LDX     2,X			; point X at source address		
L8D8B   LDU     <BasStrUtil		; point U at destination address
        INCB				; compensate for decb below
        BRA     L8D94			; do it!

; move B bytes from (X) to (U) -- could replace with call to UtilCopyBXtoU????
L8D90   LDA     ,X+			; get a byte from source
        STA     ,U+			; save in destination
L8D94   DECB				; decrement counter
        BNE     L8D90			; loop again if more to do
	
        STU     <BasStrUtil		; save end address
D8D99   RTS

; return length (accb) and address (x) of string whose descriptor is in fpa0+2
; delete the string if it is the last one
; put on the string stack. 
; remove string from string space if it is at the bottom of string variables.
BasGetStrLenAddr:
L8D9A   JSR     >VarGetExpr 		; 'TM' error if variable is numeric
L8D9D   LDX     <BasVarAssign16		; get address of selected string descriptor

VarDelVar:
L8D9F   LDB     ,X			; get length of string
        BSR     L8DBB			; check to see if this descriptor was the last
					; put on the string stack
        BNE     L8DB8			; skip if not
	
        LDX     7,X			; get start address of string just removed
        LEAX    -1,X			; move pointer down one
        CMPX    <BasVarStrTop		; compare to start of string variables
        BNE     L8DB5			; branch if not at bottom of string variables
	
        PSHS    B			; save length in D (A was already cleared)
        ADDD    <BasVarStrTop		; add length of just removed string
        STD     <BasVarStrTop		; to the start of the string variables, this 
					; will remove the string
        PULS    B			; recover length
L8DB5   LEAX    1,X			; add one to pointer
        RTS

L8DB8   LDX     2,X			; point X to address of string not on stack
        RTS

; remove string from string stack. 
; enter with x pointing to a string descriptor - 
; 	delete the string from stack if it is on top of the stack. 
;	if the string is deleted, set the zero flag
L8DBB   CMPX    <BasStrLastUsedTemp	; compare to last used descriptor address on string stack	
        BNE     L8DC6			; return if descriptor not on string stack
	
        STX     <BasStrFirstFreeTemp	; save last used descriptor as next available
        LEAX    -5,X			; move last used descriptor back 5 bytes
        STX     <BasStrLastUsedTemp	; and save last used descriptor address	
        CLRA				; set zero flag
L8DC6   RTS

; Basic LEN command
CmdLEN:
L8DC7   BSR     L8DCC			; point X to propper string and get length
L8DC9   JMP     >VarAssign8Bit 		; convert length in B to FP in FPA0

; point x to string address load length into b. 
; enter with the string descriptor in bottom two bytes of fpa0
L8DCC   BSR     BasGetStrLenAddr 	; get length and address of string
        CLR     <BasVarType		; set variable to numeric
        TSTB				; set flags according to length
        RTS

; Basic CHR$ command
CmdCHRS:
L8DD2   JSR     >FPA0toB			; convert FPA0 to integer in D
L8DD5   LDB     #$01			; reserve one byte in the string space
        JSR     >BasResStr2		
 		
        LDA     <FPA0+3			; get ascii string value
        JSR     >L8C54			; save string descriptor in temp descriptor
        STA     ,X			; save character in string
L8DE1   LEAS    2,S			; drop return address from stack
L8DE3   JMP     >L8C92			; put descriptor data on string stack

; Basic ASC command
CmdASC:
L8DE6   BSR     BasGetStrFirst 		; Get first character of string in B
        BRA     L8DC9			; convert to an FP number in FPA0

BasGetStrFirst:
L8DEA   BSR     L8DCC			; point X to string descriptor
        BEQ     L8E4C			; 'FC' error if null string
        LDB     ,X			; get first byte of string
        RTS

CmdLeftS:
L8DF1   BSR     L8E3B			; get arguments from stack
L8DF3   CLRA				; clear pointer offset, offset = 0 for LEFT$
L8DF4   CMPB    ,X			; compare length parameter to length of string
        BLS     L8DFB			; branch if length > string length
	
        LDB     ,X			; use length of string otherwise
        CLRA				; clear string pointer offset = 0 for LEFT$
L8DFB   PSHS    D			; save params on stack
        JSR     >BasResStr 		; reserve B bytes in string space
        LDX     <Eval4D			; point X to string descriptor
        BSR     VarDelVar 		; get address of old string
        PULS    B			; restore string pointer offset
        ABX				; add to string address
        PULS    B			; restore length 
        JSR     >L8D8B			; copy bytes from (X) to string	
        BRA     L8DE3			; put temp descriptor on string stack

CmdRightS:
L8E0E   BSR     L8E3B			; get arguments from stack
        SUBA    ,X			; A = length param - length of old string
        NEGA				; now length = length of old string
        BRA     L8DF4			; put new string in string space

CmdMidS:
L8E15   LDB     #$FF			; get default value and length
        STB     <FPA0+3			; save it
        JSR     <BasChrGetCurr		; get current character from basic
        CMPA    #')'			; ')' argument delimiter?
        BEQ     L8E24			; yes: no length param given
	
        JSR     >VarCKComma 		; syntax check for comma
        BSR     VarGet8Bit 		; evaluate expression length
L8E24   BSR     L8E3B			; get arguments from stack
        BEQ     L8E4C			; 'FC' error if empty string
        CLRB				; clear length (default value)
        DECA				; subtract one from position, make zero based
        CMPA    ,X			; compare to length of old string
        BCC     L8DFB			; if position > length of old string then 
					; result will be an empty string
        TFR     A,B			; save absolute position in B
        SUBB    ,X			; B now = position - length of old string
        NEGB				; now B = length of ols string - position
        CMPB    <FPA0+3			; if the amount of the string to the right 
	BLS     L8DFB			; of the position is > length then use 
					; all of the string to the right of the position
					; instead of the length
        
        LDB     <FPA0+3			; get length of new string
        BRA     L8DFB			; put new string in string space
	
; do a syntax check for ")", then pull the previously calculated numeric
; argument D and string argument descriptor addr off of the stack
L8E3B   JSR     >VarCKClBrac 		; syntax check for ')'

        LDU     ,S			; get return address into U
        LDX     5,S			; get address of string
        STX     <Eval4D			; save it in Eval4D
        LDA     4,S			; get length of string in A and B
        LDB     4,S
        LEAS    7,S			; remove descriptor + return address from stack
        TFR     U,PC			; jump to return address in U

L8E4C   JMP     >BasFCError 		; generate FC error

* evaluate an expression - return an integer in B - 'fc' error if expression > 255
L8E4F   JSR     <BasChrGet		; get next basic input character

VarGet8Bit:
L8E51   JSR     >L8872			; evaluate numeric expression
FPA0toB JSR     >L8B26			; convert FPA0 to integer in D
        TSTA				; is it > 256?
        BNE     L8E4C			; yes: FC error
	
        JMP     <BasChrGetCurr		; get next basic input character 

CmdVAL:
L8E5C   JSR     >L8DCC			; point X to string address
        LBEQ    L9182			; if empty string set FPA0
        LDU     <BasAddrSigByte		; save input pointer in U
        STX     <BasAddrSigByte		; point input pointer at address of string
        ABX				; move pointer to end of string terminator
        LDA     ,X			; get last byte of string
        PSHS    A,X,U			; save input pointer, string terminator address
					; and character
        CLR     ,X			; clear string terminator, for ASCII to FP conversion
        JSR     <BasChrGetCurr		; get current character from basic
        JSR     >L94BD			; convert ASCII string to floating point
        PULS    A,X,U			; restrore pointers and character
	
        STA     ,X			; replace string terminator
        STU     <BasAddrSigByte		; restore input character pointer
        RTS 

L8E7A   BSR     VarGet16Bit 		; evaluate expression, return in X
        STX     BasTempLine		; save in BasTempLine
	
VarGetComma8:
L8E7E   JSR     >VarCKComma 		; syntax check for comma
        BRA     VarGet8Bit 		; return 8 bit value	

; evaluate expression : return integer portion in x 
; 'fc' error if expression is negative or > 65535, i.e. not a legal positive integer.
VarGet16Bit:
L8E83   JSR     >L8872			; evaluate numeric expression
L8E86   LDA     <FP0SGN			; get sign of FPA0 mantissa			
        BMI     L8E4C			; FC error if -ve
	
        LDA     <FP0EXP			; get exponent of FPA0 	
        CMPA    #$90			; compare to largest +ve integer
        BHI     L8E4C			; oops higher, FC error
	
        JSR     >L9473			; shift binary point to extream right of FPA0
        LDX     <BasVarAssign16		; load X with lower two bytes of FPA0
        RTS

CmdPeek:
L8E96   BSR     L8E86			; convert FPA0 to integer in X
        LDB     ,X			; get byte from specified address
        JMP     >VarAssign8Bit 		; return it in FPA0

CmdPoke:
L8E9D   BSR     L8E7A			; get address in BasTempLine byte to poke in B
        LDX     BasTempLine		; get address in X
        STB     ,X			; store it
        RTS

CmdLList:
L8EA4   LDB     #$FE			; set device number to printer
        STB     <TextDevN
        JSR     <BasChrGetCurr		; get character from basic

CmdList:
L8EAA   PSHS    CC			; save zero flag on stack
        JSR     >BasGetLineNo 		; convert supplied decimal line number to binary
        JSR     >BasFindLineNo 		; find that line number's address
        STX     <BasListLine		; save address
        PULS    CC			; restore zero flag
        BEQ     L8ECA			; branch if end of line, no line number specified
	
        JSR     <BasChrGetCurr		; get current byte from basic
        BEQ     L8ECF			; branch if end of line
        CMPA    #DTokMINUS		; minus token, range of lines specified
        BNE     L8EC9			; no : return 
	
        JSR     <BasChrGet		; get next character from basic
        BEQ     L8ECA			; branch if end of line
        JSR     >BasGetLineNo 		; get ending line
        BEQ     L8ECF			; branch if legal line no
L8EC9   RTS

L8ECA   LDU     #$FFFF			; set last line to maximum line no, list all lines
        STU     BasTempLine
	
L8ECF   LEAS    2,S			; purge return address from stack
        LDX     <BasListLine		; point X at starting line address
	
L8ED3   JSR     >L90A5			; move cursor to new line
        JSR     >LB77B			; check for break or pause
        
	LDD     ,X			; get address of next basic line
        BNE     L8EE5			; branch if not end of program
	
L8EDD   JSR     >LB663			; check close file handler
        CLR     <TextDevN		; set device no to screen
        JMP     >BasCmdMode 		; return to command mode

L8EE5   STX     <BasListLine		; save new line's start address
        LDD     2,X			; get current line number
        CMPD    BasTempLine		; check against end line number to list
        BHI     L8EDD			; exit if greater than last line no 
	
        JSR     >TextOutNum16 		; output line number
        JSR     >TextOutSpace 		; output space
        LDX     <BasListLine		; get address of line
        BSR     L8F08			; uncrunch line
        LDX     [BasListLine] 		; get address of next line
        LDU     #BasLinInpBuff+1	; point U at buffer containing uncrunched line		
L8EFF   LDA     ,U+			; get a byte from line buffer
        BEQ     L8ED3			; branch if end of line
        JSR     >L90FA			; send character to console / printer
        BRA     L8EFF			; loop for next 

; uncrunch a line into basic's line input buffer
L8F08   JSR     VectDeTokenize 		; call RAM hook
        LEAX    4,X			; move past line no + link to next line
        LDY     #BasLinInpBuff+1	; point Y at destination buffer
	
L8F11   LDA     ,X+			; get a byte from line
        BEQ     L8F66			; end of line: exit
        BMI     L8F2C			; branch if bit 7 is set so a token that needs expanding
        
	CMPA    #':'			; check for :, end of sub line	
        BNE     L8F28			; nope
	
        LDB     ,X			; get character following colon
        CMPB    #DTokELSE		; token for else?
        BEQ     L8F11			; yes: don't put it in buffer
        CMPB    #DTokREMComma		; token for remark?
        BEQ     L8F11			; yes: don't put it in buffer

        FCB	Skip2 			; skip 2 bytes
	
L8F26   LDA	#'!'			; !
L8F28   BSR     L8F5A			; put character in buffer
        BRA     L8F11			; loop for next

; Uncrunch a token
L8F2C   LDU     #BasStub0-10		; Point at command table 				
        CMPA    #$FF			; secondary token (function)
        BNE     L8F37			; nope : skip
        LDA     ,X+			; load next token after $FF
        LEAU    (BasNumFuncs-BasStub0),U	; bump it up to secondary functions 
L8F37   ANDA    #$7F			; mask off bit 7 of token

L8F39   LEAU    10,U			; move to next command table
        TST     ,U			; test to see if any functions?
        BEQ     L8F26			; no: illegal function
	
        SUBA    ,U			; suptract table's starting token from current
        BPL     L8F39			; branch if token not in this table
	
        ADDA    ,U			; restore token number
        LDU     1,U			; get address of functon word table
	
L8F47   DECA				; decrement token number
        BMI     L8F50			; branch if this is correct token
	
L8F4A   TST     ,U+			; grab a byte, test bit 7 set for end of token
        BPL     L8F4A			; not set keep looping
        BRA     L8F47			; move to next token word in table

L8F50   LDA     ,U			; get a character from word table
        BSR     L8F5A			; put character in buffer
	
        TST     ,U+			; check for end of keyword
        BPL     L8F50			; no, keep copying
        BRA     L8F11			; go get next character from line we are uncrunching

L8F5A   CMPY    #(BasLinInpBuff+LineBufMax)	; past end of buffer?
        BCC     L8F66			; yes: branch				
        ANDA    #$7F			; mask off bit 7
        STA     ,Y+			; store character in buffer
        CLR     ,Y			; clear last character + 1
L8F66   RTS


; crunch the line that the input pointer is pointing to into the line input buffer
; return length of crunched line in d
L8F67   JSR     VectTokenize 		; call RAM hook
        LDX     <BasAddrSigByte		; get basic's input pointer address
        LDU     #BasLinInpBuff 		; point U to line input buffer
	
L8F6F   CLR     <Eval43			; clear illegal token flag
        CLR     <Eval44			; clear data flag
	
L8F73   LDA     ,X+			; get input char
        BEQ     L8F98			; end of line: exit
	
        TST     <Eval43			; check illegal token flag 
        BEQ     L8F8A			; branch if not processing illegal token.
	
        JSR     >CheckAAlpha		; set carry if not upper case alpha
        BCC     L8F98			; branch if upper case alpha
	
        CMPA    #'0'			; Character >= '0' ?
        BCS     L8F88			; nope, not numeric
        CMPA    #'9'			; character =< '9' ?
        BLS     L8F98			; yep: numeric

; we end up here if not upper case or numeric.
L8F88   CLR     <Eval43			; clear illegal token flag.
L8F8A   CMPA    #' '			; space character?
        BEQ     L8F98			; do not remove spaces
	
        STA     <Eval42			; save input character as scan delimiter
        CMPA    #'"'			; check for string delimiter
        BEQ     L8FCC			; branch if string
	
        TST     <Eval44			; check data flag and branch if clear
        BEQ     L8FB1
	
L8F98   STA     ,U+			; save character in buffer
        BEQ     L8FA2			; branch if end of line
	
        CMPA    #':'			; check for ':' end of subline 
        BEQ     L8F6F			; and reset flags if so
	
L8FA0   BRA     L8F73			; go get another character

L8FA2   CLR     ,U+			; double zero byte at end of line
        CLR     ,U+
        TFR     U,D			; save address of end of line in D
        SUBD    #BasLinInpHead		; work out length of line
        LDX     #BasLinInpBuff-1	; set input pointer to one before beginning 
        STX     <BasAddrSigByte		; of crunched line
        RTS

L8FB1   CMPA    #'?'			; check for '?' PRINT abreviation
        BNE     L8FB9			; nope
        LDA     #DTokPRINT		; yes set token to PRINT
        BRA     L8F98			; save it in buffer

L8FB9   CMPA    #$27			; single ' same as REM
        BNE     L8FD0			; nope
        LDD     #($3A00+DTokREMComma)	; colon + REM token
        STD     ,U++			; save in line
	
L8FC2   CLR     <Eval42			; set delimiter = 0, end of line

L8FC4   LDA     ,X+			; scan till we match Eval42
        BEQ     L8F98			; branch if end of line
        CMPA    <Eval42			; delimiter?
        BEQ     L8F98			; yep....
	
L8FCC   STA     ,U+			; don't crunch remarks or strings
        BRA     L8FC4			; get more string or remark

L8FD0   CMPA    #'0'			; less than '0'
        BCS     L8FD8			; branch if so
	CMPA    #';'+1			; check for numeric value, ':' or ';'
        BCS     L8F98			; and inser in buffer if so
	
L8FD8   LEAX    -1,X			; move input pointer back one
        PSHS    X,U			; save pointers to input and output strings
        CLR     <Eval41			; clear token flag $00 = command, $FF = secondary
        LDU     #BasStub0-BasTableSize	; point X to command table for basic
	
L8FE1   CLR     <Eval42			; initialize Eval42 as token counter
L8FE3   LEAU    BasTableSize,U		; move to next command table
        LDA     ,U			; get number of reserved words in table
        BEQ     L901A			; none, do function table
 
        LDY     BasTableWords,U		; point Y at reserved word list
L8FEC   LDX     ,S			; get pointer to input string from stack	

L8FEE   LDB     ,Y+			; get a byte from reserved word
        SUBB    ,X+			; compare against input string
        BEQ     L8FEE			; same keep going
	
        CMPB    #$80			; last character has bit 7 set, check for this
        BNE     L9030			; nope not found, check next keyword
	
        LEAS    2,S			; found, delete old input pointer from stack
        PULS    U			; restore output pointer
        ORB     <Eval42			; or in table position to make token 
					; B still contains $80 from subtraction above
        LDA     <Eval41			; check token flag 
        BNE     L9008			; branch if function
	
        CMPB    #DTokELSE		; ELSE token?
        BNE     L900C			; nope, skip
	
        LDA     #':'			; yes it's an else put a ':' before it.
L9008   STD     ,U++			; save :ELSE
        BRA     L8FA0			; go process more input characters

L900C   STB     ,U+			; save token in output
        CMPB    #DTokDATA		; DATA token?
        BNE     L9014			; nope, skip
	
        INC     <Eval44			; set data flag
L9014   CMPB    #DTokREM		; REM token?
        BEQ     L8FC2			; yep, skip
 
L9018   BRA     L8FA0			; go process more input

; now do secondary functions
L901A   LDU     #(BasStub0-CmdTableSize) ; point to first table
        COM     <Eval41			; toggle the token flag	
        BNE     L8FE1			; branch if now checking functions...

; this code will process input data which cannot be crunched and so
; is assumed to be illegal data or an illegal token	
        PULS    X,U			; restore input an output pointers
        LDA     ,X+			; get a byte from input
        STA     ,U+			; store in output
        JSR     >CheckAAlpha		; set carry if not alpha
        BCS     L9018			; branch if not alpha
        COM     <Eval43			; set illegal token flag if uppercase alpha
        BRA     L9018			; process more input characters

L9030   INC     <Eval42			; increment token counter
        DECA				; decrement command counter
        BEQ     L8FE3			; get another command table if done
					; with this one.
        LEAY    -1,Y			; move pointer back one
L9037   LDB     ,Y+			; get to next reserved word
        BPL     L9037			
        BRA     L8FEC			; go see if this word is a match

;Basic PRINT command
CmdPrint:
L903D   BEQ     TextOutCRLF 		; branch if no argument, just print a newline
        BSR     L9044			; check for print options
        CLR     <TextDevN		; set device no to streen	
        RTS

L9044   CMPA    #'@'			; check for print @xxx
        BNE     L904D			; no : branch
	
        JSR     >DoPrintAT		; move cursor to @location
        BRA     L9057			; go print the data

L904D   CMPA    #'#'			; check for channel no
        BNE     L905E			; nope, skip
	
        JSR     >LB7D7			; check for valid dev no
        JSR     >LB63C			; check for a valid output file
	
L9057   JSR     <BasChrGetCurr		; get current input char
        BEQ     TextOutCRLF 		; EOL, print a return
        JSR     >VarCKComma 		; syntax check for a comma
	
L905E   CMPA    #DTokUSING		; Print USING?
        LBEQ    DoPrintUSING		; yes : do it
	
L9064   BEQ     L90AE			; end of line : exit

L9066   CMPA    #DTokTAB		; TAB token?
        BEQ     DoPrintTAB		; go do it
	
        CMPA    #','			; Comma?
        BEQ     DoPrintComma		; go do it
	
        CMPA    #';'			; Semicolon?
        BEQ     L90E0			; yes, don't advance cursor
        
	JSR     >VarGetStr 		; evaluate expression
        LDA     <BasVarType		; get variable type
	
        PSHS    A			; save type on stack
        BNE     L9081			; branch if string variable
        JSR     >L9587			; convert FP to string variable
        JSR     >L8C59			; parse string from x-1, and put on string stack
	
L9081   BSR     L90E8			; print string, who's descriptor is pointed to by X
        PULS    B			; restore variable type
	
        JSR     >SetPRINTParams		; setup width zone etc
        TST     <CasIOFlag		; check the print device
        BEQ     L9092			; and branch if not tape
	
        BSR     TextOutCRLF 		; output a return
        JSR     <BasChrGetCurr		; get current character from basic
        BRA     L9064			; loop for more

L9092   TSTB				; check current print position
        BNE     L909D			; branch if not at start of line
        JSR     <BasChrGetCurr		; get current input char
	
        CMPA    #','			; comma?
        BEQ     DoPrintComma		; yes : deal with it
	
        BSR     TextOutSpace 		; output a space
L909D   JSR     <BasChrGetCurr		; get current char
        BNE     L9066			; loop again
	
TextOutCRLF:
L90A1   LDA     #$0D			; send return to console
        BRA     L90FA

L90A5   JSR     >SetPRINTParams		; setup print parameters		
        BEQ     TextOutCRLF 		; output a return
        LDA     <TextVDUCurrCol		; get current column
        BNE     TextOutCRLF 		; not zero, output return
L90AE   RTS

DoPrintComma   
	JSR     >SetPRINTParams		; setup print parameters		
        BEQ     L90BE			; branch if line width = 0, tape
        LDB     <TextVDUCurrCol		; get current column
        CMPB    <TextVDULastComma	; compare to last tab column
        BCS     L90C0			; branch if < last column
	
        BSR     TextOutCRLF 		; otherwise output a return
        BRA     L90E0

L90BE   LDB     <TextVDUCurrCol		; get current column
L90C0   SUBB    <TextVDUCommaW		; subtract comma width
        BCC     L90C0			; keep going until -ve
        NEGB				; this leaves spaces to next tab in b
        BRA     L90D7			; go move there

; Basic PRINT TAB
DoPrintTAB   
	JSR     >L8E4F			; evaluate expression, value in b
        CMPA    #')'			; next character ')' ?
        LBNE    BasSNError 		; nope: error
	
        JSR     >SetPRINTParams		; setup print parameters		
        SUBB    <TextVDUCurrCol		; get difference of print and tab positions
        BLS     L90E0			; branch if tab position < current position
	
L90D7   TST     <CasIOFlag		; Test device no	
        BNE     L90E0			; branch if tape
	
L90DB   BSR     TextOutSpace 		; output a space
        DECB				; decrement count
        BNE     L90DB			; keep going till we reach tab stop
	
L90E0   JSR     <BasChrGet		; get current character from basic
        JMP     >L9064			; loop again for more

; Copy a string from (X) to console out
TextOutString:
L90E5   JSR     >L8C5B			; parse string from X and put decriptor on 
					; string stack
L90E8   JSR     >L8D9D			; get length of string and drop descriptor
        INCB				; compensate for DECB below
	
L90EC   DECB				; decement counter
        BEQ     L90AE			; end of string? yes: exit
        LDA     ,X+			; get a byte from string
        BSR     L90FA			; send to console out
        BRA     L90EC			; loop for more

TextOutSpace:
L90F5   LDA     #' '			; output a space char ' '

        FCB     Skip2			; skip 2 bytes

TextOutQuestion:
L90F8   LDA     #'?'			; output a question mark '?'
L90FA   JMP     >TextOutChar 		; output character in A

;
; Floating point math package.
;

; add 0.5 to FPA0
L90FD   LDX     #FPPointFive		; point to FP constant
        BRA     AddXtoFPA0		; go add it

; subtract FPA0 from number pointed to by X, leave result in FPA0
SubtractFPA0fromX   
	JSR     >XtoFPA1

; Arithmetic operation (-) jumps here - subtract FPA0 from FPA1 
; (enter with exponent of FPA0 in B and exponent of FPA1 in A)
; Works by adding a -ve.
CmdMinus:
L9105   COM     <FP0SGN			; change mantissa sign of FPA0			
        COM     <ResSGN			; reverse result sign flag
        BRA     CmdPlus 		; go add FPA1 to FPA0

; Add FP number pointed to by X to FPA0 - leave result in FPA0
AddXtoFPA0   JSR     >XtoFPA1

; Arithmetic operation (+) jumps here - subtract FPA0 from FPA1 
; (enter with exponent of FPA0 in B and exponent of FPA1 in A)
CmdPlus:
L910E   TSTB				; check exponent of FPA0
        LBEQ    FPA1toFPA0			; copy FPA1 to FPA0, if FPA0 = 0
        LDX     #FP1EXP			; point X to FPA1 exponent
 
L9116   TFR     A,B			; exponent of FPA1 to B
        TSTB				; check exponent
        BEQ     L9187			; return if FPA1 = 0, nothing to add
	
        SUBB    <FP0EXP			; suptract exponent of FPA0 from  exponent of FPA1
        BEQ     L9188			; branch if exponents equal
        BCS     L912B			; branch if exponent FPA0 > FPA1
        STA     <FP0EXP			; replace FPA0 exponent with FPA1 exponent
        LDA     <FP1SGN			; get FPA1 mantissa sign
        STA     <FP0SGN			; replace FPA0 mantissa sign with it.
        LDX     #FP0EXP			; point X to FPA0
 
        NEGB				; negate exponent difference
L912B   CMPB    #-8			; test difference of exponents
        BLE     L9188			; branch if difference <= 8
	
        CLRA				; clear overflow byte
        LSR     1,X			; shift MS byte mantissa, bit 7 = 0 
        JSR     >L9203			; shift mantissa of X to the right B times
	
L9135   LDB     <ResSGN			; get sign flag
        BPL     L9144			; branch if FPA0 and FPA1 signs the same
        COM     1,X			; compliment mantissa pointed to by X
        COM     2,X			; A below, will convert this operation to a NEG
        COM     3,X
        COM     4,X
        COMA				
        ADCA    #$00			; add one to A, COMA always sets carry
	
; add mantissa of FPA0 and FPA1, put result in FPA0
L9144   STA     <FPSByte		; save FPA sub byte
        LDA     <FPA0+3			; add LS byte of mantissa
        ADCA    <FPA1+3				
        STA     <FPA0+3			; save in FPA0
	
        LDA     <FPA0+2			; add next byte of mantissa
        ADCA    <FPA1+2
        STA     <FPA0+2			; save in FPA0
	
        LDA     <FPA0+1			; add next byte of mantissa
        ADCA    <FPA1+1
        STA     <FPA0+1			; save in FPA0
        
	LDA     <FPA0			; add next byte of mantissa
        ADCA    <FPA1
        STA     <FPA0			; save in FPA0
	
        TSTB				; test sign flag
        BPL     L91A5			; branch if FPA0 and FPA1 signs where the same
L9161   BCS     VarNormFPA0 		; branch if +ve mantissa
        BSR     L91C2			; negate FPA0 mantissa
	
; nomalize FPA0
VarNormFPA0:
L9165   CLRB				; clear temp eponent accumulator
L9166   LDA     <FPA0			; test MSB of mantissa
        BNE     L9198			; branch if <> 0

        LDA     <FPA0+1			; if the MSB is 0
        STA     <FPA0			; shift the mantissa 1 byte at a time
        LDA     <FPA0+2			; this is faster but uses more memory
        STA     <FPA0+1
        LDA     <FPA0+3
        STA     <FPA0+2
        LDA     <FPSByte			 
        STA     <FPA0+3			; FPSByte the carry in
        CLR     <FPSByte
        ADDB    #$08			; shifting 1 byte = 8 shifts add 8 to exponent
        CMPB    #(5*8)			; check for 5 byte shifts
        BLT     L9166			; branch if < 5 shifts, if > 5 then mantissa = 0

L9182   CLRA				; a Zero exponent = 0 floating point

L9183   STA     <FP0EXP			; Zero out exponent		
        STA     <FP0SGN			; Zero out sign
L9187   RTS


L9188   BSR     L91F7			; Shift FPA0 mantissa to right
        CLRB				; clear carry flag
        BRA     L9135				

; shift FPA0 left until bit 7 of mantissa byte = 1
L918D   INCB				; add 1 to exponent accumulator
        ASL     <FPSByte		; shift subbyte 1 bit left
        ROL     <FPA0+3			; shift LSB 
        ROL     <FPA0+2			; shift NSB
        ROL     <FPA0+1			; shift NSB
        ROL     <FPA0			; shift MSB
L9198   BPL     L918D			; branch if not yet normalized
        LDA     <FP0EXP			; get current exponent
        PSHS    B			; save exponent modifier caused by normalization
        SUBA    ,S+			; subtract accumulated exponent modifier
        STA     <FP0EXP			; save as new exponent
        BLS     L9182			; set FPA0 = 0 if the shift caused more or
					; equal number of left shifts than the size of the exponent
	
	FCB	Skip2			; skip 2
L91A5   BCS	L91AF			; branch if mantissa overflow

        ASL     <FPSByte		; FPSByte bit 7 to carry. Use as a round off flag.	
        LDA     #$00			; truncate rest of FSPByte, not LDA leaves carry.
        STA     <FPSByte		; clear the sub byte	
        BRA     L91BB			; go round off result

L91AF   INC     <FP0EXP			; increment exponent, multiply by 2
        BEQ     BasOVError		; overflow error if carry past $FF
	
        ROR     <FPA0			; shift mantissa one to the right
        ROR     <FPA0+1			; this divides it by 2
        ROR     <FPA0+2
        ROR     <FPA0+3
L91BB   BCC     L91C1			; branch if no round off needed
        BSR     L91CC			; add 1 to mantissa, round it off
        BEQ     L91AF			; branch if overflow, mantissa = 0
L91C1   RTS

; Negate the mantissa
L91C2   COM     <FP0SGN			; toggle sign of mantissa
L91C4   COM     <FPA0			; compliment all 4 mantissa bytes
        COM     <FPA0+1
        COM     <FPA0+2
        COM     <FPA0+3
	
L91CC   LDX     <FPA0+2			; get bottom 2 mantissa bytes, 
        LEAX    1,X			; add 1 to them
        STX     <FPA0+2			; and save them
        BNE     L91DA			; branch if no overflow
	
        LDX     <FPA0			; if overflow add 1 to top two bytes
        LEAX    1,X
        STX     <FPA0
L91DA   RTS

BasOVError
	LDB     #ErrOV			; overflow error
        JMP     >SysErr 

L91E0   LDX     #FPA2-1			; point X at FPA2

; Shift FPA pointed to by X to the right -B times. 
; exit with a containing data shifted out to the right (sub byte) 
; and the data shifted in from the left will come from fpcary
L91E3   LDA     4,X			; get LSB of mantissa
        STA     <FPSByte		; save in FPA subbyte
        LDA     3,X			; shift 3 bytes of the mantissa to the right
        STA     4,X			; one complete byte
        LDA     2,X
        STA     3,X
        LDA     1,X
        STA     2,X
        LDA     <FPCARY			; get the carry in byte
        STA     1,X			; save in MSB of mantissa
L91F7   ADDB    #$08			; add the difference of exponents
        BLE     L91E3			; branch if difference is < -8
        LDA     <FPSByte		; get FPA sub byte
        SUBB    #$08			; cast out the 8 added above
        BEQ     L920D			; branc hif exponent difference = 0
	
L9201   ASR     1,X			; shift mantissa and subbyte one bit to the right
L9203   ROR     2,X
        ROR     3,X
        ROR     4,X
        RORA	
        INCB				; add one to exponent difference
        BNE     L9201			; branch if exponents not equal
L920D   RTS

FPOnePointZero   
	FCB     $81,$00,$00,$00,$00		; FP 1.0

; Tchebyshev modified Taylor series coefficients for LN(X)
	
FPTaylor   
	FCB     $03			; 4 coefficients
        FCB     $7F,$5E,$56,$CB,$79	; 0.434255942 (2/7)*(1/LN(2))
        FCB     $80,$13,$9B,$0B,$64	; 0.576584541 (2/5)*(1/LN(2))
        FCB     $80,$76,$38,$93,$16	; 0.961800759 (2/3)*(1/LN(2))
        FCB     $82,$38,$AA,$3B,$20	; 2.88539007 (2/1)*(1/LN(2))
	
D9228   FCB     $80,$35,$04,$F3,$34	; 1/SQR(2)
D922D   FCB     $81,$35,$04,$F3,$34	; SQR(2)
D9232   FCB     $80,$80,$00,$00,$00	; -0.5
D9237   FCB     $80,$31,$72,$17,$F8	; LN(2)

; log - natural logarithm (ln)
; the natural or naperian logarithm is calculated using mathematical identities. 
; fpa0 is of the form fpa0=a*(2**b) (scientific notation). 
; therefore, the log routine  determines the value of ln(a*(2**b)). 
; a series of mathematical identities will expand this term: 
;	ln(a*(2**b))=(-1/2+(1/ln(2))*(ln(a*sqr(2)))+b)*ln(2). 
; all of the terms of the latter expression are constants except for the
; ln(a*sqr(2)) term which is evaluated using the taylor series expansion
CmdLOG:
L923C   JSR     >TestFPA0		; test status of FPA0
        LBLE    BasFCError 		; FC error if -ve or zero
        
	LDX     #D9228			; point X to FP number (1/SQR(2))
        LDA     <FP0EXP			; get exponent of argument
        SUBA    #$80			; subtract bias and save on stack
        PSHS    A
	
        LDA     #$80			; force exponent of FPA
        STA     <FP0EXP			; to be zero
	
        JSR     >AddXtoFPA0			; add FPA0 to fp at X
        LDX     #D922D			; point X to SQR(2)
        JSR     >DivXbyFPA0			; divide SQR(2) by X
	
        LDX     #FPOnePointZero		; point X to fp constant 1.0
        JSR     >SubtractFPA0fromX	; subtract FP0 from fp at X

; now fpa0 = (1-sqr(2)*x)/(1+sqr(2)*x) where x is argument	
        LDX     #FPTaylor		; Point X to taylor series
        JSR     >L9743			; expand polynomial
	
        LDX     #D9232			; point X to FP constant -0.5
        JSR     >AddXtoFPA0			; add FPA0 to fp at X
        PULS    B			; get exponent of argument back (without bias)
        JSR     >L9547			; add B to FPA0
        LDX     #D9237			; point X at LN(2)...drops through to multiply

; multiply FP number pointed to by X by FPA0, result in FPA0
XtimesFPA0   
	BSR     XtoFPA1


; Arithmetic operation (*) jumps here - multiply fpa0 by fp pointed to by X 
; return product in FPA0
CmdMultiply:
L9275   BEQ     L92D9			; branch if exponent of FP0 = 0
        BSR     L92F3			; move packed FPA from X to FPA1
	
; multiply fpa0 mantissa by fpa1. normalize high order bytes of product in fpa0. 
; the low order four bytes of the product will be stored in vab-vae.
MultiplyFPA0byFPA1   
	LDA     #$00			
        STA     <FPA2			; Zero out mantissa of FPA2
        STA     <FPA2+1
        STA     <FPA2+2
        STA     <FPA2+3
        LDB     <FPA0+3			; get LSB of FPA0
        BSR     L92A9			; Multiply by FPA1
	
        LDB     <FPSByte		; temp save sub byte 4
        STB     <VarAE
        
	LDB     <FPA0+2			; get number 3 mantissa byte of FPA0
        BSR     L92A9			; Multiply by FPA1
        LDB     <FPSByte		; temp save sub byte 3
        STB     <VarAD			
	
        LDB     <FPA0+1			; get number 2 mantissa byte of FPA0
        BSR     L92A9			; Multiply by FPA1
        LDB     <FPSByte		; temp save sub byte 2	
        STB     <VarAC
	
        LDB     <FPA0			; get number 1 mantissa byte of FPA0
        BSR     L92AD			; Multiply by FPA1
        LDB     <FPSByte		; temp save sub byte 1
        STB     <VarAB
	
        JSR     >L93B6			; copy mantissa from FPA2 to FPA0
        JMP     >VarNormFPA0 		; normalize FPA0

L92A9   LBEQ    L91E0			; shift FPA2 one byte right
L92AD   COMA				; set carry flag

; multiply fpa1 mantissa by accb and add product to fpa2 mantissa
L92AE   LDA     <FPA2			; get FPA2 MS byte
        RORB				; rotate carry flag into shift counter
					; and data bit into carry
        BEQ     L92D9			; branch when 8 shifts done
        BCC     L92CB			; do not add FPA1 if data bit = 0
	
        LDA     <FPA2+3			; add mantissa LSB
        ADDA    <FPA1+3
        STA     <FPA2+3

        LDA     <FPA2+2			; add mantissa 3SB
        ADCA    <FPA1+2
        STA     <FPA2+2
	
        LDA     <FPA2+1			; add mantissa 2SB
        ADCA    <FPA1+1
        STA     <FPA2+1
	
        LDA     <FPA2			; add mantissa MSB
        ADCA    <FPA1
	
L92CB   RORA				; rotate carry into MSB
        STA     <FPA2			; rotate FPA2 one bit to the right
        ROR     <FPA2+1
        ROR     <FPA2+2
        ROR     <FPA2+3
        ROR     <FPSByte
        CLRA				; clear carry flag
        BRA     L92AE			; keep looping

L92D9   RTS

; Unpack FP number pointed to by X into FPA1
XtoFPA1 LDD     1,X			; get 2 bytes of mantissa pointed to by X
        STA     <FP1SGN			; save packed mantiss aign byte
        ORA     #$80			; force bit 7 of MSB mantissa = 1
        STD     <FPA1			; save 2 bytes in MSB of FPA1
        LDB     <FP1SGN			; get packed FPA1 sign byte
        EORB    <FP0SGN			; EOR with FPA0 sign
        STB     <ResSGN			; new sign position if both signs alike
					; negative if signs different
        LDD     3,X			; get 2 LSB of mantissa
        STD     <FPA1+2			; save in FPA1
        LDA     ,X			; get exponent from X
        STA     <FP1EXP			; put exponent in FPA1 
        LDB     <FP0EXP			; get exponent of FPA0
        RTS

; calculate exponent for product of fpa0 & fpa1
; enter with exponent of fpa1 in A
L92F3   TSTA				; test exponent of FPA1
        BEQ     L930C			; purge return address and set FPA0 = 0
	
        ADDA    <FP0EXP			; add FPA1 exponent to FPA0 exponent
        RORA				; rotate carry into bit 7, bit 0 into carry
        ROLA				; set overflow flag
        BVC     L930C			; branch if exponent too large or small
	
        ADDA    #$80			; add $80 to bias exponent
        STA     <FP0EXP			; save new exponent
        BEQ     L930E			; set FPA0
	
        LDA     <ResSGN			; get mantissa sign
        STA     <FP0SGN			; save as mantissa sign of FPA0
        RTS

; if fpa0 = positive then 'ov' error 
; if fpa0 is negative then fpa0 = 0
L9307   LDA     <FP0SGN			; get sign of FPA0 mantissa
        COMA				; change sign
        BRA     L930E

L930C   LEAS    2,S			; drop return address
L930E   LBPL    L9182			; zero FPA0 mantissa, sign and exponent
L9312   JMP     >BasOVError		; generate overflow error

; Fast multiply by 10 and leave result in FPA0
FPA0mul10
	JSR     >FPA0toFPA1		; transfer FPA0 to FPA1
        BEQ     L9327			; branch if exponent = 0
	
        ADDA    #$02			; add 2 to exponent (*4)
        BCS     L9312			; overflow error if exponent > $FF
	
        CLR     <ResSGN			; clear result sign byte
        JSR     >L9116			; add FPA1 to FPA0 5 times
        INC     <FP0EXP			; add 1 to exponent (*10)
        BEQ     L9312			; overflow error if exponent > $FF
L9327   RTS

FPTen   FCB     $84,$20,$00,$00,$00	; Floating point constant 10.00

; Divide FPA0 by 10
FPA0div10
	JSR     >FPA0toFPA1		; move FPA0 to FPA1
        LDX     #FPTen			; point to constant 10 
        CLRB				; zero mantissa sign byte
FPA0divX
	STB     <ResSGN
        JSR     >XtoFPA0		; unpack FP from x to FPA1
        
	FCB	Skip2			; skip 2 bytes

; divide X by fpa0-leave normalized quotient in fpa0	
DivXbyFPA0   BSR	XtoFPA1			; get FP number from X to FPA1			

; arithmetic operation (/) jumps here. divide fpa1 by fpa0 (enter with
; exponent of fpa1 in acca and flags set by tsta)

; divide fpa1 by fpa0	
CmdDivide:
L933C   BEQ     BasD0Error		; /0 divide by zero error
        NEG     <FP0EXP			; ext exponent of reciprical pf divisor		
        BSR     L92F3			; calculate the exponent of the quotient
	
        INC     <FP0EXP			; increment exponent
        BEQ     L9312			; overflow error
        LDX     #FPA2 			; point X at mantissa of FPA2
					; hold temporary quotient in FPA2
        LDB     #$04			; 5 byte divide
        STB     <BasGenCount		; save byte counter
        LDB     #$01			; shift counter and temp quotient byte

* compare fpa0 mantissa to fpa1 mantissa and set carry flag if fpa1 >= fpa0	
L934F   LDA     <FPA0			; compare two MS byte of FPA0 and FPA1
        CMPA    <FPA1
        BNE     L9368			; branch if <>
	
        LDA     <FPA0+1			; compare #2 byte
        CMPA    <FPA1+1
        BNE     L9368			; branch if <>
        
	LDA     <FPA0+2			; compare byte #3
        CMPA    <FPA1+2
        BNE     L9368			; branch if <>
	
        LDA     <FPA0+3			; compare LS byte
        CMPA    <FPA1+3
        BNE     L9368			; branch if <>
	
        COMA				; set carry if FPA0 = FPA1
L9368   TFR     CC,A			; save carry flag in a, clear if FPA0 > FPA1
        ROLB				; rotate carry into temp quotient byte
        BCC     L9377			; carry will be set after 8 shifts
        STB     ,X+			; save temp quotient
        DEC     <BasGenCount		; ecrement byte counter
        BMI     L93A7			; branch if done
        BEQ     L93A3			; branch if last byte
	
        LDB     #$01			; reset shift count and temp quotient byte

L9377   TFR     A,CC			; restore carry flag
        BCS     L9389			; branch if FPA0 =< FPA1
	
L937B   ASL     <FPA1+3			; shift FPA1 mantissa one bit to the left
        ROL     <FPA1+2
        ROL     <FPA1+1
        ROL     <FPA1
        BCS     L9368			; branch if carry add one to partial quotient
        BMI     L934F			; if MSB of high order mantissa byte is set
					; check the magnatudes of FPA0 and FPA1
        BRA     L9368			; carry clear check another bit

; subtract FPA0 from FPA1, leave result in FPA1
L9389   LDA     <FPA1+3			; subtract LSB
        SUBA    <FPA0+3
        STA     <FPA1+3
	
        LDA     <FPA1+2			; then the next
        SBCA    <FPA0+2
        STA     <FPA1+2
	
        LDA     <FPA1+1			; and the next
        SBCA    <FPA0+1
        STA     <FPA1+1
	
        LDA     <FPA1			; subtract MSB
        SBCA    <FPA0
        STA     <FPA1
        BRA     L937B			; go shift FPA1

L93A3   LDB     #$40			; use only 2 bits of last (fifth) byte
        BRA     L9377			

L93A7   RORB				; shift carry (always set here)
        RORB				; into bit 5
        RORB				; move bits 1,0 to bits 7,6
        STB     <FPSByte		; save sub byte
        BSR     L93B6			; move mantissa of FPA2 to FPA0
        JMP     >VarNormFPA0 		; normalize FPA0

BasD0Error
	LDB     #ErrD0			; Divide by zero error
        JMP     >SysErr 

; Copy mantissa from FPA2 to FPA0
L93B6   LDX     <FPA2			; top 2 bytes
        STX     <FPA0
        LDX     <FPA2+2			; bottom two bytes
        STX     <FPA0+2
        RTS

; copy a packed FP number from X to FPA0
XtoFPA0 PSHS    A			; save A
        LDD     1,X			; Get top 2 mantiss bytes
        STA     <FP0SGN			; save top byte of mantissa as sign
        ORA     #$80			; unpack MSB
        STD     <FPA0			; save unpacked top 2 bytes
        CLR     <FPSByte		; clear mantissa sub byte
        LDB     ,X			; get exponent to B
        LDX     3,X			; move last 2 mantissa bytes
        STX     <FPA0+2			
        STB     <FP0EXP			; save exponent
        PULS    A,PC			; restore and return

FPA0toFPA4   
	LDX     #BasVarFPAcc4 		; point X at mantissa of FPA4
        BRA     PackFPA0toX		; move from FPA0 to FPA4

FPA0toFPA3   LDX     #BasVarFPAcc3 		; point X to mantissa of FPA3

        FCB	Skip2			; skip 2 bytes
L93DE   LDX	<BasTempVarDesc		; point X to variable descriptor in BasTempVarDesc	

; pack fpa0 and move it to address in x
PackFPA0toX
	LDA     <FP0EXP			; copy exponent
        STA     ,X
        LDA     <FP0SGN			; get mantissa sign bit
        ORA     #$7F			; mask bottom 7 bits
        ANDA    <FPA0			; add bit 7 of mantissa sign into bit 7 of MSB
        STA     1,X			; save MSB
        LDA     <FPA0+1			; move 2nd mantissa byte
        STA     2,X			
        LDU     <BasVarAssign16		; move bottom 2 mantissa bytes
        STU     3,X
        RTS

; transfer FPA1 to FPA0, return with mantissa sign in A
FPA1toFPA0   
	LDA     <FP1SGN			; copy mantissa sign
L93F7   STA     <FP0SGN			
        LDX     <FP1EXP
        STX     <FP0EXP			; copy exponent + MSB
        CLR     <FPSByte
        LDA     <FPA1+1			; copy 2nd byte
        STA     <FPA0+1
        LDA     <FP0SGN			; get mantissa sign
        LDX     <FPA1+2			; copy last 2 bytes
        STX     <FPA0+2	
        RTS

; transfer FPA0 to FPA1
FPA0toFPA1   
	LDD     <FP0EXP			; copy exponent + 1st byte
        STD     <FP1EXP
        LDX     <FPA0+1			; copy next 2 bytes
        STX     <FPA1+1
        LDX     <FPA0+3			; and next 2
        STX     <FPA1+3
        TSTA				; set flags according to exponent
        RTS

; Test FPA0, set B dependent on FPA0 :-
;	FPA0 = 0	B = $00
;	FPA0 > 0	B = $01
;	FPA0 < 0	B = $FF
;
TestFPA0   
	LDB     <FP0EXP			; get exponent
        BEQ     L9424			; branch if FPA0 = 0
	
L941C   LDB     <FP0SGN			; get sign of mantissa
L941E   ROLB				; bit 7 to carry
        LDB     #$FF			; assume -ve
        BCS     L9424			; carry set, so -ve 
        NEGB				; flag +ve
L9424   RTS

;Basic SGN function
CmdSGN:
L9425   BSR     TestFPA0		; Test FPA0 for +ve, 0, -ve
;Convert signed number in B to FP number in FPA0
L9427   STB     <FPA0			; save B in FPA0
        CLR     <FPA0+1			; clear 2nd byte of mantissa
        LDB     #$88			; exponent required if FPA0 is to be integer
L942D   LDA     <FPA0			; get MSB of mantissa
        SUBA    #$80			; set carry if +ve mantissa
L9431   STB     <FP0EXP			; save exponent
        LDD     <DBZero	; zero out bottom half of mantissa
        STD     <FPA0+2
        STA     <FPSByte
        STA     <FP0SGN
        JMP     >L9161			; normalize FPA0

; Basic ABS
CmdABS:
L943E   CLR     <FP0SGN			; force FP0 sign +ve
        RTS

; compare a packed floating point number pointed to by x to an unpacked fp number in fpa0. 
; return
; zero flag set and B = $00, if equal; 
; 		    B = $01, if fpa0 > x
;		    B = $ff, if fpa0 < x
;
L9441   LDB     ,X			; check exponent of fp at X
        BEQ     TestFPA0		; branch if FPA0 = 0
	
        LDB     1,X			; get MSB of mantissa of fp at X
        EORB    <FP0SGN			; eor with sign of FPA0
        BMI     L941C			; branch if signs not equal

; compare fpa0 with fp number pointed to by (x). 
; fpa0 is normalized, (x) is packed.	
FPA0cmpX
	LDB     <FP0EXP			; get exponent of FPA0
        CMPB    ,X			; compare with exponent of fp at X
        BNE     L946E			; branch if not equal
	
        LDB     1,X			; get MSB of fp at X
        ORB     #$7F			; keep only sign bit
        ANDB    <FPA0			; and the bottom 7 bits of FPA0 into B
        CMPB    1,X			; compare bottom 7 bits of mantissa
        BNE     L946E			; branch if not equal
	
        LDB     <FPA0+1			; compare 2nd byte of mantissa
        CMPB    2,X
        BNE     L946E			; branch if not equal
        
	LDB     <FPA0+2			; compare 3rd byte of mantissa
        CMPB    3,X
        BNE     L946E			; branch if not equal
	
        LDB     <FPA0+3			; subtract LSB of fp at X 
        SUBB    4,X			; from LSB of FPA0
        BNE     L946E			; branch if not equal
        RTS

L946E   RORB				; shift carry bit to bit 7, carry set if 
					; FPA0 < fp at X
        EORB    <FP0SGN			; toggle size comparison bit if FP0 is -ve
        BRA     L941E			; go set B according to comparison

; de-normalize fpa0 : 
;	shift the mantissa until the binary point is to the right of the least 
;	significant byte of the mantissa
L9473   LDB     <FP0EXP			; get exponent of FPA0
        BEQ     L94B4			; Zero mantissa if FPA0 = 0
	
        SUBB    #$A0			; subtract $A0 from exponent this will yeild
					; the number of shifts required to de-normalize FPA0
					; when the exponent of FPA0 = 0 then the binary point 
					; will be to the right of the mantissa.
        LDA     <FP0SGN			; test sign of FPA0 mantissa
        BPL     L9482			; branch if +ve
	
        COM     <FPCARY			; complement carry in byte		
        JSR     >L91C4			; negate mantissa of FPA0

L9482   LDX     #FP0EXP			; point X at FPA0 
        CMPB    #-8			; exponent differenc < -8
        BGT     L948F			; yes
	
        JSR     >L91F7			; shift FPA0 right until exponent = $A0
        CLR     <FPCARY			; clear carry in byte
        RTS

L948F   CLR     <FPCARY			; clear carry in byte
        LDA     <FP0SGN			; get sign of mantissa
        ROLA				; rotate it into carry flag
        ROR     <FPA0			; rotate carry (mantissa sign) into bit 7 of 
					; LSB of mantissa
        JMP     >L9203			; de-normalize FPA0

; Basic INT function
; the int statement will "denormalize" fpa0 - that is it will shift the binary point
; to the extreme right of the mantissa to force its exponent to be $ao. 
; once this is done the mantissa of fpa0 will contain the four least significant
; bytes of the integer portion of fpa0. at the conclusion of the de-normalization
; only the integer portion of fpa0 will remain.
CmdINT:
L9499   LDB     <FP0EXP			; get exponent of FPA0
        CMPB    #$A0			; largest possible integer exponent
        BCC     L94BC			; return if FPA0 > 32768
	
        BSR     L9473			; shift binary point to the right of LSB of the mantissa
        STB     <FPSByte		; B=0, zero out sub byte	
        LDA     <FP0SGN			; get mantissa sign
        STB     <FP0SGN			; force sign to be +ve
        SUBA    #$80			; set carry if mantissa 
        LDA     #$A0			; get denmomalized exponent
        STA     <FP0EXP			; save it in FPA0 exponent
        LDA     <FPA0+3			; get LSB of FPA0
        STA     <BasDelim1		; save it
        JMP     >L9161			; normalize FPA0

L94B4   STB     <FPA0			; load mantissa with contents of B		
        STB     <FPA0+1
        STB     <FPA0+2
        STB     <FPA0+3
L94BC   RTS

; convert ASCII string to FP
L94BD   LDX     <DBZero	; X = 0
        STX     <FP0SGN			; zero out FPA0 & sign flag
        STX     <FP0EXP
        STX     <FPA0+1
        STX     <FPA0+2
        STX     <Eval47			; initialize exponent & exponent sign flag to zero
        STX     <Eval45			; initialize decimal counter & decimal PT flag to zero
        BCS     L9534			; if carry is zet (numeric character) assume A 
					; contains first numeric character, sign is +ve 
        CMPA    #'&'			; check for leading '&' sign
        LBEQ    L9C1B			; yes branch, hex or octal
	
        CMPA    #'-'			; check for leading '-' sign
        BNE     L94DB			; no : skip
        COM     <COEFCT			; toggle sign, 0=+ve, $FF=-ve
        BRA     L94DF			; interpret rest of string

L94DB   CMPA    #'+'			; check for leading '+' sign
        BNE     L94E3			; branch if not 
	
L94DF   JSR     <BasChrGet		; get next character from basic
        BCS     L9534			; branch if numeric
	
L94E3   CMPA    #'.'			; decimal point '.' character?
        BEQ     L950F			; yes, skip
	
        CMPA    #'E'			; check for 'E' character, shorthand for scientific notation
        BNE     L9513			; nope skip

; evaluate the exponent of expinatial format	
        JSR     <BasChrGet		; get next character 
        BCS     L9553			; branch if numeric
	
        CMPA    #DTokMINUS		; minus token?
        BEQ     L9501			; yes
        
	CMPA    #'-'			; ASCII minus?
        BEQ     L9501			; yes
	
        CMPA    #DTokPLUS		; plus token?
        BEQ     L9503			; yes
	
        CMPA    #'+'			; ASCII plus?
        BEQ     L9503			; yes
	
        BRA     L9507			; branch if no sign found

L9501   COM     <Eval48			; set exponent sign flag to -ve

; strip a decimal number from basic line, convert it to binary in Eval47
L9503   JSR     <BasChrGet		; get next char
        BCS     L9553			; branch if numeric
	
L9507   TST     <Eval48			; check exponent sign flag
        BEQ     L9513			; branch if +ve
	
        NEG     <Eval47			; negate value of exponent
        BRA     L9513			

L950F   COM     <Eval46			; toggle decimal point flag and interpret another character
        BNE     L94DF			; if <> 0 terminate interpretation, second decimal point!

; adjust fpa0 for the decimal exponent in Eval47	
L9513   LDA     <Eval47			; get exponent		
        SUBA    <Eval45			; subtract no of places to the right of decimal point		
        STA     <Eval47			; resave it
        BEQ     L952D			; escape routine if adjusted exponent is zero
        BPL     L9526			; branch if +ve exponent
	
L951D   JSR     >FPA0div10		; Divide FPA0 by 10
        INC     <Eval47			; increment exponent counter, multiply by 10
        BNE     L951D			; keep multiplying
        BRA     L952D			; exit routine

L9526   JSR     >FPA0mul10		; Multiply FPA0 by 10
        DEC     <Eval47			; decrement exponent, divide by 10
        BNE     L9526			; keep multiplying
	
L952D   LDA     <COEFCT			; get sign flag
        BPL     L94BC			; return if +ve
        JMP     >ChangeFPA0Sign			; toggle mantissa sign of FPA0 if -ve

;multiply fpa0 by ten and add A to the result
L9534   LDB     <Eval45			; get the right decimal counter	
        SUBB    <Eval46			; subtract the decimal point flag from it
        STB     <Eval45			; if decimal point flag is 0 then do nothing, 
					; if it is -1 then right decimal counter is incremented by 1
        
	PSHS    A			; save new digit on stack
        JSR     >FPA0mul10		; multiply FPA0 by 10
        PULS    B			; restore digit from stack
        SUBB    #'0'			; convert it to binary 
        BSR     L9547			; add B to FPA0
        BRA     L94DF			; get next character from basic

L9547   JSR     >FPA0toFPA3		; pack FPA0 and save in FPA3
        JSR     >L9427			; convert B to FP in FPA0
        LDX     #BasVarFPAcc3 		; point to FPA3
        JMP     >AddXtoFPA0			; Add FPA0 to FPA3

; multiply Eval47 by 10 and add to ascii number in A, save binary result in Eval47
L9553   LDB     <Eval47				
        ASLB				; times 2
        ASLB				; times 4
        ADDB    <Eval47			; add 1 (same as times 5)
        ASLB				; times 2 = * 10
        SUBA    #'0'			; convert digit to binary
        PSHS    B			; save result on stack
        ADDA    ,S+			; add it to digit
        STA     <Eval47			; save it
        BRA     L9503			; interpret next character
	
; FP constants for printing.
D9564   FCB     $9B,$3E,$BC,$1F,$FD		; 99999999.9
D9569   FCB     $9E,$6E,$6B,$27,$FD		; 999999999
D956E   FCB     $9E,$6E,$6B,$28,$00		; 1E + 09

; Prints " IN "<lineno> for error messages.....
L9573   LDX     #MessIn-1		; point X to " IN " message
        BSR     L9584			; go output it to console
        LDD     <BasCurrentLine		; get current line number

; Convert number in D to decimal and output on console
TextOutNum16:	
L957A   STD     <FPA0			; save integer to print
        LDB     #$90			; required exponent if top half of D = integer
        COMA				; set carry flag force +ve mantissa
        JSR     >L9431			; Zero bottom half and sign of FPA0 
					; then save exponent and normalize it
TextOutNumFPA0:
        BSR     L9587			; convert FP number to ASCII string
L9584   JMP     >TextOutString 		; output it to the console

; convert FP number to ASCII string
L9587   LDU     #BasBuffer+3		; point U to buffer which will not cause string to
					; be stored in string space.
L958A   LDA     #' '			; space default sign for +ve no
        LDB     <FP0SGN			; get sign of FPA0
        BPL     L9592			; branch if +ve
        LDA     #'-'			; get '-' for sign
	
L9592   STA     ,U+			; store char in string
        STU     <CoefPTR		; save buffer pointer	
        STA     <FP0SGN			; save sign (in ASCII)
        LDA     #'0'			; ASCII '0' if exponent 0
        LDB     <FP0EXP			; get exponent
        LBEQ    L9666			; branch if FPA0 = 0
	
        CLRA				; base 10 exponenet = 0 for FP number > 1
        CMPB    #$80			; check exponent
        BHI     L95AD			; branch if FP number > 1

; if fpa0 < 1.0, multiply it by 1e+09 to speed up the conversion process	
        LDX     #D956E			; point to 1e+09 constant
        JSR     >XtimesFPA0		; multiply it
			
        LDA     #-9			; base 10 exponent = -9
L95AD   STA     <Eval45			; save exponent

; Pseudo - normalize the fp number to a value in the range
; of 999,999,999 ro 99,999,999.9 
; This is the largest number range in which all of the digits are
; significant which can be displayed without using scientific notation
L95AF   LDX     #D9569			; FP constant 999,999,999
        JSR     >FPA0cmpX		; compare to FPA0 
        BGT     L95C6			; branch if FPA0 greater

L95B7   LDX     #D9564			; FP constant 99,999,999.9
        JSR     >FPA0cmpX		; compare to FPA0
        BGT     L95CD			; branch if FPA0 greater
	
        JSR     >FPA0mul10		; multiply FPA0 by 10
        DEC     <Eval45			; subtract 1 from decimal offset
        BRA     L95B7			; psudo: normalize some more

L95C6   JSR     >FPA0div10		; Divide FPA0 by 10
        INC     <Eval45			; add 1 to decimal offset
        BRA     L95AF			; psudo: normalize some more	

L95CD   JSR     >L90FD			; add 0.5 to FPA0 (round off)
        JSR     >L9473			; convert FPA0 to an integer
        LDB     #$01			; default decimal point flag force immediate decimal
        LDA     <Eval45			; get base 10 exponent and add 10 to it
        ADDA    #9+1			; number 'normalized' to 10 decimal places
        BMI     L95E4			; branch if number < 1.0
	
        CMPA    #9+2			; 9 places may be displaed without scientific notation			
        BCC     L95E4			; branch if scientific notation needed
	
        DECA				; subtract 1 from modified base 10 exponent counter
        TFR     A,B			; save it to B, decimal point flag
        LDA     #$02			; force exponent = 0, don't use scientific notation
	
L95E4   DECA				; subtract 2 (without affecting carry) from
        DECA				; base 10 exponent
        STA     <Eval47			; save exponent, zero = don't display scientific
        STB     <Eval45			; decimal point flag, no of places to left of point.
        BGT     L95F9			; branch if >= 1.0
	
        LDU     <CoefPTR		; point U to string buffer
        LDA     #'.'			; store a period
        STA     ,U+
        TSTB				; check decimal point flag
        BEQ     L95F9			; branch if nothing to the left of the point
	
        LDA     #'0'			; store a zero
        STA     ,U+			; 

; convert fpa0 into a string of ascii digits	
L95F9   LDX     #PowersOf10		; point to powers of 10 table 
        LDB     #$00+$80		; initialize digit counter

; bit 7 set is used to indicate that the power of 10 mantissa is negative. 
; when you 'add' a negative mantissa, it is the same as subtracting a positive one
; and bit 7 of accb is how the routine knows that a 'subtraction' is occuring.			
L95FE   LDA     <FPA0+3			; add mantissa LSB of FPA0 and fp at X 
        ADDA    3,X
        STA     <FPA0+3
	
        LDA     <FPA0+2			; add mantissa #3 byte of FPA0 and fp at X 
        ADCA    2,X
        STA     <FPA0+2
	
        LDA     <FPA0+1			; add mantissa #2 byte of FPA0 and fp at X 
        ADCA    1,X
        STA     <FPA0+1
	
        LDA     <FPA0			; add mantissa MSB of FPA0 and fp at X 
        ADCA    ,X
        STA     <FPA0
	
        INCB				; ad one to digit counter
        RORB				; get bit 7 into carry
        ROLB				; set overflow flag, and branch if carry
        BVC     L95FE			; +ve mantissa or carry = 0 and -ve mantissa 
        BCC     L9620			; branch if -ve mantissa
	
        SUBB    #10+1			; take 9's compliment if adding mantisa				
        NEGB				
	
L9620   ADDB    #$2F			; add ASCII offset to digit
        LEAX    4,X			; move to next power of 10 mantissa
        TFR     B,A			; save digit in A	
        ANDA    #$7F			; mask off bit 7 (add/subtract flag).
	
        STA     ,U+			; store digit in string
        DEC     <Eval45			; decrement decimal point flag
        BNE     L9632			; branch if not time for decimal point
        LDA     #'.'			; load point character
        STA     ,U+			; output it

L9632   COMB				; toggle bit 7 add/subtract flag
        ANDB    #$80			; mask off all but flag
        CMPX    #PowersOf10End		; end of table? 		
        BNE     L95FE			; no do next 

; blank trailing zeros and store exponent if any
L963A   LDA     ,-U			; get the last character, move pointer back	
        CMPA    #'0'			; was last character a zero?
        BEQ     L963A			; yes, ignore it
	
        CMPA    #'.'			; was it a point?
        BNE     L9646			; no, not a decimal point
	
        LEAU    -1,U			; step over decimal point
L9646   LDA     #'+'			; ASCII plus sign?
        LDB     <Eval47			; get scientific notion exponent
        BEQ     L9668			; branch if not scientific notation
        BPL     L9651			; branch if +ve exponent
        LDA     #'-'			; ASCII - sign
        NEGB				; negate exponent if negative
	
L9651   STA     2,U			; strore exponent sign in string
        LDA     #'E'			; ASCII 'E' for scientific notation
        STA     1,U			; save it in string
        LDA     #'0'-1			; initialize A to ASCII zero

; CONVERT BINARY VALUE IN ACCB TO DECIMAL ASCII NUMBER (< 100) IN ACCD	
L9659   INCA				; add 1 to 10's digit of exponent 
        SUBB    #$0A			; subtract 10 from B
        BCC     L9659			; add 1 to 10's if no carry
	
        ADDB    #$3A			; convert units to ASCII
        STD     3,U			; save exponent in string
        CLR     5,U			; clear last byte
        BRA     L966A			; go reset pointer

L9666   STA     ,U			; save last char
L9668   CLR     1,U			; save a null terminator
L966A   LDX     #BasBuffer+3		; point back to beginning of string
L966D   RTS

FPPointFive   
	FCB     $80,$00,$00,$00,$00		; Floating point 0.5

; Table of unnormalized powers of 10.
PowersOf10
	FCB     $FA,$0A,$1F,$00		; -100000000
        FCB     $00,$98,$96,$80		; +10000000
        FCB     $FF,$F0,$BD,$C0		; -1000000
        FCB     $00,$01,$86,$A0		; +100000
        FCB     $FF,$FF,$D8,$F0		; -10000
        FCB     $00,$00,$03,$E8		; +1000
        FCB     $FF,$FF,$FF,$9C		; -100
        FCB     $00,$00,$00,$0A		; +10 
        FCB     $FF,$FF,$FF,$FF		; -1
PowersOf10End

; basic command SQR
CmdSQR:
L9697   JSR     >FPA0toFPA1		; move FPA0 to FPA1
        LDX     #FPPointFive		; point X at fp constant 0.5
        JSR     >XtoFPA0		; copy it to FPA0
	
; arithmetic operator for exponentiation jumps here. 
; the formula used to evaluate exponentiation is :
;	a**x=e**(x ln a) = e**(fpa0*ln(fpa1)), e=2.7182818	
; basic EXP command
CmdExponet:
L96A0   BEQ     CmdEXP  		; do a natural exponention if exponent = 0 
        TSTA				; check value being exponated
        BNE     L96AE			; and branch if it is <> 0
	
        LDA     <FP0SGN			; get mantissa sign
        LBMI    BasD0Error		; -ve, division by zero error
        JMP     >L9183			; FPA0 = 0 if raising zero to a power

L96AE   LDX     #BasVarFPAcc5		; point X at FPA5
        JSR     >PackFPA0toX		; pack FPA0 into FPA5	 
        
	CLRB				
        LDA     <FP1SGN			; check the sign of argument
        BPL     L96C9			; branch if +ve
	
        JSR     >CmdINT 		; convert exponent to integer
        LDX     #BasVarFPAcc5 		; point X to FPA5 (original exponent)
        LDA     <FP1SGN			; get mantissa sign of FPA1 (argument)
        JSR     >FPA0cmpX		; compare FPA0 and fp at X
        BNE     L96C9			; branch if not equal
	
        COMA				; toggle mantissa sign, force +ve
        LDB     <BasDelim1		; get LSB of integer value of exponent (result sign)
	
L96C9   JSR     >L93F7			; copy FPA1 to FPA0, A = mantissa sign
        PSHS    B			; save result sign on stack
        JSR     >CmdLOG 		; get natural logarithm of FPA0
        LDX     #BasVarFPAcc5 		; point X at FPA5
        JSR     >XtimesFPA0		; multiply FPA0 by FPA5
        BSR     CmdEXP  		; calculate E**(FPA0)
        PULS    A			; get result sign from stack
        RORA				; branch if -ve
        BCC     L966D
	
ChangeFPA0Sign   LDA     <FP0EXP			; get exponent of FPA0
        BEQ     L96E4			; branch if FPA0 = 0
        COM     <FP0SGN			; toggle mantissa sign of FPA0
L96E4   RTS

; Correction factor for exponential function 
D96E5   FCB     $81,$38,$AA,$3B,$29	; 1.44269504 (CF)

; Tchebyshev modified Taylor series coefficients for E**X
 
D96EA   FCB     $07			; Eight coefficeints
        FCB     $71,$34,$58,$3E,$56	; 2.14987637E-05: 1/(7!*(CF**7))
        FCB     $74,$16,$7E,$B3,$1B	; 1.4352314E-04 : 1/(6!*(CF**6))
        FCB     $77,$2F,$EE,$E3,$85	; 1.34226348E-03: 1/(5!*(CF**5))
        FCB     $7A,$1D,$84,$1C,$2A	; 9.61401701E-03: 1/(4!*(CF**4))
        FCB     $7C,$63,$59,$58,$0A	; 0.0555051269 : 1/(3!*(CF**3))
        FCB     $7E,$75,$FD,$E7,$C6	; 0.240226385 : 1/(2!*(CF**2))
        FCB     $80,$31,$72,$18,$10	; 0.693147186 : 1/(1!*(CF**1))
        FCB     $81,$00,$00,$00,$00	; 1

; EXP ( E**X)
;The exponential function is evaluated by first multiplying the argument by a 
; correction factor (cf). 
; After this is done, an argument >= 127 will yield a zero result (no underflow) 
; for a negative argument or an 'ov' (overflow) error for a positive argument. 
; The polynomial coefficients are modified to reflect the cf multiplication at 
; the start of the evaluation process.
CmdEXP:
L9713   LDX     #D96E5			; point X at correction factor fp constant
        BSR     L974F			; multiply FPA0 by correction factor
        JSR     >FPA0toFPA3		; copy FPA0 to FPA3
	
        LDA     <FP0EXP			; get exponent of FPA0
        CMPA    #$88			; compare to maximum value
        BCS     L9724			; branch if FPA0 < 128
L9721   JMP     >L9307			; set FPA0 or OV error

L9724   JSR     >CmdINT 		; convert FPA0 to integer
        LDA     <BasDelim1		; get LSB of integer
        ADDA    #$81			; this also adds the $80 bias required below  
        BEQ     L9721			; was argument = 127 if so OV error,
        
	DECA				; decrement by 1 because $81 was used above
        PSHS    A			; save exponent of integer portion on stack
        LDX     #BasVarFPAcc3 		; point X to FPA3
        JSR     >SubtractFPA0fromX	; subtract FPA0 from fp at X, 
					; get fractional part of argument
        LDX     #D96EA			; point X at coefficients
        BSR     L9752			; evaluate polynomial fraction part
	
        CLR     <ResSGN			; force mantissa to be +ve
        PULS    A			; restore integer exponent 
        JSR     >L92F3			; calculate the new exponent by adding exponents together
        RTS

; expand a polynomial of the form
;	aq+bq**3+cq**5+dq**7.... where 
;	q = fpa0
;	and the x register points to a table of coefficients a,b,c,d....
L9743   STX     <CoefPTR		; save coefficient table pointer
        JSR     >FPA0toFPA3		; copy FPA0 to FPA3
        BSR     L974F			; multiply FPA0 by FPA3
        BSR     L9754			; expand polynomial
        LDX     #BasVarFPAcc3 		; point X at FPA3
L974F   JMP     >XtimesFPA0		; Multiply by FPA0

; calculate the value of an expanded polynomial expression. 
; enter with (x) pointing to a table of coefficients, 
; the first byte of which is the number of (coefficients-1) 
; followed by that number of packed floating point numbers. 
; the polynomial is evaluated as follows: 
; 	value = (((fpa0*y0+y1)*fpa0+y2)*fpa0...yn)
;
L9752   STX     <CoefPTR		; save table pointer
L9754   JSR     >FPA0toFPA4		; copy FPA0 to FPA4

        LDX     <CoefPTR		; reload table pointer
        LDB     ,X+			; get number of coefficients
        STB     <COEFCT			; save coeficcient count
        STX     <CoefPTR		; save new pointer
	
L975F   BSR     L974F			; multiply fp at X by FPA0
        LDX     <CoefPTR		; get coeficient pointer	
        LEAX    5,X			; move to next coefficeint
        STX     <CoefPTR		; save pointer
        JSR     >AddXtoFPA0			; add fp at X to FPA0
        LDX     #BasVarFPAcc4		; point X at FPA4 
        DEC     <COEFCT			; dec coeficient counter
        BNE     L975F			; loop if more
        RTS

; Basic RND function
CmdRND:
L9772   JSR     >TestFPA0		; Test FPA0
        BMI     L9798			; branch if FPA0 -ve
        BEQ     BasRandom8 		; branch if FPA0 zero
	
        BSR     L978B			; convert FPA0 to an integer
        JSR     >FPA0toFPA3		; transfer FPA0 to FPA3
        BSR     BasRandom8 		; get an integer in FPA0 < 1.0
        
	LDX     #BasVarFPAcc3 		; point X at FPA3
        BSR     L974F			; multiply FPA3 by FPA0
	
        LDX     #FPOnePointZero		; point X at fp 1.0
        JSR     >AddXtoFPA0		; add 1.0 to FPA0
L978B   JMP     >CmdINT 		; convert FPA0 to integer

; calculat a random number in the range 0.0 < X < 1.0
BasRandom8:
L978E   LDX     BasRandomSeed+1		; move variable random seed 
        STX     <FPA0			; to mantissa of FPA0
        LDX     BasRandomSeed+3	
        STX     <FPA0+2
	
L9798   LDX     >RandomSeed		; move fixed random seed
        STX     <FPA1			; to mantissa of FPA1
        LDX     >RandomSeed+2
        STX     <FPA1+2
        JSR     >MultiplyFPA0byFPA1	; multiply FPA0 x FPA1
			
        LDD     <BasRndData+2		; get 2 lowest order product bytes
        ADDD    #$658B			; add a constant
        STD     BasRandomSeed+3		; save new low order bytes random number seed
        STD     <FPA0+2			; save new low order bytes of FPA0 mantissa
	
        LDD     <BasRndData		; get 2 more low order product bytes
        ADCB    #$B0			; add 2 constant bytes
        ADCA    #$05
        STD     BasRandomSeed+1		; save new high order bytes random number seed
        STD     <FPA0			; save new high order bytes of FPA0 mantissa
        
	CLR     <FP0SGN			; force FPA0 mantissa +ve
        LDA     #$80			; set FPA0 biased exponent
        STA     <FP0EXP
        LDA     <FPA2+2			; get a byte from FPA2, more randomness
        STA     <FPSByte		; save as sub byte
        JMP     >VarNormFPA0 		; normalize FPA0

; fixed random seed
RandomSeed   
	FCB     $40,$E6,$4D,$AB

; Basic COS function
; the value of cos(x) is determined by the trig identity cos(x)=sin((pi/2)+x)
CmdCOS:
L97CB   LDX     #PIdiv2			; point X to PI/2
        JSR     >AddXtoFPA0		; Add it to FPA0
; fall through into SIN......

; Basic SIN function
; the sin function requires an argument in radians and will repeat itself every 2*pi radians. 
; the argument is divided by 2*pi and only the fractional part is retained. 
; since the argument was divided by 2*p1, the coefficients must be multiplied by the 
; appropriate power of 2*pi.
;
; sin is evaluated using the trigonometric identities below:
; sin(x)=sin(pi-x) & -sin(pi/2-x)=sin((3*pi)/2+x)	
CmdSIN:
L97D1   JSR     >FPA0toFPA1		; copy FPA0 to FPA1
        LDX     #PImul2			; point to 2PI constant
        LDB     <FP1SGN			; get mantissa sign of FPA1
        JSR     >FPA0divX		; divide FPA0 by 2PI
        JSR     >FPA0toFPA1		; copy FPA0 it to FPA1
        BSR     L978B			; convert FPA0 to an integer
	
        CLR     <ResSGN			; set result sign +ve
        LDA     <FP1EXP			; get exponent of FPA1
        LDB     <FP0EXP			; get exponent of FPA0
        JSR     >CmdMinus 		; subtract FPA0 from FPA1

;now fpa0 contains only the fractional part of argument/2*pi	
        LDX     #Quater			; point X at fp constant 0.25
        JSR     >SubtractFPA0fromX	; subtract FPA0 from 0.25
        
	LDA     <FP0SGN			; get mantissa sign of FPA0
        PSHS    A			; save on stack
        BPL     L97FF			; branch if mantissa +ve
	
        JSR     >L90FD			; add 0.5 PI to FPA0
        LDA     <FP0SGN			; get FPA0 mantissa sign
        BMI     L9802			; branch if -ve
	
        COM     <BasRelateFlag		; com if +(3*pi)/2 >= argument >+ pi/2 (quadrant flag)
L97FF   JSR     >ChangeFPA0Sign		; toggle mantissa sign of FPA0
L9802   LDX     #Quater			; point at 0.25 fp constant
        JSR     >AddXtoFPA0		; add it to FPA0	
        PULS    A			; get old mantissa sign	
        TSTA				; test it
        BPL     L9810			; branch if old mantissa sign was +ve
        JSR     >ChangeFPA0Sign			; toggle mantissa sign
L9810   LDX     #TaylorSINConst		; point to constants
L9813   JMP     >L9743			; go calculate polynomial value

; Basic TAN function
; the value of tan(x) is determined by the trig identity tan(x)=sin(x)/cos(x)
CmdTAN:
L9816   JSR     >FPA0toFPA3		; copy FPA0 to FPA3
        CLR     <BasRelateFlag		; reset quadrant flag
        BSR     CmdSIN  		; go calculate sine of FPA0
	
        LDX     #BasVarFPAcc5 		; copy FPA0 (sin(x)) to FPA5
        JSR     >PackFPA0toX
	
        LDX     #BasVarFPAcc3 		; recover X from FPA3
        JSR     >XtoFPA0
	
        CLR     <FP0SGN			; make mantissa +ve
        LDA     <BasRelateFlag		; get quadrant flag, COS is -ve in quadrants 2,3
        BSR     L983B			; calculate cosine of FPA0
        
	TST     <FP0EXP			; check exponent of FPA0
        LBEQ    BasOVError		; overflow error if cos(X)=0
        LDX     #BasVarFPAcc5 		; point X to FPA5 (sin(x))
L9838   JMP     >DivXbyFPA0		; divide X by FPA0 sin(x)/cos(x)			

L983B   PSHS    A			; save sign flag on stack
        BRA     L97FF			; go calculate cosine

; Trig constants
PIdiv2  FCB     $81,$49,$0F,$DA,$A2	; PI/2 1.57079633
PImul2  FCB     $83,$49,$0F,$DA,$A2	; PI*2 6.28318531
Quater  FCB     $7F,$00,$00,$00,$00	; 0.25 1/4

; Modified Taylor series SIN coefficients
TaylorSINConst   
	FCB     6-1			; 6 coefficients
        FCB     $84,$E6,$1A,$2D,$1B	; -((2*PI)**11)/11!
        FCB     $86,$28,$07,$FB,$F8	; ((2*PI)**9)/9!
        FCB     $87,$99,$68,$89,$01	; -((2*PI)**7)/7!
        FCB     $87,$23,$35,$DF,$E1	; ((2*PI)**5)/5!
        FCB     $86,$A5,$5D,$E7,$28	; -((2*PI)**3)/3!
        FCB     $83,$49,$0F,$DA,$A2	; 2*PI
        
	FCB     $A1,$54,$46,$8F,$13	; unused / unknown ?
        FCB     $8F,$52,$43,$89,$CD	; Microsoft encrypted watermark from PET basic!!

; Basic ATN function
; a 12 term taylor series is used to evaluate the arctan expression.  
; two different formuli are used to evaluate the expression depending upon
;  whether or not the argument squared is > or < 1.0
;
; if x**2<1 then atn=x-(x**3)/3+(x**5)/5-(x**7)/7...........
; if x**2>=1 then atn=pi/2-(1/x-1/((x**3)*3)+(1/((x**5)*5)-.........)
CmdATN:
L9877   LDA     <FP0SGN			; get sign of mantissa
        PSHS    A			; save it on the stack
        BPL     L987F			; branch if sign +ve
        BSR     L98A2			; change the sign of FPA0
	
L987F   LDA     <FP0EXP			; get FPA0 exponent
        PSHS    A			; save it on stack
        CMPA    #$81			; is FPA0 < 1.0 ?
        BCS     L988C			; yes
	
        LDX     #FPOnePointZero		; point X at FP 1.0
        BSR     L9838			; get reciprocal of FPA0
	
L988C   LDX     #TchebyshevTaylor	; point at coeficient table
        BSR     L9813			; expand polynomial
	
        PULS    A			; get exponent of argument
        CMPA    #$81			; was argument < 1.0 ?
        BCS     L989D			; yes
	
        LDX     #PIdiv2			; point X at PI/2 constant
        JSR     >SubtractFPA0fromX	; subtract FPA0 from PI/2
L989D   PULS    A			; restore argument sign
        TSTA				; set flags acording to argument sign
        BPL     L98A5			; return if argument was +ve
L98A2   JMP     >ChangeFPA0Sign		; change mantissa sign on FPA0

L98A5   RTS

; Tchebyshev modified Taylor series coefficients for ArcTangent

TchebyshevTaylor   
	FCB     $0B			; Twelve coefficients
        FCB     $76,$B3,$83,$BD,$D3	; -6.84793912E-04 1/23
        FCB     $79,$1E,$F4,$A6,$F5	; +4.85094216E-03 1/21
        FCB     $7B,$83,$FC,$B0,$10	; -0.0161117018 1/19
        FCB     $7C,$0C,$1F,$67,$CA	; +0.0342096381 1/17
        FCB     $7C,$DE,$53,$CB,$C1	; -0.0542791328 1/15
        FCB     $7D,$14,$64,$70,$4C	; +0.0724571965 1/13
        FCB     $7D,$B7,$EA,$51,$7A	; -0.0898023954 1/11
	FCB	$7D,$63,$30,$88,$7E	; +0.110932413 1/9
        FCB     $7E,$92,$44,$99,$3A	; -0.142839808 1/7
	FCB     $7E,$4C,$CC,$91,$C7	; +0.199999121 1/5
        FCB     $7F,$AA,$AA,$AA,$13	; -0.333333316 1/3
        FCB     $81,$00,$00,$00,$00	; +1.000000000 1/1

; init sound and graphics constants
InitSndGraph   
	ifdef	Dragon64
	LDD	#$B844			; initial volume Dragon 64
	else
	LDD     #$BA42			; initial volume Dragon 32
	endc
        STD     <SndVolume		; set initial volume
        LDA     #$02			
        STA     <SndTempo		; set initial sound tempo
        STA     <SndOctave		; set inital octave
        ASLA				; multiply by 2
        STA     <SndNoteLen		; set initial note length (4)
        CLR     <SndDotNoteScale	; clear note timer scale factor
	
        LDD     <DBZero	; D=0
        STD     <GrDrawAngle		; zero draw scale and angle
        LDB     #$80			; initialize grapics X pos to 128
        STD     <GrCurrXCo
        LDB     #$60			; initialize grapics Y pos to 96
        STD     <GrCurrYCo
L98FF   RTS

; get parameters for CSAVEM
L9900   JSR     <BasChrGet		; skip over the "M"
        JSR     >GetCasFnameToBuff	; get cassette filename to filename buffer
        
	BSR     GetCommaInt		; get base address for save
        STX     CasLoadAddr 
        
	BSR     GetCommaInt		; get end address for save
        CMPX    2,S			; check that it's after base address
        LBCS    BasFCError 		; nope, generate FC errror
	
        BSR     GetCommaInt		; get entry address for save
        STX     CasEntryAddr 		; save it
        JSR     <BasChrGetCurr		; get next character from basic
        BNE     L98FF			; extrac characters, return... causes crash!
					; because addresses still on stack!
CasWriteBin:
L991B   LDA     #FtMachineCode		; Set file type as binary
        LDX     <DBZero	; X=0, file mode and ASCII flag	
        JSR     >CasWriteHeader		; write header block
	
        CLR     <CasStatus		; close tape files 
        INC     <CasBlockType		; block type = 1 (data) 
        JSR     >CasWriteLeader 	; write leader tone
	
        LDX     4,S			; get start address
L992B   STX     <CasIOBuffAddr		; set write address
        LDA     #$FF			; block length = 256
        STA     <CasBlockLen		; set it
        LDD     2,S			; get end address
        SUBD    <CasIOBuffAddr		; work out length of block to save
        BCC     L993C			; branch if data still to write
        LEAS    6,S			; drop addresses from stack
        JMP     >LB6CD			; write final block

L993C   CMPD    #$00FF			; at least one block left?
        BCC     L9945			; yes
        INCB				; no, put waht's left in blklen
        STB     <CasBlockLen
L9945   JSR     >CasBlockOut 		; save next block
        BRA     L992B			; loop for more

; get a 16 bit integer from basic, first checks for comma
GetCommaInt
	JSR     >VarCKComma 		; syntax check for comma
        JSR     >VarGet16Bit 		; get 16 bit integer
        LDU     ,S			; get return address
        STX     ,S			; save integer on stack
        TFR     U,PC			; return to caller

; Basic FIX function
CmdFIX:
L9956   JSR     >TestFPA0		; test FPA0
        BMI     L995E			; branch if -ve
L995B   JMP     >CmdINT 		; convert FPA0 to integer
			
L995E   COM     <FP0SGN			; toggle sign of FPA0 mantissa
        BSR     L995B			; convert FPA0 to integer
        JMP     >ChangeFPA0Sign		; toggle sign of FPA0

; basic EDIT command
CmdEdit:
L9965   JSR     >L9D9F			; get line number from basic
        LEAS    2,S			; drop return address
L996A   LDA     #$01			; list flag 
        STA     <EvalD8			; set flag to list line
        JSR     >BasFindLineNo 		; go find the line number
        LBCS    BasULError		; Line not found : error
        
	JSR     >L8F08			; go uncrunch the line 
        TFR     Y,D			; put address of end of line in D
        SUBD    #BasLinInpBuff+2	; work out line length	
        STB     <BasEditorLineLen	; save length of line
	
L997F   LDD     BasTempLine		; get value of line number		
        JSR     >TextOutNum16 		; output the line number
        JSR     >TextOutSpace 		; and a space
        LDX     #BasLinInpBuff+1
        LDB     <EvalD8			; check if line is to be listed to screen
        BNE     L99B3			; yes : go do it
	
L998E   CLRB				; reset digit accumulator
L998F   JSR     >L9AB9			; get keystroke
        JSR     >LA438			; set carry if not numeric
        BCS     L99A2			; branch if not numeric
	
        SUBA    #'0'			; convert to binary
        PSHS    A			; save it
        LDA     #$0A			; multiply digit accumulator by 10
        MUL
        ADDB    ,S+			; add new digit
        BRA     L998F			; loop again

; key typed is not a digit, B contains a repeat count for the following command
L99A2   SUBB    #$01			; number of repeats param in B
        ADCB    #$01			; if it is 0 then make it 1
        
	CMPA    #'A'			; 'A' : Abort?
        BNE     L99AF			; nope
        JSR     >TextOutCRLF 		; yes print a return
        BRA     L996A			; output line and start editing again

L99AF   CMPA    #'L'			; 'L' List?
        BNE     L99BE			; no.....
	
L99B3   BSR     L99E6			; List the line
        CLR     <EvalD8			; reset list flag to no list			
        JSR     >TextOutCRLF 		; output a return
        BRA     L997F			; loop again for another edit command	

L99BC   LEAS    2,S			; purge return address
L99BE   CMPA    #$0D			; enter key pressed ?
        BNE     L99CF			; nope
	
        BSR     L99E6			;	
L99C4   JSR     >TextOutCRLF 		; output a return
        LDX     #BasLinInpBuff+1	; point to line in buffer
        STX     <BasAddrSigByte		; reset basic input pointer
        JMP     >L83A6			; go put the line back in program

L99CF   CMPA    #'E'			; 'E' same as enter except no echo	
        BEQ     L99C4
	
        CMPA    #'Q'			; Quit?
        BNE     L99DD			; nope, skip
        JSR     >TextOutCRLF 		; output a return
        JMP     >BasCmdMode 		; go back to basic interpreter, don't re-insert line

L99DD   BSR     L99E1			; interpret remaining commands as subroutines
        BRA     L998E			; loop again for another edit command	

L99E1   CMPA    #' '			; space pressed?
        BNE     L99F5			; nope, skip

        FCB     Skip2			; skip 2 bytes

L99E6   LDB     #$F9			; max bytes in line buffer
L99E8   LDA     ,X			; get a character from buffer
        BEQ     L99F4			; end of line ?, yep exit
        JSR     >TextOutChar 		; output the character
        LEAX    1,X			; increment buffer pointer
        DECB				; decrement character counter (entered above)
        BNE     L99E8			; all done, if not loop again
L99F4   RTS				; return, do another command

L99F5   CMPA    #'D'			; delete command
        BNE     L9A41			; nope, skip
	
L99F9   TST     ,X			; check for end of line
        BEQ     L99F4			; branch if at end
        BSR     L9A03			; remove a character
        DECB				; decrement counter
        BNE     L99F9			; loop again if more
        RTS				; return, do another command

;remove one character from edit buffer
L9A03   DEC     <BasEditorLineLen	; decrement line length
        LEAY    -1,X			; point at previous character, compensate for LEAY 1,Y below	
L9A07   LEAY    1,Y			; move to next character
        LDA     1,Y			; get next character 
        STA     ,Y			; save in current character position, move it back one
        BNE     L9A07			; not end of line, keep going
        RTS				; return, do another command

L9A10   CMPA    #'I'			; insert?
        BEQ     L9A27			; yes : do it
	
        CMPA    #'X'			; extend?
        BEQ     L9A25			; yes : do it
	
        CMPA    #'H'			; hack?
        BNE     L9A78			; yes : do it
	
        CLR     ,X			; put EOL flag at current buffer pointer
        TFR     X,D			; transfer buffer pointer to D
        SUBD    #BasLinInpBuff+2	; subtract initial buff position
	
        STB     <BasEditorLineLen	; set line length
L9A25   BSR     L99E6			; display line on screen
L9A27   JSR     >L9AB9			; get a keystroke

        CMPA    #$0D			; return?
        BEQ     L99BC			; yes, interpret another command print line
	
        CMPA    #$1B			; escape?
        BEQ     L9A57			; yes, return to command level, discard line
 
        CMPA    #$08			; backspace
        BNE     L9A58			; no, skip

; handle backspace	
        CMPX    #BasLinInpBuff+1	; at beginning of buffer
        BEQ     L9A27			; yes: can't delete anymore
	
        BSR     L9A82			; move pointer back, BS on the screen
        BSR     L9A03			; delete character from buffer
        BRA     L9A27			; go back and get next insert subcommand

L9A41   CMPA    #'C'			; change?
        BNE     L9A10			; no
	
L9A45   TST     ,X			; check current buffer character
        BEQ     L9A57			; at end of line, exit
        JSR     >L9AB9			; get a keystroke
        BCS     L9A50			; ; ligitimate key skip on
        BRA     L9A45			; key invalid, try again

L9A50   STA     ,X+			; save character in line (replacing current one)
        BSR     L9A8B			; send new character to screen
        DECB				; decrement character counter 
        BNE     L9A45			; loop again if more
L9A57   RTS				; return, do another command

L9A58   LDB     <BasEditorLineLen	; get length of line	
        CMPB    #$F9			; bigger than max length ?	
        BNE     L9A60			; no, skip on
        BRA     L9A27			; ignore input if at maximul length

; Insert a character in line, by moving everything from current
; character up one byte to make room.
L9A60   PSHS    X			; save current buffer pointer
L9A62   TST     ,X+			; scan line for terminator
        BNE     L9A62			; till we reach the end of the line
	
L9A66   LDB     ,-X			; decrement current line pointer and get a character
        STB     1,X			; put character up one byte
        CMPX    ,S			; have we reached the starting point?
        BNE     L9A66			; nope, keep shuffling
	
        LEAS    2,S			; drop saved pointer
        STA     ,X+			; save typed character at insert position
        BSR     L9A8B			; send character to console
        INC     <BasEditorLineLen	; increment saved line length
        BRA     L9A27			; get another insert sub command

L9A78   CMPA    #$08			; backspace?
        BNE     L9A8E			; nope, skip
L9A7C   BSR     L9A82			; move pointer back, send BS to screen
        DECB				; decrement line length
        BNE     L9A7C			; not at beginning of line, loop again
        RTS				

L9A82   CMPX    #BasLinInpBuff+1	; compare pointer to start of buffer
        BEQ     L9A57			; at beginning, can't backspace further, exit
        LEAX    -1,X			; move pointer back one
        LDA     #$08			; display a backspace on screen
L9A8B   JMP     >TextOutChar

L9A8E   CMPA    #'K'			; Kill?
        BEQ     L9A97			; yes
        SUBA    #'S'			; search?
        BEQ     L9A97			; yes
        RTS

L9A97   PSHS    A			; save search / kill flag on stack			
        BSR     L9AB9			; get a keystroke, target character
        PSHS    A			; save it on stack
	
L9A9D   LDA     ,X			; get current buffer character
        BEQ     L9AB7			; branch if end of line
        TST     1,S			; check search / kill flag
        BNE     L9AAB			; branch if kill
        BSR     L9A8B			; send character to console out	
        LEAX    1,X			; move to next char in buffer
        BRA     L9AAE			; check next input character

L9AAB   JSR     >L9A03			; remove one character from buffer
L9AAE   LDA     ,X			; get current input character
        CMPA    ,S			; same as searched for?
        BNE     L9A9D			; branch if not same, loop again
        DECB				; decement count
        BNE     L9A9D			; loop again if not zero
L9AB7   PULS    Y,PC			; restore and return, pull Y will clean the stack

; get a keystroke, used by edit command
L9AB9   JSR     >TextWaitKeyCurs2 	; wait for keypress with cursor	
        CMPA    #$7F			; graphic character?
        BCC     L9AB9			; yes, get another
	
        CMPA    #$5F			; shift up-arrow? (quit insert)
        BNE     L9AC6			; no : skip
        LDA     #$1B			; yes : replace code with ESC
	
L9AC6   CMPA    #$0D			; enter?
        BEQ     L9AD8			; yes : return
	
        CMPA    #$1B			; ESC?
        BEQ     L9AD8			; yes : return
	
        CMPA    #$08			; backspace?
        BEQ     L9AD8			; yes : return
	
        CMPA    #$20			; space?
        BCS     L9AB9			; nope, get another, character is invalid
	
        ORCC    #FlagCarry		; set carry
L9AD8   RTS

; Basic TRON
CmdTron:
L9AD9   FCB	Skip1LD 		; skip one byte, leaving A nonzero

; Basic TROFF
CmdTroff:
L9ADA   CLRA				; clear trace flag
        STA     <BasTronFlag		; save it
        RTS

CmdPOS:
L9ADE   LDA     <TextDevN		; get device number
        PSHS    A			; save it
        JSR     >LB7E0			; get device number
        JSR     >LB63C			; file status check
        JSR     >SetPRINTParams		; setup tab width etc
        LDB     <TextVDUCurrCol		; get current column
        PULS    A			; retore device number
        STA     <TextDevN
        JMP     >VarAssign8Bit 		; return pos to basic from B

CmdVarptr:
L9AF4   JSR     >VarCKOpBrac 		; syntax check for open bracket
        LDD     <BasVarEnd		; point to end of vars	
        PSHS    D			; save it
        JSR     >VarGetVar 		; get descriptor pointer in X
        JSR     >VarCKClBrac 		; syntax check for cloe bracket
        PULS    D			; recover pointer to end of vars
        EXG     X,D			; swap descriptor pointer and end pointer
        CMPX    <BasVarEnd		; compare to new end of vars
        BNE     L9B5A			; FC error, if var was not defined before call
					; i.e. end of variables has changed, creating a 
					; new variable in the VarGetVar call above will do this.
	
	ifdef	Dragon64
	JMP     >L9FD0			; Dragon 64, convert to 16 bit unsigned as addr may be > 32K
	else
        JMP     >VarAssign16Bit2 	; Dragon 32, convert to 16 bit signed
	endc
	
; MID$(oldstring,position,length)=replacement	
DoMIDS  JSR     <BasChrGet		; get a character from basic
        JSR     >VarCKOpBrac 		; syntax check for open bracket
        JSR     >VarGetVar 		; get the variable to slice
	
        PSHS    X			; save it's descriptor pointer on stack
        LDD     2,X			; get address of the string part of it
        CMPD    <AddrStack		; compare to start of CLEARed space	
        BLS     L9B21			; branch if <=
	
        SUBD    <AddrFWareRamTop	; suptract out top of CLEARed space	
        BLS     L9B33			; branch if string in string space
	
L9B21   LDB     ,X			; get length of old string
        JSR     >BasResStr2 		; reserve length bytes in string space for new string
	
        PSHS    X			; save resrved space pointer on stack
        LDX     2,S			; recover descriptor pointer
        JSR     >L8D89			; move oldstring into string space
	
        PULS    X,U			; get old string descriptor and new string space pointer
        STX     2,U			; save reserved address as oldstring address
        PSHS    U			; save oldstring descriptor address
	
L9B33   JSR     >VarGetComma8 		; syntax check for comma and get start pos in string
        PSHS    B			; save start pos
        TSTB				; test position parameter
        BEQ     L9B5A			; branch if start of string
	
        LDB     #$FF			; default replacement length = $ff
        CMPA    #')'			; check for end if MID$, i.e. only start position specified
        BEQ     L9B44			; yes, branch
	
        JSR     >VarGetComma8 		; syntax check comma, and get length of string to copy
L9B44   PSHS    B			; save length on stack

        JSR     >VarCKClBrac 		; syntax check for closing bracket
        LDB     #DTokEQUAL
        JSR     >VarCKChar		; syntax check for '=' token 
        BSR     L9B7E			; evaluate replacement string
	
        TFR     X,U			; save replacement string address in U
        LDX     2,S			; point X to oldstring descriptor address
        LDA     ,X			; get length of oldstring
        SUBA    1,S			; subtract position parameter
        BCC     L9B5D			; position < length, continue
L9B5A   JMP     >BasFCError 		; position > length, generate FC error

L9B5D   INCA				; now A is number of characters to the right 
					; of the position parameter
        CMPA    ,S			; compare to length parameter
        BCC     L9B64			; branch if newstring will fit in oldstring
	
        STA     ,S			; if not use as much of length as will fit
L9B64   LDA     1,S			; get position parameter
        EXG     A,B			; A=length of replacement string, B=length
        LDX     2,X			; point at oldstring address
	
        DECB				; make position parameter zero based
        ABX				; add position parameter to start of string
        TSTA				; test length of replacement string
        BEQ     L9B7C			; if it is zero, return
        
	CMPA    ,S			; if the length of the replacement string is <
	BLS     L9B75			; adjusted replacement length, the branch
        
        LDA     ,S			; otherwise use as much room as is available
L9B75   TFR     A,B			; save no of bytes to move in B
        EXG     U,X			; swap source and destination pointers
        JSR     >UtilCopyBXtoU 		; copy the bytes
L9B7C   PULS    D,X,PC			; restore and return

L9B7E   JSR     >VarGetStr 		; evaluate expression
        JMP     >BasGetStrLenAddr 	; TM error if not string, return with :
					; X=pointer to string, B=length

; Basic STRING$ function
CmdStringS:
L9B84   JSR     >VarCKOpBrac 		; syntax check for open bracket
        JSR     >VarGet8Bit 		; get number of characters, error if >255 
        PSHS    B			; save it
	
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >VarGetStr 		; get string / evaluate expression
        JSR     >VarCKClBrac 		; syntax check for close bracket
        
	LDA     <BasVarType		; get variable type of last expression
        BNE     L9B9E			; branch if string
	
        JSR     >FPA0toB		; convert FPA0 to integer in B, error if >255
        BRA     L9BA1

L9B9E   JSR     >BasGetStrFirst 	; get first byte of string in B
L9BA1   PSHS    B			; save repeat character
        LDB     1,S			; get repeat count in B
        JSR     >BasResStr 		; reserve a string of B bytes
	
        PULS    D			; recover repeat count and character
        BEQ     L9BB1			; branch if null string
	
L9BAC   STA     ,X+			; store character in string
        DECB				; decrement count
        BNE     L9BAC			; keep going until count = 0
L9BB1   JMP     >L8DE1			; return string to basic

; Basic INSTR function
CmdInstr:
L9BB4   JSR     >VarCKOpBrac		; syntax check for open bracket
        JSR     >VarGetStr 		; evaluate expression 
        LDB     #$01			; default to starting at beginning of string
        PSHS    B			; save start pos on stack
	
        LDA     <BasVarType		; get variable type
        BNE     L9BD2			; branch if string
        JSR     >FPA0toB		; otherwise get start pos
        STB     ,S			; and save it on stack
	
        BEQ     L9B5A			; FC error if start pos = 0
        
	JSR     >VarCKComma 		; syntax check for comma
        JSR     >VarGetStr 		; evaluate expression, get search string
        JSR     >VarGetExpr 		; TM error if numeric
	
L9BD2   LDX     <FPA0+2			; get descriptor address for search string
        PSHS    X			; save on stack
	
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >L9B7E			; go get target string & length
        PSHS    B,X			; save length and address of target string on stack
	
        JSR     >VarCKClBrac 		; syntax check for close bracket
        LDX     3,S			; get pointer to search string descriptor into X
        JSR     >VarDelVar 		; get length and address of search string
        PSHS    B			; save length on stack
 
; AT THIS POINT THE STACK HAS THE FOLLOWING INFORMATION ON IT:
; 0	S-SEARCH LENGTH 
; 1	S-TARGET LENGTH
; 2 3	S-TARGET ADDRESS 
; 4 5	S-SEARCH DESCRIPTOR ADDRESS
; 6	S-SEARCH POSITION 
;      
	CMPB    6,S			; compare length of search string, with start pos of search
        BCS     L9C0F			; return if start pos > length
	
        LDA     1,S			; get length of target string
        BEQ     L9C0C			; target string is null, return
	
        LDB     6,S			; get start position
        DECB				; make zero based
        ABX				; add to start of string
	
L9BF4   LEAY    ,X			; point Y to search position
        LDU     2,S			; point U at target string
        LDB     1,S			; get target length
        LDA     ,S			; get search length
        
	SUBA    6,S			; subtract search position from search length
        INCA				; add one
        CMPA    1,S			; compare to target length
        BCS     L9C0F			; return 0 if target length > what's left of string
	
L9C03   LDA     ,X+			; get a character from search string
        CMPA    ,U+			; compare to target string
        BNE     L9C15			; not same, skip
	
        DECB				; decrement search length
        BNE     L9C03			; loop again if characters match
L9C0C   LDB     6,S			; get search position
        
	FCB	Skip1			; skip a byte
L9C0F   CLRB				; clear b
        LEAS    7,S			; drop stack vars
        JMP     >VarAssign8Bit 		; return B to basic

L9C15   INC     6,S			; move to next position in search string
        LEAX    1,Y			; move x to next search position
        BRA     L9BF4			; loop again

; deal with &H and &O
L9C1B   CLR     <FPA0+2			; clear out FPA0			
        CLR     <FPA0+3
        LDX     #FPA0+2 		; FPA0+2,3 temporary accumulator
        JSR     <BasChrGet		; get next character from basic
	
        CMPA    #'O'			; Octal number?
        BEQ     L9C3A			; yes : processs
        CMPA    #'H'			; Hex number?
        BEQ     L9C4F			; yes process
        JSR     <BasChrGetCurr		; get current character from basic
        BRA     L9C3C

; no specifier after &, assume octal?
; note, BUG, cmpa #'8' should be cmpa #'7' this leads to 8 being accepted as a digit
; in an octal number which is not correct, this is interpreted as &O10 << column number
; e.g. :
; number	interpreted as
; &O8		&O10
; &O80		&O100
; &O800		&O1000
L9C30   CMPA    #'8'			; Octal number
        LBHI    BasSNError 		; higher or equal
        LDB     #$03			; base 8 multiplier
        BSR     L9C64			; add digit to temp accumulator
	
L9C3A   JSR     <BasChrGet		; get next character
L9C3C   BCS     L9C30			; branch if numeric, loop again

VarAssign16BitB:
L9C3E   CLR     <FPA0			; clear FPA0, bytes 0,1
        CLR     <FPA0+1
        CLR     <BasVarType		; set var type to numeric
        CLR     <FPSByte		; clear sub byte	
        CLR     <FP0SGN			; clear mantissa sign byte
        LDB     #$A0			; set exponent of FPA0
        STB     <FP0EXP		
        JMP     >VarNormFPA0 		; normalize

L9C4F   JSR     <BasChrGet		; get next char
        BCS     L9C5E			; branch if numeric
        JSR     >CheckAAlpha		; check alphametic
        BCS     VarAssign16BitB 	; branch if not alpha or numeric	
	
        CMPA    #'G'			; first illgeal hex digit
        BCC     VarAssign16BitB 	; above, not valid hex
	
        SUBA    #'A'-('9'+1)		; subtract ascii diffrence between 'A' and '9'+1
L9C5E   LDB     #$04			; multiplcatiion factor, for hex
        BSR     L9C64			; add digit to temp accumulator
        BRA     L9C4F			; loop for next

L9C64   ASL     1,X			; multiply temp accumulator by 2
        ROL     ,X
        LBCS    BasOVError		; overflow error!
        DECB				; decrement multiplcation counter
        BNE     L9C64			; still multiplications to do
D9C6F   SUBA    #'0'			; convert hex digit to binary
        ADDA    1,X			; add digit to temp accumulator
        STA     1,X			; and save it
L9C75   RTS

; Check if in direct mode, Illegal Direct error if so.
BasChkDirect:
L9C76   LDX     <BasCurrentLine		; get current input line
        LEAX    1,X			; add one
        BNE     L9C75			; not direct mode, return
BasIDError
        LDB     #ErrID			; illegal direct mode
L9C7E   JMP     >SysErr 		; generate error

CmdDef:
L9C81   LDX     [BasAddrSigByte] 	; get two input chars
        CMPX    #$FF00+DTokUSR		; check for token for USR
        LBEQ    L9D00			; yes: go handle it
	
        BSR     L9CB1			; get descriptor address for FN name
        BSR     BasChkDirect 		; don't allow DEF FN in direct mode
        
	JSR     >VarCKOpBrac 		; syntax check for open brackets
        LDB     #$80			; flag to indicate array search disable
        STB     <BasDisArraySearch	; set flag
	
        JSR     >VarGetVar 		; get variable descriptor
        BSR     L9CC1			; generate TM error if string
	
        JSR     >VarCKClBrac 		; syntax check closing bracket
        LDB     #DTokEQUAL		; token for '='
        JSR     >VarCKChar 		; syntax check for it
	
        LDX     <Eval4B			; get the address of the FN name descriptor
        LDD     <BasAddrSigByte		; get the input buffer address
        STD     ,X			; save in first 2 bytes of descriptor
        LDD     <BasVarPtrLast		; get the descriptor address of the argument variable
        STD     2,X			; save it in FN deescriptor
        JMP     >CmdData 		; move pointer to end of line or subline


L9CB1   LDB     #DTokFN			; token for FN
        JSR     >VarCKChar 		; syntax check for it
        LDB     #$80			; flag to indicate array search disable
        STB     <BasDisArraySearch	; set flag
	
	ORA     #$80			; set bit 7 of current input character to indicate FN
        JSR     >L8A99			; get discriptor address of current variable
        STX     <Eval4B			; and save it in Eval4B
L9CC1   JMP     >VarGetExprCC 		; TM error if string variable

;evaluate an FN call
L9CC4   BSR     L9CB1			; get descriptor of FN variable
        PSHS    X			; save it on stack
	
        JSR     >L899F			; syntax check for '(' and evaluate expression
        BSR     L9CC1			; TM error if string
	
        PULS    U			; recover descriptor address in U
        LDB     #ErrUF			; undefined function call error
        LDX     2,U			; point X to argument variable descriptor
        BEQ     L9C7E			; if zero, jump to error handler
	
        LDY     <BasAddrSigByte		; get current input pointer in Y
        LDU     ,U			; point U at the start of the FN formula 
        STU     <BasAddrSigByte		; and save it in the input pointer
	
        LDA     4,X			; get the FP value of the input variable
        PSHS    A			; 
        LDD     ,X			; get pointer address
        LDU     2,X			
        PSHS    D,X,Y,U			; save on stack
	
        JSR     >PackFPA0toX		; pack FP0 and save in fp at X
        JSR     >L8872			; evaluate FN expression
	
        PULS    D,X,Y,U			; restore pointers etc
        STD     ,X			; get FP value of argument off the stack
        STU     2,X			; and re- save it
        PULS    A
        STA     4,X
	
        JSR     <BasChrGetCurr		; get current character
        LBNE    BasSNError 		; Syntax error if not end of line
        STY     <BasAddrSigByte		; retore previous input pointer
        RTS

; DEF USR handler
; The Dragon 64 fixes a bug with USR that would have been better left broken, 
; as it made some code written for the Dragon 32 fail on the 64.
L9D00   JSR     <BasChrGet		; skip past USR token
	
	ifdef	Dragon64
	JSR     <BasChrGet		; If Dragon 64, get an additional character 
	endc
	
        BSR     GetUSRTablePtrX		; get FN number usr table offset in X 
        PSHS    X			; save table offset
        BSR     L9D35			; get exec address in X
	
	ifdef	Dragon64
	STX     [,S++]			; Save address in table
        else
	PULS    U			; get table address
        STX     ,U			; save address in table
	endc
	
        RTS

GetUSRTablePtrX   
	CLRB				; default to USR0 if no argument
	
	ifdef	Dragon64
	JSR	<BasChrGetCurr		; get current character
	else
        JSR     <BasChrGet		; get current character
	endc
	
        BCC     L9D18			; branch if not numeric
        SUBA    #'0'			; convert to binary
        TFR     A,B			; save in B
        JSR     <BasChrGet		; get next character
	
L9D18   LDX     <BasUSRTableAddr	; point to user table	
        ASLB				; multiply function no by 2, as 2 bytes / address
        ABX				; add function no offset to table base
        RTS

; USR function call
;
; USR routine can be called with either a string or a numeric argument.
; On entry to the routine A will contain the variable type, numeric=0, string=$FF
; If type is numeric then FPA0 will contain the value, 
; If the type is string then X will point to it's descriptor.
CmdUSR:
L9D1D   BSR     GetUSRTablePtrX		; get user table pointer in X
        LDX     ,X			; get usr execution address
        PSHS    X			; pusu USR address on stack for RTS below
        JSR     >L899F			; syntax check for '(' and evaluate expression
        
	LDX     #FP0EXP			; point at FPA0 			 		
        LDA     <BasVarType		; check variable type
        BEQ     L9D34			; numeric, jump to usr routine 
        
	JSR     >L8D9D			; get length and address of string variable
        LDX     <FPA0+2			; get pointer to string descriptor
        LDA     <BasVarType		; get basic variable type
	
L9D34   RTS				; jump to usr routine, address pushed onto stack above

L9D35   LDB     #DTokEQUAL		; check for equals token
        JSR     >VarCKChar 		; syntax error if not
        JMP     >VarGet16Bit 		; get 16 bit address in X

BasIRQVec:
L9D3D   LDA     PIA0CRB			; read control register of PIA0B
        BMI     FramSyncINT		; branch if frame sync interrupt
        RTI				; otherwise RTI

; Frame sync interrupt handler.
; Called at 50Hz in PAL/SECAM machines.
; Called at 60Hz in NTSC machines.	
FramSyncINT   
	LDA     PIA0DB			; read data register, clear interrupt flag in PIA
        LDX     SysTimeVal 		; Get timer value
        LEAX    1,X			; add one to it	
        STX     SysTimeVal 		; reset time value
        JMP     >SoundINT		; call sound interrupt routines

; Basic TIMER command, when setting timer
L9D51   JSR     <BasChrGet		; get next char
        BSR     L9D35			; check for = and get new timer value
        STX     SysTimeVal 		; set new timer value
        RTS

; Basic TIMER command, when getting timer
CmdTimer:
L9D59   LDX     SysTimeVal 		; get timer value
        STX     <FPA0+2			; store it in FPA0
        JMP     >VarAssign16BitB 	; return it to basic

; Basic DEL command, delete lines from program
CmdDelete:
L9D61   LBEQ    BasFCError 		; if no parameters FC error
        JSR     >BasGetLineNo 		; get start line number
        JSR     >BasFindLineNo 		; find it in program
	
        STX     <BasCloadMOffs		; save start line number
        JSR     <BasChrGetCurr		; get input character
        BEQ     L9D81			; none skip ahead
	
        CMPA    #DTokMINUS		; check for '-' token
        BNE     L9DB0			; nope skip ahead
	
        JSR     <BasChrGet		; get next character
        BEQ     L9D7D			; end of line, delete rest of program 
        BSR     L9D9F			; get end line number
        BRA     L9D81			; skip on

L9D7D   LDA     #$FF			; set line number to $FFxx
        STA     BasTempLine		
L9D81   LDU     <BasCloadMOffs		; get start line no address

	FCB	Skip2
L9D84   LDU	,U			; point U to start of next line
        LDD     ,U			; check for end of program
        BEQ     L9D90			; branch if end of program
	
        LDD     2,U			; get this line's line no
        SUBD    BasTempLine		; subtract end line no
        BLS     L9D84			; it's lower, keep searching
	
L9D90   LDX     <BasCloadMOffs		; get starting line number
        BSR     L9DA9			; move bytes from U to X till end of program
        
	JSR     >BasVect1 		; reset basic input pointer & variable addresses
        LDX     <BasCloadMOffs		; get start line pointer
        JSR     >L83EF			; recompute start of next line address
        JMP     >BasCmdMode 		; return to main command loop

L9D9F   JSR     >BasGetLineNo 		; get line no and convert to binary
        JMP     >LB7F9			; check for further characters, error if found

L9DA5   LDA     ,U+			; get a byte pointed to by U
        STA     ,X+			; move it to X
L9DA9   CMPU    <BasVarSimpleAddr	; end of basic program / start of variables?
        BNE     L9DA5			; nope, keep going
        STX     <BasVarSimpleAddr	; update end of program.
L9DB0   RTS

CmdLineInput:
L9DB1   JSR     >BasChkDirect 		; Check if direct mode 'ID' error if so
        JSR     <BasChrGet		; get a character from basic
        CMPA    #'#'			; check for device number
        BNE     L9DC3			; none, skip
	
        JSR     >LB7D7			; check for valid device no
        JSR     >LB623			; check for open file
        JSR     >VarCKComma 		; syntax check for comma (after dev no)
	
L9DC3   CMPA    #'"'			; check for the start of prompt string
        BNE     L9DD2			; nope skip
	
        JSR     >L8975			; strip off " and put prompt on string stack
        LDB     #';'
        JSR     >VarCKChar 		; syntax check for ';'
        JSR     >L90E8			; get prompt string from string stack and send to console
	
L9DD2   LEAS    -2,S			; reserve two bytes on stack
        JSR     >L8766			; input a line from current input device
        LEAS    2,S			; clean up stack
	
BasLineInputEntry:
        CLR     <TextDevN		; set device number to screen
        JSR     >VarGetVar 		; search for a variable
        STX     <BasTempVarDesc		; save pointer to variable descriptor
	
        JSR     >VarGetExpr 		; 'TM' error if variable numeric
        LDX     #BasLinInpBuff 		; point X at input buffer
        
	CLRA				; terminator character at EOL
        JSR     >L8C5D			; parse input string and save to string space
        JMP     >L86D7			; remove descriptor from string stack

L9DED   JSR     >BasGetLineNo 		; get a line number from basic line		
        LDX     BasTempLine		; get binary value
        RTS

L9DF3   LDX     <BasRenumStart		; get the old number being renumbered
L9DF5   STX     BasTempLine		; save the line being searched for
        JMP     >BasFindLineNo 		; go find it	

; Basic RENUM command
CmdRenum:
L9DFA   JSR     >BasEraseVars		; erase variables
        LDD     #$000A			; default line number interval / start line
        STD     <BasRenumStartLine	; save start line
        STD     <BasRenumVal		; save interval
        
	CLRB				; now D = 0
        STD     <BasRenumStart		; default line number of where to start renumbering
	
        JSR     <BasChrGetCurr		; get current input character
        BCC     L9E11			; branch if not numeric
	
        BSR     L9DED			; convert supplied line number to binary
        STX     <BasRenumStartLine	; save the line to start renumbering from
        JSR     <BasChrGetCurr		; get current input character
	
L9E11   BEQ     L9E2E			; none, skip 
        JSR     >VarCKComma 		; syntax check for comma
        BCC     L9E1E			; branch if next char is not numeric
	
        BSR     L9DED			; convert supplied line number to binary 
        STX     <BasRenumStart		; set new start line
        JSR     <BasChrGetCurr		; get current character

L9E1E   BEQ     L9E2E			; end of line, skip 
        
	JSR     >VarCKComma 		; syntax check for comma
        BCC     L9E2B			; branch if next character not numeric
        BSR     L9DED			; convert supplied line number to binary 
        STX     <BasRenumVal		; set line interval value
        BEQ     L9E74			; if interval = 0, the FC error
	
L9E2B   JSR     >LB7F9			; check for more characters on line, SN error if so

L9E2E   BSR     L9DF3			; get address of old number being renumbered
        STX     <BasCloadMOffs		; save it
        LDX     <BasRenumStartLine	; get next renumbered line number to use
        BSR     L9DF5			; find the number in the basic program
	
        CMPX    <BasCloadMOffs		; compare to address of old line number
        BCS     L9E74			; FC error if old < new
	
        BSR     L9E58			; make sure renumbered lines will be in range
        JSR     >L9ECE			; convert ASCII line numbers to expanded binary
        JSR     >BasVect2 		; recalculate next line ram address
        BSR     L9DF3			; get RAM address of first line to be renumbered
        STX     <BasCloadMOffs		; save it
	
        BSR     L9E82			; make sure new line numbers exist
        BSR     L9E59			; insert new line number in line headers
        BSR     L9E82			; insert new line numbers in program statements (goto etc)
        JSR     >L9F6C			; convert packed binary line numbers to ASCII
        JSR     >BasEraseVars		; erase variables
        JSR     >BasVect2 		; recalculate next line ram address
        JMP     >BasCmdMode 		; return to command mode

L9E58   FCB     Skip1LD			; skip 1 byte

L9E59   CLRA				; new line number flag, 0= insert new line numbers
        STA     <EvalD8			; save flag
	
        LDX     <BasCloadMOffs		; get address of old line number, being renumbered
        LDD     <BasRenumStartLine	; get the current renumbered line number
        BSR     L9E77			; return if end of program
	
L9E62   TST     <EvalD8			; test insert line flag
        BNE     L9E68			; branch if not inserting new line numbers
	
        STD     2,X			; store the new line number in the basic program
L9E68   LDX     ,X			; point X at next basic line number
        BSR     L9E77			; return if end of program
        ADDD    <BasRenumVal		; add interval to current renumbered line no
        BCS     L9E74			; 'FC' error if line no > $FFFF
	
        CMPA    #$FA			; biger than max line number : $F9FF?
        BCS     L9E62			; no loop for next line
L9E74   JMP     >BasFCError 		; yes : 'FC' error

; test the two bytes pointed to by X.
; normal return if <> 0. if = 0 (end ofprogram) return is pulled off stack and
; you return to previous subroutine call.
L9E77   PSHS    D			; save D
        LDD     ,X			; test the two bytes pointed to by X
        PULS    D			; restore D, flags unchanged
        BNE     L9E81			; normal return to our caller
        LEAS    2,S			; pull our return addresss from stack and return to 
					; our caller's caller
L9E81   RTS

L9E82   LDX     <BasStartProg		; get start of basic program
        LEAX    -1,X			; backup one byte (to compensate for below)

L9E86   LEAX    1,X			; move pointer up one
        BSR     L9E77			; return if end of program
	
L9E8A   LEAX    3,X			; skip over next line address and line number
L9E8C   LEAX    1,X			; move to next character
        LDA     ,X			; get a byte from the program
        BEQ     L9E86			; end of line : skip
	
        STX     <BasTempPtr		; save current pointer
        DECA				; branch if start of packed numeric line no
        BEQ     L9EA3			
        DECA
        BEQ     L9EC4			; branch if line number exists
        DECA
        BNE     L9E8C			; move to next character if > 3
	
L9E9D   LDA     #$03			; set first byte to 3 to indicate line no doesn't currently exist
        STA     ,X+
        BRA     L9E8A			; get another character

L9EA3   LDD     1,X			; get MS part of line number
        DEC     2,X			; decrement zero check byte
        BEQ     L9EAA			; branch if MS part <> 0
	
        CLRA				; clear MS byte
L9EAA   LDB     3,X			; get LSB of line number
        DEC     4,X			; decrement zero check flag
        BEQ     L9EB1			; branch if LSB <> 0
	
        CLRB				; clear LSB
L9EB1   STD     1,X			; save back in line 
        STD     BasTempLine		; save trial line number
        JSR     >BasFindLineNo 		; find RAM address of basic line
	
L9EB8   LDX     <BasTempPtr		; get back pointer to start of line
        BCS     L9E9D			; branch if no line number match found
	
        LDD     <Eval47			; get start address of line number
        INC     ,X+			; set first byte = 2 to indicate line number exists if checkin 
					; existance of line numbers. Set it to 1 if inserting lines
        STD     ,X			
        BRA     L9E8A			; save the RAM address of the correct line number

L9EC4   CLR     ,X			; clear carry flag and first byte
        LDX     1,X			; point X to the RAM address of the correct line number
        LDX     2,X			; put correct line number into X
        STX     <Eval47			; save save in temp pointer
        BRA     L9EB8			; go insert it in the basic line

L9ECE   LDX     <BasStartProg		; point to start of basic program
        BRA     L9ED6				

L9ED2   LDX     <BasAddrSigByte		; get current input pointer
        LEAX    1,X			; increment by 1

L9ED6   BSR     L9E77			; return if end of program
        LEAX    2,X			; skip past line address
L9EDA   LEAX    1,X			; advance pointer by 1
L9EDC   STX     <BasAddrSigByte		; update input pointer

L9EDE   JSR     <BasChrGet		; get a character from basic
L9EE0   TSTA				; check it
        BEQ     L9ED2			; branch if end of line
        BPL     L9EDE			; branch if not a token
	
        LDX     <BasAddrSigByte		; get current input pointer
        CMPA    #$FF			; is this a secondary token?
        BEQ     L9EDA			; yes, loop back to get next 
        JSR     VectAccessScreen 	; user RAM hook
	
        CMPA    #DTokTHEN		; THEN token?		
        BEQ     L9F04			; yes
	
        CMPA    #DTokELSE		; ELSE token?
        BEQ     L9F04			; yes 
	
        CMPA    #DTokGO			; GO token (as in GOTO, GOSUB)
        BNE     L9EDE			; no skip
        JSR     <BasChrGet		; get next character from basic
	
        CMPA    #DTokTO			; TO token?
        BEQ     L9F04			; yes
	
        CMPA    #DTokSUB		; SUB token?
        BNE     L9EDC			; no 

; Come here for THEN, ELSE, GOTO, GOSUB	
L9F04   JSR     <BasChrGet		; get next character from basic 
        BCS     L9F0C			; branch if numeric
	
L9F08   JSR     <BasChrGetCurr		; get current character
        BRA     L9EE0			; loop back to keep checking line	

L9F0C   LDX     <BasAddrSigByte		; Get current basic input pointer
        PSHS    X			; save on stack
        JSR     >BasGetLineNo 		; convert decimal number to binary
        LDX     <BasAddrSigByte		; get current input pointer

L9F15   LDA     ,-X			; get previous input character
        JSR     >LA438			; clear carry of numeric input value
        BCS     L9F15			; branch if non numeric
	
        LEAX    1,X			; move pointer up one
        TFR     X,D			; D now point beyond end of line no
        SUBB    1,S			; subtract pre numeric pointer LSB
        SUBB    #$05			; make sure there are at least 5 characters in the numeric line
        BEQ     L9F46			; branch if exactly 5
        BCS     L9F32			; branch if less than 5
	
        LEAU    ,X			; transfer X to U
        NEGB				; negate B
        LEAX    B,X			; move X back B bytes
        JSR     >L9DA9			; move bytes from X-> to U-> until 
					; U = end of basic, X = new end of basic
        BRA     L9F46

; force 5 bytes of space for the line number
L9F32   STX     <Eval47			; save end of numeric value
        LDX     <BasVarSimpleAddr	; get end of basic prog / start of vars
        STX     <Eval43			; save it
	
        NEGB				; negate B
        LEAX    B,X			; move pointer back B bytes
        STX     <Eval41			; save pointer
        STX     <BasVarSimpleAddr	; update new end of basic
        JSR     >BasChkArrSpaceMv 	; get end of array space in D
	
        LDX     <Eval45			; get and save the new input pointer
        STX     <BasAddrSigByte
	
L9F46   PULS    X			; restore pointer to start of numeric value
        LDA     #$01			; save the new line flag
        STA     ,X
        STA     2,X
        STA     4,X
        LDB     BasTempLine		; get MSB of binary line number
        BNE     L9F58			; branch if nonzero
	
        LDB     #$01			; save a 1 byte if byte is a 0
        INC     2,X			; if 2,x = 2 then previous was zero
L9F58   STB     1,X			; save MSB of binary line number

        LDB     <BasTempLine+1		; get LSB of binary line number
        BNE     L9F62			; branch not zero

        LDB     #$01			; save a 1 byte if it is a zero
        INC     4,X			
L9F62   STB     3,X
        JSR     <BasChrGetCurr		; get current input character
        CMPA    #','			; is it a comma?
        BEQ     L9F04			; yes process another numeric value
        BRA     L9F08			; no go procees another input character

L9F6C   LDX     <BasStartProg		; point to start of basic program
        LEAX    -1,X			; move pointer back one (compensate)
	
L9F70   LEAX    1,X			; move pointer up one
        LDD     2,X			; get address of next line
        STD     <BasCurrentLine		; save in current line
        JSR     >L9E77			; return if end of program
	
        LEAX    3,X			; skip address of current line and first byte of line number
L9F7B   LEAX    1,X			; move pointer up one
L9F7D   LDA     ,X			; get current character
        BEQ     L9F70			; branch if EOL
	
        DECA				; input character = 1? valid line number
        BEQ     L9F9F			; yep 
	
        SUBA    #$02			; input character = 3? UL line number?
        BNE     L9F7B			; no
	
        PSHS    X			; save pointer
        LDX     #ULMess-1		; point to UL message
        JSR     >TextOutString 		; display it on console
        LDX     ,S			; get input pointer
        LDD     1,X			; get undefined line's number
        JSR     >TextOutNum16 		; output the number
        JSR     >L9573			; print ' IN XXXX' XXXX=current line number
        JSR     >TextOutCRLF 		; output a return
	
        PULS    X			; get input pointer back
L9F9F   PSHS    X			; save current pos of input pointer
        LDD     1,X			; get binary value of line number
        STD     <FPA0+2			; save it in FPA0, bottom 2 bytes
        JSR     >VarAssign16BitB 	; adjust rest of FPA0 as integer
        JSR     >L9587			; convert FPA0 to ASCII, save it in line
	
        PULS    U			; get previous input pointer
        LDB     #$05			; each expanded line no uses 5 bytes
L9FAF   LEAX    1,X			; move pointer forward one
        LDA     ,X			; get ASCII byte
        BEQ     L9FBA			; branch at end of number
	
        DECB				; decrement count
        STA     ,U+			; save in program line
        BRA     L9FAF			; loop again

L9FBA   LEAX    ,U			; transfer new line pointer to X
        TSTB				; does the new line no require 5 bytes?
        BEQ     L9F7D			; yes : go get another input character
        LEAY    ,U			; save new line pointer in Y
        LEAU    B,U			; point U to end of 5 byte packed line no block
        JSR     >L9DA9			; move bytes from U to X till end of program
        LEAX    ,Y			; load X with new line pointer
        BRA     L9F7D			; go get another input character

; Undefined line number message
ULMess
        FCC     /UL /
        FCB     $00

	ifdef	Dragon64
;
; Fixup for MEM command to allow for mem > 32K
; assigns an unsigned 16bit no	
; on entry D contains the address of the top of the stack.
;
L9FCE   SUBD    <BasVarEnd		; subtract end of variables area
L9FD0   CLR     <BasVarType		; flag as integer
        STD     <BasVarAssign16		; save it in FPA0
        LDB     #$90				
        JMP     >VarAssign16BitB 	; return an unsigned int
	else	
	endc

	ifdef	Dragon64ram
	FILL	$00,$E000-*		; Fill spare bytes with zeros
	else
	FILL	$00,$A000-*		; Fill spare bytes with zeros
	endc

; Indirect jump table, for compatibility with CoCo Colour basic.	
IndKeyInput:
        FDB     BasicKbdIn		; Keyboard input
IndCharOutput:
        FDB     TextOutChar		; Charcter output
IndCasOnRead:
        FDB     BasicCassOnRd		; Tape motor on, prepare to read
IndCasBlockIn:	
        FDB     CasBlockIn		; read a block from tape
IndCasBlockOut:
        FDB     CasBlockOut		; write a block to tape
IndJoystickIn:
        FDB     BasicJoyIn		; joystick input
IndCasWriteLead:
        FDB     CasWriteLeader		; write tape leader

; basic HEX$ function
CmdHexS:
LA00E   JSR     >L8E86			; convert FPA0 to a +ve 2 byte integer
        LDX     #BasBuffer+2		; point to temorary buffer
        LDB     #$04			; 4 nibbles
	
LA016   PSHS    B			; save nibble counter
        CLRB				; clear B
        LDA     #$04			; 4 bits
LA01B   ASL     <FPA0+3			; shift word down by on bit
        ROL     <FPA0+2
        ROLB				; get bit into B
        DECA				; decrement bit counter
        BNE     LA01B			; do next bit if nonzero

; The next few lines (up till LA030) supress leading zeroes. 	
        TSTB				; test nibble
        BNE     LA030			; not 0, skip on 
	
        LDA     ,S			; get nibble counter		
        DECA				; decrement
        BEQ     LA030			; if last nibble put it in anyway
        CMPX    #BasBuffer+2		; check to see if we have already output digits
        BEQ     LA03C			; if so put it in string
	
LA030   ADDB    #'0'			; convert nibble to ASCII
        CMPB    #'9'			; greater than 9?
        BLS     LA038			; no put it in string
	
        ADDB    #$07			; adjust to 'A'..'F'
LA038   STB     ,X+			; put byte in string
        CLR     ,X			; clear terminator

LA03C   PULS    B			; recover nibble counter
        DECB				; decrement it
        BNE     LA016			; do next nibble if nonzero
        LEAS    2,S			; drop return address
        LDX     #BasBuffer+1		; point to hex string
        JMP     >L8C5B			; return it to basic

; Basic DLOAD function, on the Dragon 32 this just generates IO error, as it has no
; serial hardware.
CmdDload:
LA049   JSR     >CasClosFiles 		; close tape files 
        CLR     ,-S			; make a temp byte on stack (DLOADM flag)	
        JSR     <BasChrGetCurr		; get current character from basic
	
        CMPA    #'M'			; is it DLOADM ?
        BNE     LA058			; no skip ahead
	
        STA     ,S			; save DLOADM flag
        JSR     <BasChrGet		; get next character from basic
	
LA058   JSR     >GetCasFnameToBuff	; get filename and put it in buffer
        JSR     <BasChrGetCurr		; get current character from basic
        BEQ     LA070			; branch if end of input
 
        JSR     >VarCKComma		; syntax check for comma
        CMPA    #','			; check for consecutave commas
        BEQ     LA070			; branch if found
	
        JSR     >VarGet8Bit 		; get baud rate from basic
        JSR     >BasicSetBaud 		; set baud rate
        LBCS    BasFCError 		; generate FC error if baud rate invalid
	
LA070   JSR     >LA0F4			; transmit filename and read in file status
        
	PSHS    A			; save A
        LDA     #-3			; set device number to -3			
        STA     <TextDevN
        PULS    A			; recover A
	
        TST     ,S+			; is it DLOADM?
        BNE     LA0A9			; if DLOAD branch, into CLOAD code
        JSR     >LB7F9			; check command line, if further chars error
        TSTB
        BEQ     LA08B			; Generate FM error
        JSR     >BasNew 		; clear program
        JMP     >BasCmdMode2		; return to command mode

LA08B   JMP     >BasFMError 

; basic CLOADM command
LA08E   
CmdCloadM	
	CLR     <CasStatus              ; Zero tape status      
        JSR     <BasChrGet              ; get next character skip the 'M'
        JSR     >GetCasFnameToBuff      ; get filename to search for                                          
CmdCloadMEntry        
        JSR     >CasCheckFindOpen       ; Find and open file
        TST     CasGapFlag         	; Gapped?
        LBEQ    LB73C
        
        LDU     CasFType 	        ; get filetype & ascii flag
        DEC     <TextDevN                               
        JSR     >CasOnReadBlockOff      ; Read first block into tape buffer
        
        TFR     U,D                     ; get filetype (msb) and ascii flag (lsb)

; strip a load offset from the basic line, then load in blocks of data (cloadm,dloadm)
; which are preceeded by a 5 byte pre or post-amble. 
; the preamble contains a block length and a load address so that any number of non-contiguous 
; blocks may be loaded. the post-amble will terminate the loading process and provide a transfer address
;
LA0A9   SUBD    #$0200                  ; check for binary (msb) and non-ascii (lsb)
        BNE     LA08B                   ; non zero so error
        
        LDX     <DBZero       ; Zero X reg, default offset
        JSR     <BasChrGetCurr          ; get next character
        BEQ     LA0BA                   ; nothing, so no offset addr
        
        JSR     >VarCKComma       	; check for comma, error if not
        JSR     >VarGet16Bit 	      	; get offset address
        
LA0BA   STX     <BasCloadMOffs          ; save offset address
        JSR     >LB7F9                  ; check for further characters : error if found

LA0BF   BSR     TextWaitKeyCurs         ; Read a byte from tape (eof flag)
        PSHS    A			; save eof flag on stack
        BSR     IOReadD                 ; Read D from tape move to Y
        TFR     D,Y
        BSR     IOReadD                 ; Read D from tape
        
        ADDD    <BasCloadMOffs          ; Load = Load + Offset                        
        STD     <BasExecAddr            ; save in exec address
        TFR     D,X			; save load address in X
        LDA     ,S+			; get eof flag from stack
        LBNE    LB663			; close files if EOF block
        
LA0D5   BSR     TextWaitKeyCurs 	; get a byte from tape	
        STA     ,X			; save it in RAM
        CMPA    ,X+			; compare saved byte, make sure it saved OK
        BNE     LA0F1			; not same, error bad RAM or ROM
        
        LEAY    -1,Y			; decrement byte count
        BNE     LA0D5			; more bytes, keep going
        BRA     LA0BF			; loop again for more blocks

;
; Read D from active IO stream
;
;LA0E3
IOReadD BSR     LA0E5			; get a character in B
LA0E5   BSR     TextWaitKeyCurs 	; get a character in A
        EXG     A,B			; swap them
LA0E9   RTS

TextWaitKeyCurs:	
LA0EA   JSR     >TextWaitKeyCurs2a	; Read a byte from IO dev
        TST     <CasEOFFlag             ; End of file    
        BEQ     LA0E9                   ; no : return
LA0F1   JMP     >BasIOError		; generate IO error

; used by DLOAD, transmit filename and read in status
LA0F4   BSR     LA115			
        PSHS    D
        INCA
        BEQ     LA101
        LDU     <DBZero	; zero
        BSR     LA108
        PULS    D,PC			; restore and return

LA101   LDB     #ErrNE			; Error, not exist
        JMP     >SysErr 

; refill console in character buffer from dload
LA106   LDU     <CasIOBuffAddr		; get block number?
LA108   LEAX    1,U			; increment block number
        STX     <CasIOBuffAddr		; save it
        LDX     #CasIOBuff 		; use tape buffer as DLOAD buffer
        JSR     >LA17E			; read block into buffer
        JMP     >LB876			; reset console in buffer

LA115   CLRA				; reset attempt counter
        PSHS    D,X			; save regs on stack to make room
        LEAY    ,S			; set Y = S
        BRA     LA11E			; skip ahead

LA11C   BSR     LA149
LA11E   LDA     #$8A			; get file request control code
        BSR     LA159			; transmit it
        BNE     LA11C			; branch if no echo or error
        
	LDX     #CasFName 		; point to filename
LA127   LDA     ,X+			; get a byte
        JSR     >LA1C1			; transmit it
        CMPX    #CasIOBuff 		; end of buffer?
        BNE     LA127			; no keep going
	
        BSR     LA163			; output check byte and look for acknoledge
        BNE     LA11C			; transmit name again if no acknoledge
	
        BSR     LA173			; get file type, $FF=not found
        BNE     LA11C			; branch if error
	STA     2,Y			; save file type
        
	BSR     LA173			; get ASCII flag
        BNE     LA11C			; branch if error
        STA     3,Y			; save ASCII flag
        
	BSR     LA16C			; read checksum byte	
        BNE     LA11C			; branch if error
        LEAS    2,S			; drop saved X			
        PULS    D,PC			; restore and return

; on entry Y points to the stacked registers
; increment retry counter after 5 reties give up, with an IO error
LA149   INC     ,Y			; increment retry count
        LDA     ,Y			; get retry count
        CMPA    #$05			; have we exhausted retries?
        BCS     LA16B			; no, return and try again
	
        LDA     #$BC			; send QUIT-GET abbort code
        JSR     >BasicSerOut 		; send to serial
        JMP     >BasIOError		; generate IO error

; echo check - output a character, read a character and
; compare it to the output character. z=0 if no match or error
LA159   PSHS    A			; save character
        BSR     LA1BA			; send & receive character 
        BNE     LA161			; branch if read error
        CMPA    ,S			; compare character read back the same
LA161   PULS    A,PC			; restore and return

; transmit xor checkbyte and read acknowlege ($c8)
; return zero flag set if no error and acknowlege
LA163   LDA     1,Y			; get XOR checkbyte
        BSR     LA1BA			; output XOR checkbyte and read one byte
        BNE     LA16B			; branch if read error
        CMPA    #$C8			; compare the byte acknoledge code
LA16B   RTS

; read xor checkbyte then load accumulated xor checkbyte.
; set zero flag if accumulated check byte = 0
LA16C   BSR     LA173			; input character from serial
        BNE     LA16B			; branch if timeout
        LDA     1,Y			; get check byte
        RTS

LA173   JSR     >BasicSerIn 		; read byte from serial
        PSHS    CC,A			; save character and flags
        EORA    1,Y			; eor character with checkbyte
        STA     1,Y			; and save it back
        PULS    CC,A,PC			; restore and return

; request a block from rs 232 input -
; load the received data into the buffer pointed to by X
; U regfister contains the block number; 
; return CC.Z=1 if no errors, character count in A; A = 0 if file empty

LA17E   CLRA				; reset attempt counter
        PSHS    D,X,Y,U			; save regs
        ASL     7,S			; bytes 6,7 on stack contain U which contains 14 bit block no.
        ROL     6,S			; put bottom 7 bits in 7,S
        LSR     7,S			; and top 7 bits in 6,S
        LEAY    ,S			; point Y at stack frame
        BRA     LA18D			; 

LA18B   BSR     LA149
LA18D   LDA     #$97			; block request code
        BSR     LA159			; transmit code and wait for echo
        BNE     LA18B			; branch if no match or error
	
        LDA     6,Y			; send out high order 7 bits of block number
        BSR     LA1C1
	
        LDA     7,Y			; send out low order 7 bits of block number
        BSR     LA1C1
	
        BSR     LA163			; transmit check byte and get acknoledge
        BNE     LA18B			; branch if error or no acknoledge
	
        BSR     LA173			; read character count
        BNE     LA18B			; branch if read error
	
        STA     4,Y			; save character count
        LDX     2,Y			; get variables pointer from stack frame

; read in a block of 128 characters - the host will transmit 128
; characters regardless of how many are valid.     	
        LDB     #$80			; 128 characters / buffer	
LA1A9   BSR     LA173			; read a character
        BNE     LA18B			; restart process if read error
        STA     ,X+			; save in buffer
        DECB				; decrement count
        BNE     LA1A9			; loop again if more
	
        BSR     LA16C			; input XOR checkbyte
        BNE     LA18B			; restart if bad checkbyte or read error
	
        LEAS    4,S			; purge stack frame
        PULS    D,X,PC			; restore and return, character count in A

LA1BA   CLR     1,Y			; clear check byte
        BSR     LA1C9			; output character
	
; read a character from the rs 232 input port.
; return character in A.  
; exit with CC.Z=0 for timeout error, CC.Z=1 for valid byte input.
        JMP     >BasicSerIn 		; read character

LA1C1   PSHS    A			; save A
        EORA    1,Y			; generate EOR byte
        STA     1,Y			; save it
        PULS    A			; restore A
LA1C9   JMP     >BasicSerOut 		; output it


; Part of PRINT USING code?
LA1CC   LDA     #$01			; set spaces
        STA     <EvalD9			; counter = 1	
	
; process string item - list
LA1D0   DECB				; decrement format string length counter
        JSR     >LA366			; send a '+' to console out if EvalDA <> 0
        JSR     <BasChrGetCurr		; get current input character
        LBEQ    LA266			; exit print using if end of line
	
        STB     <BasCloadMOffs		; save remainder of format string length
        JSR     >VarGetStr 		; evaluate expression
        JSR     >VarGetExpr 		; 'TM' error if numeric
	
        LDX     <FPA0+2			; get item list descriptor address
        STX     <Eval4D			; and save it
	
        LDB     <EvalD9			; get spaces counter
        JSR     >L8DF3			; put B bytes into string space and put descriptor on string stack
        JSR     >L90E8			; print the formatted string to console out

; pad format string with spaces if item - list string < format string length	
        LDX     <FPA0+2			; point X to fromatted string descriptor address
        LDB     <EvalD9			; get spaces counter
        SUBB    ,X			; subtract length of formatted string
	
LA1F4   DECB				; decrement difference (spaces remaining counter)
        LBMI    LA341			; no more spaces, go interpret next item 
        JSR     >TextOutSpace 		; output a space
        BRA     LA1F4			; loop again

; percent sign - process a %spaces% command
LA1FE   STB     <BasCloadMOffs		; Save the current format string counter
        STX     <BasTempPtr		; and pointer
	
        LDA     #$02			; initial set spaces counter = 2
        STA     <EvalD9			
	
LA206   LDA     ,X			; get a character from format string
        CMPA    #'%'			; compare to terminator		
        BEQ     LA1D0			; branch if end of spaces command
	
        CMPA    #' '			; blank ?
        BNE     LA217			; branch if illegal character
        
	INC     <EvalD9			; increment spaces counter
        LEAX    1,X			; move to next character in format string
        DECB				; decrement length counter
        BNE     LA206			; loop if more
	
LA217   LDX     <BasTempPtr		; restrore current format string pointer 
        LDB     <BasCloadMOffs		; and counter from before spaces command
        LDA     #'%'			; send a % to the screen as a debugging aid

; error processor - illegal character or bad syntax in format string
LA21D   JSR     >LA366			; send a '+' to console out if EvalDA <> 0		
        JSR     >TextOutChar 		; output characrer
        BRA     LA247			; get next character in format string

; Basic PRINT USING
; EvalDA IS USED AS A STATUS BYTE: 
; BIT 6 = COMMA FORCE
; BIT 5 =LEADING ASTERISK FORCE
; BIT 4 = FLOATING $ FORCE
; BIT 3 = PRE SIGN FORCE
; BIT 2 = POST SIGN FORCE
; BIT 0 = EXPONENTIAL FORCE

UsingComma	equ	$40		; comma force
UsingAsterisk	equ	$20		; leading asterisk force
UsingFloatS	equ	$10		; floating dollar
UsingPreSign	equ	$08		; pre-sign force
UsingPostSign	equ	$04		; post sign force
UsingExp	equ	$01		; exponential force

DoPrintUSING   
	JSR     >L8889
        JSR     >VarGetExpr 		; 'TM' error if numeric
        LDB     #';'			; check for item list sperator
        JSR     >VarCKChar 		; syntax check for ';'
	
        LDX     <FPA0+2			; get format string descriptor addresss
        STX     <BasRenumStartLine	; and save it
        BRA     LA23C			; go process format string

LA236   LDA     <BasEditorLineLen	; check next print item flag
        BEQ     LA242			; 'FC' error if no further print items
	
        LDX     <BasRenumStartLine	; reset format pointer to start of string
LA23C   CLR     <BasEditorLineLen	; resset next item flag	
        LDB     ,X			; get length of format string
        BNE     LA245			; interpret format string if length > 0
LA242   JMP     >BasFCError 		; else FC error

LA245   LDX     2,X			; point X ats the start of the format string

; interpret the format string
LA247   CLR     <EvalDA			; clear the status byte
LA249   CLR     <EvalD9			; clear the left digit counter
        LDA     ,X+			; get a character from format string
        
	CMPA    #'!'			; exclamation?
        LBEQ    LA1CC			; yep: string type format
	
        CMPA    #'#'			; number sign (digit locator)
        BEQ     LA2B2			; yep: numeric type format
	
        DECB				; decrement format string length
        BNE     LA270			; branch if not done
	
        JSR     >LA366			; send a '+' to the console if EvalDA <> 0
        JSR     >TextOutChar 		; send character in A to console out		
	
LA260   JSR     <BasChrGetCurr		; get current character from basic
        BNE     LA236			; branch if not end of line
	
        LDA     <BasEditorLineLen	; get next print item flag
LA266   BNE     LA26B			; branch if more print items

        JSR     >TextOutCRLF 		; output a return
LA26B   LDX     <BasRenumStartLine	; point X to format string descriptor
        JMP     >VarDelVar 		; return length and address of format string, exit print

LA270   CMPA    #'+'			; check for '+', pre-sign force
        BNE     LA27D			; nope : skip
	
        JSR     >LA366			;  send a '+' to the console if EvalDA <> 0
        LDA     #UsingPreSign		; load the status byte with $08 (pre sign force)
        STA     <EvalDA			
        BRA     LA249			; interpret rest of format string

LA27D   CMPA    #'.'			; decimal point?
        BEQ     LA2CF			; yes, deal with it
	
        CMPA    #'%'			; precent sign?
        LBEQ    LA1FE			; yes, deal with it
	
        CMPA    ,X			; compare current format string char with next character
LA289   BNE     LA21D			; no match illegal character

; Two consecutive equal characters in string
        CMPA    #'$'			; dollar sign		
        BEQ     LA2A8			; yep, make dollar sign float
	
        CMPA    #'*'			; asterisk?
        BNE     LA289			; no: illegal character
	
        LDA     <EvalDA			; set asterisk flag, to indicate output padded with asterisks
        ORA     #UsingAsterisk
        STA     <EvalDA
	
        CMPB    #$02			; check to see if "$$" are the last 2 characters in format string
        BCS     LA2AE			; branch if so
	
        LDA     1,X			; get the character after "**"
        CMPA    #'$'			; dollar?
        BNE     LA2AE			; check for more characters
	
        DECB				; decrement string length counter
        LEAX    1,X			; move to next character of format string
	
        INC     <EvalD9			; add 1 to digit count for asterisk and
					; floating dollar combination
					
LA2A8   LDA     <EvalDA			; add floating dollar flag to status byte
        ORA     #UsingFloatS
        STA     <EvalDA
	
LA2AE   LEAX    1,X			; move format string pointer to next character
        INC     <EvalD9			; add 1 to digit count for asterisk and
					; floating dollar combination 
					
; process characters to the left of the decimal point in the format string
LA2B2   CLR     <EvalD8			; clear the right digit counter 
LA2B4   INC     <EvalD9			; add 1 to the left digit counter
        DECB				; decrement format string length counter
        BEQ     LA302			; branch if end of format string
	
        LDA     ,X+			; get next format string character
        CMPA    #'.'			; decimal point?
        BEQ     LA2DD			; yes
	
        CMPA    #'#'			; number sign?
        BEQ     LA2B4			; yes
	
        CMPA    #','			; comma?
        BNE     LA2E8			; no 
	
        LDA     <EvalDA			; set comma flag in status byte
        ORA     #UsingComma
        STA     <EvalDA
	
        BRA     LA2B4			; process more characters

; process decimal point if no digits to left of it
LA2CF   LDA     ,X			; get the next charcter from the format string
        CMPA    #'#'			; number sign?
        LBNE    LA21D			; no
	
        LDA     #$01			; set the right digit counter to 1
        STA     <EvalD8
        LEAX    1,X			; move to next character in format string
	
; process digits to right of decimal point
LA2DD   INC     <EvalD8			; add 1 to right digit counter
        DECB				; decrement format string counter
        BEQ     LA302			; end of sring, branch
	
        LDA     ,X+			; get next character from format string
        CMPA    #'#'			; number sign?
        BEQ     LA2DD			; yes keep checking
	
; check for exponential force	
LA2E8   CMPA    #'^'			; up arrow 
        BNE     LA302			; no up arrow
	
        CMPA    ,X			; is the next character '^' ?
        BNE     LA302			; nope
	
        CMPA    1,X			; and the next?
        BNE     LA302			; nope
	
        CMPA    2,X			; and the next? :)
        BNE     LA302			; nope
	
        CMPB    #$04			; check to see if the 4 up arrows are in the format string
        BCS     LA302			; branch if not
	
        SUBB    #$04			; subtract 4 from format string length
        LEAX    4,X			; and move format string pointer up
        INC     <EvalDA			; increment status byte force exponential

; check for a pre or post - sign force at end of format string	
LA302   LEAX    -1,X			; move format string pointer back one
        INC     <EvalD9			; add 1 to digit counter for pre-sign force
        LDA     <EvalDA			; get status byte
        BITA    #UsingPreSign		; check for pre sign bit set?			
        BNE     LA324			; yep 
	
        DEC     <EvalD9			; decrement left digit no pre-sign force
        TSTB				; test length counter
        BEQ     LA324			; branch if zero (end of format strings)
	
        LDA     ,X			; get next fromat byte
        SUBA    #'-'			; check for minus sign
        BEQ     LA31D			; yep 
	
        CMPA    #('+')-('-')		; check for '+' sign
        BNE     LA324			; nope
	
        LDA     #UsingPreSign		; get pre sign flag
LA31D   ORA     #UsingPostSign		; or in post sign flag
        ORA     <EvalDA			; or in bits currently set
        STA     <EvalDA			; resave status byte
	DECB				; decrement format string length

; evaluate numeric item list
LA324   JSR     <BasChrGetCurr		; get current input character
        LBEQ    LA266			; branch if end of line, exit 
	
        STB     <BasCloadMOffs		; save format string length when format evaluation ended
        JSR     >L8872			; evaluate expression
	
        LDA     <EvalD9			; get the left digit counter
        ADDA    <EvalD8			; add to right digit counter
        CMPA    #$11			; more than 16 digits + decimal point?
        LBHI    BasFCError 		; yes : FC error

        JSR     >LA373			; convert item list to formatted ASCII string
        LEAX    -1,X			; move buffer pointer back one
        JSR     >TextOutString 		; output the string to the console
	
LA341   CLR     <BasEditorLineLen	; reset print item flag
        JSR     <BasChrGetCurr		; get current input character
        BEQ     LA354			; branch if end of line
	
        STA     <BasEditorLineLen	; save current character (<>0) in the next print item flag
        CMPA    #';'			; check for ';' item seperator, and branch if found
        BEQ     LA352
	
        JSR     >VarCKComma 		; syntax check for comma
        BRA     LA354			; process next print item

LA352   JSR     <BasChrGet		; get current input character
LA354   LDX     <BasRenumStartLine	; get format string descriptor address	
        LDB     ,X			; get length of format string
        SUBB    <BasCloadMOffs		; subtract amount of format string left after last print item
        LDX     2,X			; get format string start address and advance
        ABX				; pointer to begining of unused string
        LDB     <BasCloadMOffs		; get amount of unused format string
        LBNE    LA247			; reinterpret format string from the start if entirely
        JMP     >LA260			; used on last print item

; Output a + if EvalDA <> 0
LA366   PSHS    A			; save A
        LDA     #'+'			; setup character
        TST     <EvalDA			; test EvalDA
        BEQ     LA371			; EvalDA is zero, skip
        JSR     >TextOutChar 		; output the '+'
LA371   PULS    A,PC			; restroe and return

LA373   LDU     #BasBuffer+4		; point u at string buffer
        LDB     #' '			; space
        LDA     <EvalDA			; get status flag
        
	BITA    #UsingPreSign		; check for pre-sign force
        BEQ     LA380			; yes 
        
	LDB     #'+'			; plus sign
LA380   TST     <FP0SGN			; check the sign of FPA0
        BPL     LA388			; branch if +ve
	
        CLR     <FP0SGN			; force FPA0 sign +ve
        LDB     #'-'			; minus sign
	
LA388   STB     ,U+			; save sign into buffer
        LDB     #'0'			; put a '0' in buffer
        STB     ,U+
	
        ANDA    #UsingExp		; check for exponential flag in status byte
        LBNE    LA49B			; branch if active
	
        LDX     #D956E			; point X at FP number 1E+09
        JSR     >FPA0cmpX		; compare it to FPA0
        BMI     LA3B1			; branch if FPA0 < 1e+09
	
        JSR     >L9587			; convert FPA0 to string

LA39F   LDA     ,X+			; advance pointer to end of ASCII string
        BNE     LA39F			
					; move the entire string back one
LA3A3   LDA     ,-X			; get a byte, move pointer back	
        STA     1,X			; put a byte in string
        CMPX    #BasBuffer+3		; at beginning of buffer?
        BNE     LA3A3			; nope keep going
	
        LDA     #'%'			; put a '%' at beginning of buffer
        STA     ,X			; flag overflow error
        RTS

LA3B1   LDA     <FP0EXP			; get FPA0 exponent
        STA     <Eval47			; and save in Eval47
        BEQ     LA3BA			; branch if FPA0 = 0
	
        JSR     >LA55B			; convert FPA0 to number with 9 significant digits
					; to the left of the decimal point
LA3BA   LDA     <Eval47			; recoever exponent
        LBMI    LA441			; branch if FPA0 < 100,000,000
	
        NEGA				; calculate the number of leading zeros to insert
        ADDA    <EvalD9			; subtract base 10 exponent offset and 9 from digit counter.
        SUBA    #$09			; (FPA0 has 9 places to the left of exponent)
	
        JSR     >LA478			; put A zeroes in buffer
        JSR     >LA5F1			; initialize decimal point and comma counters
        JSR     >LA590			; convert FPA0 to decimal ASCII in string buffer
	
        LDA     <Eval47			; get base 10 exponent
        JSR     >LA60F			; and put that many zeros in string buffer
	
        LDA     <Eval47			; wasted instruction serves no purpose!
        JSR     >LA5D7			; check for decimal point
	
        LDA     <EvalD8			; get right digit counter
        BNE     LA3DE			; branch if right digit counter nonzero
	
        LEAU    -1,U			; move pointer back one, delete decimal point if no 
					; right zeroes
LA3DE   DECA				; subtract one (decimal point)
        JSR     >LA478			; put A zeroes in buffer
LA3E2   JSR     >LA513			; inser asterisk padding, floating $ and post-sign

        TSTA				; was there a post sign?
        BEQ     LA3EE			; no, skip 
        
	CMPB    #'*'			; is the first character a '*' ?
        BEQ     LA3EE			; yes, skip
	
        STB     ,U+			; store post-sign
LA3EE   CLR     ,U			; clear last character in buffer

; remove any blanks or asterisks to the left of the decimal point
        LDX     #BasBuffer+3		; point to string buffer
LA3F3   LEAX    1,X			; move buffer pointer up one
        STX     <BasTempPtr		; save buffer pointer
	
        LDA     <BasVarPtrLast+1	; get address of decimal point in string buffer
        SUBA    <BasTempPtr+1		; subtract current position
        SUBA    <EvalD9			; subtract left digits counter
					; the result will be 0 when BasTempPtr+1 is pointing to 
					; the first digit of the format string
        BEQ     LA437			; return if no digits to the left of the decimal point
	
        LDA     ,X			; get the current buffer character
        CMPA    #' '			; space?
        BEQ     LA3F3			; yep, advance pointer
 
        CMPA    #'*'			; asterisk?
        BEQ     LA3F3			; yep, advance pointer
        
	CLRA				; a zero on the stack is the end of the data pointer
LA40A   PSHS    A			; save on stack
        LDA     ,X+			; get next character from buffer
        
	CMPA    #'-'			; minus?
        BEQ     LA40A			; yes
	
        CMPA    #'+'			; plus?
        BEQ     LA40A			; yes
	
        CMPA    #'$'			; dollar?
        BEQ     LA40A			; yes
	
        CMPA    #'0'			; zero?
        BNE     LA42C			; no : error
	
        LDA     1,X			; get character following zero
        BSR     LA438			; clear carry if numeric
        BCS     LA42C			; branch if not numeric, error
	
LA424   PULS    A			; pull a character off the stack
        STA     ,-X			; put it back in the string buffer
        BNE     LA424			; keep going until the zero flag
        BRA     LA3F3			; keep cleaning up input buffer

LA42C   PULS    A			; remove characters on the stack and
        TSTA				; terminate when zero flag found
        BNE     LA42C			; non-zero loop again
        LDX     <BasTempPtr		; get the string buffer start pointer
        LDA     #'%'			; put a % before error position to indicate error
        STA     ,-X			; store it
LA437   RTS

; clear carry if numeric
LA438   CMPA    #'0'			; ASCII zero
        BCS     LA440			; return if A < '0'
        SUBA    #'9'+1			; adjust to be zero based
        SUBA    #-('9'+1)		; clear carry if numeric
LA440   RTS

; process an item-list which is < 100,000,000
LA441   LDA     <EvalD8			; get right digit counter
        BEQ     LA446			; branch if no formmated digits to the right of decimal point
	
        DECA				; subtract 1 for decimal point
LA446   ADDA    <Eval47			; add the base 10 exponent offset A contains the
					; number of shifts required to adjust fpa0 to the specified
					; number of dlgits to the right of the decimal point
        BMI     LA44B			; if A >= 0 no shifts required
        
	CLRA				; force shift counter = 0
LA44B   PSHS    A			; save initial shift counter on stack
LA44D   BPL     LA459			; exit routine if +ve

        PSHS    A			; save shift counter on stack
        JSR     >FPA0div10		; Divide FPA0 by 10, shift one place right
        PULS    A			; restore shift counter
	
        INCA				; increment counter
        BRA     LA44D			; loop again

LA459   LDA     <Eval47			; get base 10 exponent offset and add initial shift counter
        SUBA    ,S+			; and save new base 10 exponent offset.  
        STA     <Eval47			; because FPA0 was shifted above
	
        ADDA    #$09			; add 9 (significant places)
        BMI     LA47C			; and branch if no zeroes to the left of the decimal point
	
        LDA     <EvalD9			; determine how many filler zeroes to the left of the decimal point
        SUBA    #$09			; get the number of format places to the left of the decimal point
        SUBA    <Eval47			; subtract base 10 exponent offset and the constant 9 (unnormalization)
	
	BSR     LA478			; output that many zeroes to the buffer
        JSR     >LA5F1			; initialize decimal point and comma counters
        BRA     LA48D			; process the remainder of the print item

; put A+1 ASCII '0' in the buffer
LA470   PSHS    A			; save count on stack
        LDA     #'0'			; put '0' in buffer
        STA     ,U+			; store it, increment pointer
        PULS    A			; recorver counter

; put A ASCII '0' in the buffer
LA478   DECA				; decrement counter
        BPL     LA470			; loop again whilst +ve
        RTS

LA47C   LDA     <EvalD9			; get the left digit counter
        BSR     LA478			; and put that many zeroes in string buffer
        JSR     >LA5DB			; put the decimal point in the string buffer
	
        LDA     #-9			; determine how many filler zeroes between decimal point
        SUBA    <Eval47			; and significant data. Subtract base 10 exponent from -9
	BSR     LA478			; (unnormalization) and output that many zeroes to buffer
	
        CLR     <Eval45			; clear decimal point counter
        CLR     <BasEditorLineLen	; clear comma counter
LA48D   JSR     >LA590			; decode FPA0 into decimal ASCII string

        LDA     <EvalD8			; get the right digit counter
        BNE     LA496			; branch if right digit counter <> 0
        LDU     <BasVarPtrLast		; reset buffer pointer to decimal point if no digits to right
LA496   ADDA    <Eval47			; add basse 10 exponent, +ve value will cause that many filler
					; zeroes to be output to the right of the last significant data
        LBRA    LA3DE			; insert leading asterisks, floating dollar sign etc.

; force the numeric output format to be exponential format
LA49B   LDA     <FP0EXP			; get exponent of FPA0		
        PSHS    A			; save on stack
        BEQ     LA4A4			; branch if FPA0 is zero
	
        JSR     >LA55B			; convert FPA0 to a decimal number with 9 significant digits
					; to the left of the decimal point
LA4A4   LDA     <EvalD8			; get the right digit counter
        BEQ     LA4A9			; branch if no formatted digits to the right
	
        DECA				; subtract 1 for the decimal point
LA4A9   ADDA    <EvalD9			; add the left digit counter
        CLR     BasBuffer+3		; clear buffer as temp storage location
        LDB     <EvalDA			; get the status byte 
        ANDB    #UsingPostSign		; check for post sign flag
        BNE     LA4B7			; branch if post sign force
	
        COM     BasBuffer+3		; toggle buffer byte to -1 if no post byte force
LA4B7   ADDA    BasBuffer+3		; subtract 1 if no post byte force
        SUBA    #$09			; subtract 9 due to conversion of 9 significant
					; digits to the left of the decimal point
					
        PSHS    A			; save shift counter on stack A contains number of shifts
					; required to adjust FPA0 for the number of formatted
					; decimal places
LA4BE   BPL     LA4CA			; no more shifts when A >= 0
        PSHS    A			; save shift counter
        JSR     >FPA0div10		; divide FPA0 by 10, shift to right one
        PULS    A			; restore shift counter
        INCA				; add 1 to shift counter
        BRA     LA4BE			; loop again

LA4CA   LDA     ,S			; get the initial value of the shift counter
        BMI     LA4CF			; and branch if shifting has taken place
        
	CLRA				; reset A if no shifting has taken place
LA4CF   NEGA				; calculate the position of the decimal point negating shift count
        ADDA    <EvalD9			; adding the left digit counter
        INCA				
        ADDA    BasBuffer+3		; and adding the post byte count if used
	
        STA     <Eval45			; save decimal point counter
        CLR     <BasEditorLineLen	; clear comma counter, no commas inserted	
        JSR     >LA590			; convert FPA0 into ASCII decimal string
        PULS    A			; get the initial value of the shift counter
	
        JSR     >LA60F			; insert that many zeroes into the buffer
	
        LDA     <EvalD8			; get right digit counter
        BNE     LA4E8			; branch if not zero
	
        LEAU    -1,U			; move buffer pointer back one
LA4E8   LDB     ,S+			; get original exponent of FPA0
        BEQ     LA4F5			; branch if exponent = 0
	
        LDB     <Eval47			; get base 10 exponent
        ADDB    #$09			; add 9 for significant digit conversion
        SUBB    <EvalD9			; subtract left digit counter
        SUBB    BasBuffer+3		; add 1 if post sign force
	
LA4F5   LDA     #'+'			; get plus sign
        TSTB				; text exponent
        BPL     LA4FD			; branch if +ve
	
        LDA     #'-'			; get minus sign
        NEGB				; convert exponent to +ve number
LA4FD   STA     1,U			; put sign of exponent in string buffer
        LDA     #'E'			; put an 'E' exponention flag 		
        STA     ,U++			; in buffer, move pointer past it 
        LDA     #'0'-1			; initialize tens digit to ASCII zero -1 (to compensate for INCA)

LA505   INCA				; increment 10s digit counter
        SUBB    #$0A			; subtract 10 from the exponent
        BCC     LA505			; if still +ve then loop again
	
        ADDB    #'9'+1			; add ASCII bias to units digit
        STD     ,U++			; save ASCII exponent in buffer	
        CLR     ,U			; clear final byte to terminate buffer
        JMP     >LA3E2			; insert asterisk padding floating dollar sign etc

; insert asterisk padding, floating $ and pre-sign
LA513   LDX     #BasBuffer+4		; point X at start of item buffer
        LDB     ,X			; get sign byte of item list buffer
        PSHS    B			; save it on stack
        LDA     #' '			; space
        LDB     <EvalDA			; get status byte
        BITB    #UsingAsterisk		; check asterisk padding flag
        PULS    B			; restore sign, note flags not touched 
        BEQ     LA52C			; branch if no padding
	
        LDA     #'*'			; pad character, astersik
        CMPB    #' '			; was the first byte a blank? (+ve)
        BNE     LA52C			; no 
	
        TFR     A,B			; transfer pad character to B
LA52C   PSHS    B			; save first character on stack
LA52E   STA     ,X+			; store pad character in buffer
        LDB     ,X			; get next character in buffer
        BEQ     LA544			; branch if end of buffer, add trailing zero 
	
        CMPB    #'E'			; check for an 'E' 
        BEQ     LA544			; yes: put a zero before it
	
        CMPB    #'0'			; check for a zero
        BEQ     LA52E			; replace leading zeros with pad characters.
	
        CMPB    #','			; comma?
        BEQ     LA52E			; replace leading commas with pad characters
	
        CMPB    #'.'			; decimal point?
        BNE     LA548			; don't put a zero before it
	
LA544   LDA     #'0'			; replace previous character with '0' character
        STA     ,-X			; store it
LA548   LDA     <EvalDA			; get status byte
        BITA    #UsingFloatS		; are we using floating '$'
        BEQ     LA552			; branch if not
	
        LDB     #'$'			; store '$' in buffer
        STB     ,-X
LA552   ANDA    #UsingPostSign		; check for post sign flag
        PULS    B			; get saved sign character, flags unchanged
        BNE     LA55A			; return if post sign required
        STB     ,-X			; store first character
LA55A   RTS

; convert FPA0 into a number of the form - nnn,nnn,nnn x 10**m.
; the exponent m will be returned in Eval47 (base 10 exponent).
LA55B   PSHS    U			; save buffer pointer
        CLRA				; initial exponent offset = 0
	
LA55E   STA     <Eval47			; save exponent offset
        LDB     <FP0EXP			; get FPA0 exponent
        CMPB    #$80			; compare to exponent of 0.5
        BHI     LA577			; branch if FPA0 > 1.0
	
; if FPA0 is less than 0.5, multiply by 1E+09 until it is >= 1
        LDX     #D956E			; point to FP constant 1E+09
        JSR     >XtimesFPA0		; multiply FPA0 by constant
        LDA     <Eval47			; get exponent offset
        SUBA    #$09			; subtract 9, because we multiplied by 1E+09 above
        BRA     LA55E			; check to see if > 1.0

LA572   JSR     >FPA0div10		; divide FPA0 by 10	
        INC     <Eval47			; increment exponent offset to compensate
	
LA577   LDX     #D9569			; FP constant 999,999,999
        JSR     >FPA0cmpX		; compare to FPA0
        BGT     LA572			; branch if FPA0 greater
	
LA57F   LDX     #D9564			; FP constant 99,999,999.9
        JSR     >FPA0cmpX		; compare to FPA0
        BGT     LA58E			; return if 999,999,999 > FPA0 > 99,999,999.9 
	
        JSR     >FPA0mul10		; Multiply FPA0 by 10
        DEC     <Eval47			; decrement exponent offset to compensate
        BRA     LA57F			; loop again

LA58E   PULS    U,PC			; restore and return

; convert fpa0 into an integer, then decode it into a decimal ascii string in the buffer
LA590   PSHS    U			; save buffer pointer
        JSR     >L90FD			; add 0.5 to FPA0 (round off)
        JSR     >L9473			; convert FPA0 to integer format
        PULS    U			; restore buffer pointer

; convert FPA0 into a decimal ASCII string	
        LDX     #PowersOf10		; point X at unnormalized powers of 10
        LDB     #$80			; initialize digit counter to 0+$80

; bit 7 set is used to indicate that the power of 10 mantissa is negative. 
; when you `add' a negative mantissa, it is the same as subtracting a positive one.
; bit 7 of B is how this routine knows that a `subtraction' is occurring.	
LA59F   BSR     LA5D7			; check for comma insertion
LA5A1   LDA     <FPA0+3			; add a power of 10 mantissa to FPA0
        ADDA    3,X			; if the mantissa is -ve a subtraction
        STA     <FPA0+3			; will take place
        LDA     <FPA0+2
        ADCA    2,X
        STA     <FPA0+2
        LDA     <FPA0+1
        ADCA    1,X
        STA     <FPA0+1
        LDA     <FPA0
        ADCA    ,X
        STA     <FPA0
        INCB				; add 1 to digit counter
        RORB				; rotate carry into bit 7
        ROLB				; set overflow flag
        BVC     LA5A1			; branch if carry set and adding mantissa
        BCC     LA5C3			; brnach if carry clear and subtracting mantissa
        SUBB    #10+1			; take the 9's complement
        NEGB				; if adding mantissa
	
LA5C3   ADDB    #'0'-1			; add ascii offset
        LEAX    4,X			; move to next power of 10 mantissa
        TFR     B,A			; save digit in A
        ANDA    #$7F			; mask off add / subtract flag in bit 7
        STA     ,U+			; save digit in buffer
        COMB				; toggle add / subtract flag
        ANDB    #$80			; extract add / subtract flag
        CMPX    #PowersOf10End		; compare to end of powers of 10 		
        BNE     LA59F			; no : loop again
	
        CLR     ,U			; zero at end of buffer
	
; decrement decimal point counter and check for comma insertion
LA5D7   DEC     <Eval45			; decrement comma counter
        BNE     LA5E4			; return if not time for comma
	
LA5DB   STU     <BasVarPtrLast		; save buffer pointer position of decimal point
        LDA     #'.'			; point character
        STA     ,U+			; save it in the buffer
        CLR     <BasEditorLineLen	; clear comma counter, will now take 256 decrements 
					; before another comma inserted 	
        RTS

LA5E4   DEC     <BasEditorLineLen	; decrement comma counter
        BNE     LA5F0			; branch if not time for another comma
        LDA     #$03			; reset comma count to 3
        STA     <BasEditorLineLen	
        LDA     #','			; store the comma in buffer
        STA     ,U+
LA5F0   RTS

; initialize decimal point and comma counters
LA5F1   LDA     <Eval47			; get the base 10 exponent offset	
        ADDA    #$0A			; add 10 (FPA0 was 'normalized' 9 places to the left 
					; of decimal point)
        STA     <BasVarFPAcc4		; save decimal point counter
        INCA				; add 1 for the decimal point
LA5F8   SUBA    #$03			; devicde decimal point counter by 3, remainer in A
        BCC     LA5F8			;
        ADDA    #$05			; convert remainder into a number 1..3
        STA     <BasEditorLineLen	; save comma counter
        LDA     <EvalDA			; get status byte
        ANDA    #UsingComma		; check for comma flag
        BNE     LA608			; branch if comma active
        STA     <BasEditorLineLen	; clear comma counter, will now take 256 decrements 
					; before another comma inserted 	
LA608   RTS

; insert A zeroes into the buffer
LA609   PSHS    A			; save zeroes counter
        BSR     LA5D7			; check for decimal point
        PULS    A			; recover zeroes counter

LA60F   DECA				; decrement zeroes counter
        BMI     LA61C			; return if counter < 0
        PSHS    A			; save zeroes counter
        LDA     #'0'			; zero character
        STA     ,U+			; insert it
        LDA     ,S+			; recover counter
        BNE     LA609			; loop if more zeroes to insert
LA61C   RTS

; ******* graphics package ********

; get the address of the routine which will convert hor & ver coordinates into
; an absolute ram address and pixel mask depending upon the current pmode.
; return the address in u.
GetCoToAbsAddr   
	LDU     #DA62A			; point to table
        LDA     <GrCurrPmode		; get current PMODE
        ASLA				; multiply by 2 to get offset
        LDU     A,U			; get the addresss of the routine in U
        RTS

; convert ver coord (GrCurrY) & nor coord (GrCurrX) into 
; absolute screen addr in X and pixel mask in A.
CoToAbsAddr   
	BSR     GetCoToAbsAddr		; get addrress of routine
        JMP     ,U			; call it!

; address table of conversiion routines
DA62A   FDB     Conv2Colour		; pmode 0 
        FDB     Conv4Colour		; pmode 1
        FDB     Conv2Colour		; pmode 2
        FDB     Conv4Colour		; pmode 3
        FDB     Conv2Colour		; pmode 4

; The majority of the code in these two routines is the same! 
; optimizing for space would convert into two stubs and a common subroutine.
; However this may be slightly slower.
Conv2Colour   
	PSHS    B,U			; save regs
        LDB     <GrBytesPerLine		; get number of bytes per graphic row
        LDA     <GrCurrY+1		; get vertical coordinate
        MUL				; multiply them to get offset
        ADDD    <GrDisplayStartAddr	; add current graphics start address 
        TFR     D,X			; point to it in X
	
        LDB     <GrCurrX+1		; get current X coordinate
        LSRB				; divide by 8 as 8 pixels / byte
        LSRB				; in 2 colour modes
        LSRB
        ABX				; add byte offset to address
	
        LDA     GrCurrX+1		; get current X coordinate	
        ANDA    #$07			; keep only bottom 3 bits, these contain the pixel no within byte
        LDU     #PixMaskTable2Col 	; point at pixel mask table
        LDA     A,U			; get the correct mask
        PULS    B,U,PC			; restore and return

Conv4Colour   
	PSHS    B,U			; save regs
        LDB     <GrBytesPerLine		; get number of bytes per graphic row
        LDA     <GrCurrY+1		; get vertical coordinate
        MUL				; multiply them to get offset
        ADDD    <GrDisplayStartAddr	; add current graphics start address 
        TFR     D,X			; point to it in X

        LDB     <GrCurrX+1		; get current X coordinate
        LSRB				; divide by 4 as 4 pixels / byte
        LSRB				; in 4 colour modes
        ABX				; add byte offset to address

        LDA     GrCurrX+1		; get current X coordinate	
        ANDA    #$03			; keep only bottom 2 bits as, these contain pixel no within byte
        LDU     #PixMaskTable4Col 	; point at the pixel mask table
        LDA     A,U			; get mask
        PULS    B,U,PC			; restore and return

; 2 colour pixel mask table
PixMaskTable2Col:
	FCB	$80
	FCB	$40
	FCB	$20
	FCB	$10
        FCB     $08
        FCB     $04
        FCB     $02
        FCB     $01

PixMaskTable4Col:
	FCB	$C0
	FCB	$30
	FCB	$0C
	FCB	$03

; Move X down one graphic row
LA677   LDB     <GrBytesPerLine		; get bytes per line
        ABX				; add it
        RTS

; enter with absolute screen position in X and the pixel mask in A 
; adjust X and A to the next pixel to the right in the two color mode.
LA67B   LSRA				; shift pixel mask one bit right
        BCC     LA681			; branch if in same byte, exit
	
        RORA				; if we have moved to the next byte set bit 7 in mask
LA67F   LEAX    1,X			; and add 1 to the RAM address
LA681   RTS

; move absolute screen address of current horizontal, verertical co-ordinate one to right 
; and adjust the pixel mask for the 4 color mode
LA682   LSRA				; shift pixel mask one bit right
        BCC     LA67B			; shift again if same byte
        LDA     #$C0			; otherwise load mask for leftmost pixel
        LEAX    1,X			; and advance to next byte
        RTS

; evaluate two expressions from basic
; 	put the first value (horizontal co-ordinate) in GrCurrX
;	put the second second (vertical co-ordinate) in GrCurrY.
LA68A   JSR     >L8E7A			; evaluate 2 expressions from basic line
					; return first value in BasTempLine, second in B
        LDY     #GrCurrX 		; point at graphic co-ordinate locations
LA691   CMPB    #192			; is vertical co-ordinate bigger than max (192)?
        BCS     LA697			; no, continue
	
        LDB     #$BF			; otherwise clip to the maximum
LA697   CLRA				; clear MSB of co-ordinate
        STD     2,Y			; save it
        LDD     BasTempLine		; get horizontal co-ordinate			
        CMPD    #$0100			; bigger than 256?
        BCS     LA6A5			; no, store it
	
        LDD     #$00FF			; otherwise clip to maximum
LA6A5   STD     ,Y			; save X co-ordinate
        RTS

; normalize horizontal and vertical coordinates for the proper pmode
; return normalized values in GrCurrX, GrCurrY
LA6A8   JSR     >LA68A			; go get co-ordinates from basic
NormalizeXY
	LDU     #GrCurrX 		; point to co-ordinate store
LA6AE   LDA     <GrCurrPmode		; get current pmode
        CMPA    #$02			; check mode
        BCC     LA6BA			; branch if > pmode 1
	
        LDD     2,U			; get Y co-ordinate
        LSRA				; divide by 2 since pmode 0,1 have only 96 
        RORB				; vertical pixels
        STD     2,U			; update Y co-ordinate
	
LA6BA   LDA     <GrCurrPmode		; get current pmode
        CMPA    #$04			; is it pmode 4?
        BCC     LA6C6			; branch if mode 4
        LDD     ,U			; get X coordinate
        LSRA				; divide by 2 as pmodes 2,3 have only 128
        RORB				; horizontal pixels
        STD     ,U			; update X co-ordinate
LA6C6   RTS

; basic PPOINT 
CmdPPoint:
LA6C7   JSR     >BasGetPixel		; get two parameters from basic in GrCurrX, GrCurrY
        JSR     >NormalizeXY		; normalize for PMODE
        JSR     >CoToAbsAddr		; convert co-ordintes to absolute address and mask
        ANDA    ,X			; and pixel mask with contents of screen
        LDB     <GrCurrPmode		; get current pmode
        RORB				; shift right
        BCC     LA6E9			; branch if 2 colour mode (pmode 0,2,4)
	
LA6D7   CMPA    #$04			; is the on pixel in the two rightmost bits?
        BCS     LA6DF			; branch if so
	
        RORA				; rotate right 2 times
        RORA
        BRA     LA6D7			; loop again

LA6DF   INCA				; add 1 to colour basic uses 1..4 not 0..3
        ASLA				; times 2
        ADDA    <GrColourSet		; add colour set 0 or 8	
        LSRA				; divide by 2 colours are 0..8
LA6E4   TFR     A,B			; transfer to B
        JMP     >VarAssign8Bit 		; return it to basic

LA6E9   TSTA				; is 2 colour pixel on?
        BEQ     LA6E4			; no
        CLRA				; force 2 colour on value to be 1 or 5 depending on CSS
        BRA     LA6DF

; basic PSET
CmdPset:
LA6EF   LDA     #$01			; pset flag
        BRA     LA6F4

; basic PRESET
CmdPReset:
LA6F3   CLRA				; preset flag

; common pset/preset code
LA6F4   STA     <GrPlotFlag		; save plot / erase flag
        JSR     >VarCKOpBrac 		; syntax check for open brackets
        JSR     >LA6A8			; get X and Y co-ordinates and normalize
        JSR     >BasGetColour		; evaluate colour, return colour in GrCurrColour
        JSR     >VarCKClBrac 		; syntax check close brackets
        JSR     >CoToAbsAddr		; convert co-ordinates to absolute address & mask
	
; change the pixel, pointed to by X, mask in A, colour in GrCurrColour.
; set GrDirtyFlag, if pixel changed colour.
LA705   LDB     ,X			; get the byte from the screen
        PSHS    B			; save it on stack
        TFR     A,B			; put pixel mask in B
        COMA				; invert pixel mask in A
        ANDA    ,X			; and with data on screen, keep all pixels except the
					; one we are changing
        ANDB    <GrCurrColour		; convert the pixel mask to the correct colour
        PSHS    B			; save on stack
        ORA     ,S+			; or in the rest of the pixels
        STA     ,X			; save it back on screen
        SUBA    ,S+			; subtract old byte (on screen) from new byte
        ORA     <GrDirtyFlag		; or difference with GrDirtyFlag
        STA     <GrDirtyFlag		; GrDirtyFlag will be 0 if the byte is unchanged
        RTS

; evaluate two sets of co-ordinates e.g. for LINE command (x,y)-(x1,y1)
; place x,y in GrCurrX, GrCurrY and x1,y1 in GrPixelNoX, GrPixelNoY
LA71D   LDX     <GrCurrXCo
        STX     <GrCurrX
        LDX     <GrCurrYCo
        STX     <GrCurrY
	
        CMPA    #DTokMINUS		; Minus token?
        BEQ     LA72C			; yep no start co-ordinates
        JSR     >BasGetPixel		; get starting co-ordinates
	
LA72C   LDB     #DTokMINUS		; syntax check for minus
        JSR     >VarCKChar 		
        JSR     >VarCKOpBrac 		; syntax check for open brackets
        JSR     >L8E7A			; get end co-ordinates
        LDY     #GrPixelNoX 		; point Y at place to store them
        JSR     >LA691			; save them
        BRA     LA746			; syntax check for close bracket

BasGetPixel   
	JSR     >VarCKOpBrac 		; syntax check open baracket
        JSR     >LA68A			; get pixel co-ordinates
LA746   JMP     >VarCKClBrac 		; syntax check close bracket

; basic LINE command
CmdLine:
LA749   CMPA    #DTokINPUT		; Check for INPUT token 
        LBEQ    CmdLineInput 		; yes, do LINE INPUT
	
        CMPA    #'('			; open bracket?
        BEQ     LA75C			; yes, look for start and end points
	
        CMPA    #DTokMINUS		; check for minus sign, no start point given
        BEQ     LA75C			; skip ahead

; LINE@ syntax unknown and maybe redundent!	
        LDB     #'@'			; syntax check for @
        JSR     >VarCKChar 
	
LA75C   JSR     >LA71D			; get start and end co-ordinates
        LDX     <GrPixelNoX		; get end X co-ordinate
        STX     <GrCurrXCo		; put in last used X end
        LDX     <GrPixelNoY		; likewise for Y
        STX     <GrCurrYCo
	
        JSR     >VarCKComma 		; syntax check for comma
        CMPA    #DTokPRESET		; is next token PRESET?
        BEQ     LA777			; yep
	
        CMPA    #DTokPSET		; is it PSET?
        LBNE    BasSNError 		; nope, generate SN error
	
        LDB     #$01			; pset flag

        FCB	Skip1LD
LA777   CLRB				; preset flag

        PSHS    B			; save set/reset flag
        JSR     <BasChrGet		; get next character
        JSR     >LA7AE			; normalize start / end co-ordinates
        PULS    B			; restore set/reset flag
	
        STB     <GrPlotFlag		; save it
        JSR     >GrSetColours 		; set active colour byte
        JSR     <BasChrGetCurr		; get another character
        LBEQ    LA82F			; branch if no box to be drawn
	
        JSR     >VarCKComma 		; syntax check for comma
        LDB     #'B'			; syntax check for 'B' for box
        JSR     >VarCKChar 
        BNE     LA7B7			; found a 'B' and something follows

; At this point we want to draw a non filled box 	
        BSR     DrawHLine		; draw horizontal line from x1,y1 to x2,y1
        BSR     DrawVLine		; draw vertical line x1,y1 to x1,y2
        
	LDX     <GrCurrX		; get start X co-ordinate
        PSHS    X			; save on stack
        LDX     <GrPixelNoX		; get end X co-ordinate
        STX     <GrCurrX		; save in start X co ordinate
        BSR     DrawVLine		; draw vertical line from x2,y1 to x2,y2
        PULS    X			; restore start x co-ordinate
        STX     <GrCurrX
	
        LDX     <GrPixelNoY		; get end Y co-ordinate
        STX     <GrCurrY		; save in start y co-ordinate
        BRA     DrawHLine		; draw horizontal line x1,y2 to x2,y2, 
					; then return to caller

* normalize start coords in (GrCurrX,GrCurrY) & end coords in (GrPixelNoX,GrPixelNoY)
LA7AE   JSR     >NormalizeXY		; normalize co-ordinates in GrCurrX,GrCurrY
        LDU     #GrPixelNoX 		; point at end co-ordinates
        JMP     >LA6AE			; normalize them

LA7B7   LDB     #'F'			; check for 'F' to fill the box?
        JSR     >VarCKChar 		; syntax check for F, SNError if not found
        BRA     LA7C2			; skip ahead

; draw a series of horizontal lines from y1 to y2
LA7BE   LEAX    -1,X			; move Y co-ordinate up one
LA7C0   STX     <GrCurrY		; save it

LA7C2   JSR     >DrawHLine		; draw horizontal line from x,y1 to x,y2
        LDX     <GrCurrY		; get current Y co-ordinate
        CMPX    <GrPixelNoY		; compare to y2 specified in command
        BEQ     LA7D1			; yep, drawn all box, exit
        BCC     LA7BE			; if x1 > x2 then loop to subtract x
        LEAX    1,X			; else increment x
        BRA     LA7C0			; loop to next

LA7D1   RTS

; draw a horizontal line from GrCurrX, GrCurrY to GrPixelNoX, GrCurrY
DrawHLine   
	LDX     <GrCurrX		; get starting co-ordinates
        PSHS    X			; save on stack
        JSR     >LAAB8			; get absolute value of GrPixelNoX - GrCurrX
        BCC     LA7DF			; branch if end > start
	
        LDX     <GrPixelNoX		; transfer end to start
        STX     <GrCurrX
	
LA7DF   TFR     D,Y			; save diffrence in Y
        LEAY    1,Y			; add 1 to difference, turn on starting and ending co-ordinates
        JSR     >CoToAbsAddr		; get abs screen pos in X, pixel mask in A
	
        PULS    U			; get start co-ordinates
        STU     <GrCurrX		; and reset them
        BSR     LA822			; point U at routine to move pixel pointers right
	
LA7EC   STA     <EvalD7			; save pxiel mask	
        JSR     >LA705			; turn on pixel
        LDA     <EvalD7			; restore pixel mask
        JSR     ,U			; move pixel pointer right
	
        LEAY    -1,Y			; decrement counter
        BNE     LA7EC			; keep going if more pixels to draw
        RTS				; return

; Draw a vertical line from GrCurrX, GrCurrY  to  GrCurrX, GrPixelNoY
; enter at 
LA7FA   PULS    D			; clean up stack
DrawVLine
	LDD     <GrCurrY		; get end co-ordinates
        PSHS    D			; save on stack
        JSR     >LAAAB			; calculate absolute value of GrPixelNoY-GrCurrY
        BCC     LA809			; switch co-ordinates if end < beginning
	
        LDX     <GrPixelNoY		; get end co-ordinates
        STX     <GrCurrY		; switch to beginning
LA809   TFR     D,Y			; transfer pixel count to Y
        LEAY    1,Y			; set both start and end co-ordinate?
        JSR     >CoToAbsAddr		; get absolute screen pos in X pixel mask in A 
        PULS    U			; get end co-ordinate
        STU     <GrCurrY		; restore them
        BSR     LA82B			; point u to routine to move down one row
        BRA     LA7EC			; go do it

; jump table of addresses of routines which will move the
; absolute screen address pointer one pixel to the right.
DA818   FDB     LA67B			; pmode 0
        FDB     LA682			; pmode 1
        FDB     LA67B			; pmode 2
        FDB     LA682			; pmode 3
        FDB     LA67B			; pmode 4

; point U to routine to move one pixel right
LA822   LDU     #DA818			; point at table
        LDB     <GrCurrPmode		; get current mode
        ASLB				; multiply by 2 to give table offset
        LDU     B,U			; get routine address in U
        RTS

; point U to routine to move down one row
LA82B   LDU     #LA677			; point to it
        RTS
; Draw a line from from GrCurrX, GrCurrY  to  GrPixelNoX, GrPixelNoY
LA82F   LDY     #IncGrCurrY		; point at routine to increment GrCurrY
        JSR     >LAAAB			; calculate vertical difference
        LBEQ    DrawHLine		; no vertical difference, draw horizontal line
        BCC     LA840			; branch ahead if Vend > Vstart
        LDY     #DecGrCurrY		; point at routine to decrement GrCurrY
	
LA840   PSHS    D			; save vertical difference
        LDU     #IncGrCurrX		; point to routine to increment GrCurrX
        JSR     >LAAB8			; calculate horizontal difference
        BEQ     LA7FA			; draw a vertical line if no difference
        BCC     LA84F			; branch if Hend > Hstart
        LDU     #DecGrCurrX		; point to routine to decrement GrCurrX
	
LA84F   CMPD    ,S			; compare X and Y differences
        PULS    X			; put Y difference in X
        BCC     LA85A			; branch if Y differnce > X difference
	
        EXG     U,Y			; swap X and Y increment/decrement routines
        EXG     D,X			; swap X and Y differences

LA85A   PSHS    D,U			; save the larger difference, and it's inc/dec address 
        PSHS    D			; save the bigger difference
        
	LSRA				; divide difference by 2, shift D right one bit
        RORB
        BCS     LA86B			; branch if odd number
	
        CMPU    #IncGrCurrY+1		; see if routine is increment or decrement
        BCS     LA86B			; branch if increment
        SUBD    #$0001			; subtract 1 if decrement
	
LA86B   PSHS    D,X			; save smallest difference in X, and initial minor co-ordinate
					; increment counter which is 1/2 of the largest difference
        JSR     >GetCoToAbsAddr		; point U at correct co-ordinate to screen routine
	
;* draw the line here - at this point the stack has the draw data on it
;
; 0 1,s=minor coordinate increment counter
; 2 3,s=assolute value of the smallest delta coordinate
; 4 5,s=absolute value of the largest delta coordinate
; 6 7,s=largest coordinate counter (how many times through the draw
;       loop. initially set to absolute value of largest delta coord
; 8 9,s=address of the routine which will increment or decrement
;       the largest delta coordinate	
;
LA870   JSR     ,U			; convert X,Y to absolute screen address & pixel mask
        JSR     >LA705			; turn on pixel
        LDX     6,S			; get distance counter
        BEQ     LA890			; branch if line completely drawn
	
        LEAX    -1,X			; decrement by 1
        STX     6,S			; resave it
        JSR     [$08,S]			; increment / decrement co-ordinate with largest difference
        LDD     ,S			; get the minor co-ordinate increment counter
        ADDD    2,S			; add the smallest difference
        STD     ,S			; save new minor co-ordinate increment counter
        SUBD    4,S			; subtract out the largest difference
        BCS     LA870			; branch if result not > largest difference
        STD     ,S			; if result >= then store new minor co-ordinate increment
        JSR     ,Y			; increment/decrement co-ordinate with smallest difference
        BRA     LA870			; loop again

LA890   PULS    X			; clean up stack and return
        PULS    D,X,Y,U,PC


; Note line drawing code above relies on the order of these 4 routines not changing

; Increment current X co-ordinate
IncGrCurrX   
	LDX     <GrCurrX		; get X
        LEAX    1,X			; increment it
        STX     <GrCurrX		; put it back
        RTS

; Increment current Y co-ordinate
IncGrCurrY
	LDX	<GrCurrY		; get Y
	LEAX	1,X			; increment it
	STX	<GrCurrY		; put it back
	RTS

; Decrement current X co-ordinate 
DecGrCurrX   
	LDX     <GrCurrX		; get X
        LEAX    -1,X			; decrement it
        STX     <GrCurrX		; put it back
	RTS

; Decrement current Y co-ordinate 
DecGrCurrY
	LDX	<GrCurrY		; get Y
	LEAX	-1,X			; decrement it
	STX	<GrCurrY		; put it back
	RTS
	
; get maximum value of hor/ver coordinates normalized for proper pmode.  
; return values : 
; Horizontal = EvalD3  
; Vertical = EvalD5
LA8B0   LDU     #EvalD3 		; point U at storage area
        LDX     #GrMaxX			; maximum X 
        STX     ,U			; save it
        LDX     #GrMaxY			; maximum Y
        STX     2,U			; save it
        JMP     >LA6AE			; go convert to proper mode based co-ordinates

; basic PCLS
CmdPCls:
LA8C0   BEQ     LA8D0			; clear to background colour if no argument
        BSR     LA8E8			; evaluate expression + convert to colour code
LA8C4   LDA     #$55			; considder each byte as 4 groups of 2 bits
        MUL				; multipl by colour
	
GrClearGrScreen:
LA8C7   LDX     <GrDisplayStartAddr	; point X at start of screen
LA8C9   STB     ,X+			; set byte to specified colour
        CMPX    <GrLastDisplayAddr	; end of graphics memory ?
        BNE     LA8C9			; no loop again
        RTS

LA8D0   LDB     <GrBackground		; get background colour
        BRA     LA8C4			; go clear it

; basic COLOUR
CmdColor:
LA8D4   CMPA    #','			; check for comma
        BEQ     LA8E0			; branch if foreground colour missing
        BSR     LA8E8			; evaluate first argument
        STB     <GrForeground		; set foreground colour
	
        JSR     <BasChrGetCurr		; get next character from basic
        BEQ     LA8E7			; no more, exit
	
LA8E0   JSR     >VarCKComma 		; syntax check for comma
        BSR     LA8E8			; evaluate last argument
        STB     <GrBackground		; set background colour
LA8E7   RTS

; evaluate an expression and convert it to a proper color code
; depending on the pmode and css; 
; illegal function call if > 8 otherwsie
; return color value in B css value in A
LA8E8   JSR     >VarGet8Bit 		; get 8 bit value from basic
LA8EB   CMPB    #GrMaxColour+1		; bigger than max colour?
        LBCC    BasFCError 		; yes, error
	
        CLRA				; VDG CSS value for first colour set
        CMPB    #$05			; first or second colour set?
        BCS     LA8FA			; branch if first
        LDA     #MaskCSS		; load CSS mask bit
        SUBB    #$04			; make colour value 0..1
	
LA8FA   PSHS    A			; save CSS mask
        LDA     <GrCurrPmode		; get current mode
        RORA				; get bit 0 into carry, 4 or 2 colour mode?
        BCC     LA909			; branch if 2 colour
	
        TSTB				; was colour = 0?
        BNE     LA906			; no
	
LA904   LDB     #$04			; if colour was 0 make it 4
LA906   DECB				; convert 1..4 to 0..3
LA907   PULS    A,PC			; restore  and return

LA909   RORB				; check only bit 0 if 2 colour
        BCS     LA904			; if odd fource colour to 3
        CLRB				; if even force colour to 0
        BRA     LA907

; set the current active color and all pixel byte to foreground/background color 
; depending on pset, preset if no expression , `)' or `,`.  
; otherwise evaluate the expression
BasGetColour   
	JSR     >GrSetColours 		; get the colour byte
        JSR     <BasChrGetCurr		; get the current input character
        BEQ     LA926			; branch if none
	
        CMPA    #')'			; close bracket?
        BEQ     LA926			; yes, no more arguments
	
        JSR     >VarCKComma 		; syntax check for comma
        CMPA    #','			; was next charcter a comma?
        BEQ     LA926			; yes
	
        JSR     >LA8E8			; evaluate expression, colour in B
        BSR     LA930			; temp store colour and pixel byte
LA926   JMP     <BasChrGetCurr		; check input character and return

GrSetColours:
LA928   LDB     <GrForeground		; get current foreground colour
        TST     <GrPlotFlag		; check pset/preset flag
        BNE     LA930			; branch if PSET
	
        LDB     <GrBackground		; get current background colour
LA930   STB     <GrColourTemp		; temp store colour
        LDA     #$55			; considder a byte as 4 2 bit pixels
        MUL				; set colour on all 4 pixels
        STB     <GrCurrColour		; save byte with all pixels turned on
        RTS

GrSelectDisplay:
LA938   BNE     LA95D			; branch if graphic mode, otherwise set alpha mode 

; this code will reset the display page register in the
; sam chip to 2 ($400) and reset the sam's vdg control
; register to 0 (alpha-numerics). in addition, it will
; reset the vdg control pins to alpha-graphics mode.
;
; set up the sam and vdg to graphics mode
TextResetVDU:
LA93A   PSHS    D,X			; save registers
        LDX     #SAMCF1			; point at SAM registers
        STA     10,X			; reset SAM page to $0400
        STA     8,X
        STA     6,X
        STA     4,X
        STA     2,X
        STA     1,X
        STA     -2,X
        STA     -4,X
        STA     -6,X			; Reset SAM to alpha numeric mode
        STA     -8,X
	
        LDA     PIA1DB			; get data from PIA1, port B
        ANDA    #MaskNONVideo		; reset all VDG bits
        STA     PIA1DB			; save back to PIA
        PULS    D,X,PC			; restore and return

; set graphics mode
LA95D   PSHS    D,X			; save registers
        LDA     <GrCurrPmode		; get current pmode
        ADDA    #$03			; add 3, mode is now 3..7, only 5 of 8 possible modes used
        LDB     #$10			; offset between pmodes
        MUL				; get pmode values for GM0, GM1, GM2 bits
        ORB     #MaskAG			; force AG bit high, graphics mode
        ORB     <GrColourSet		; or in CSS mask
        
	LDA     PIA1DB			; get current VDG bits from PIA
        ANDA    #MaskNONVideo		; keep only non video bits			
        PSHS    A			; save on stack
        ORB     ,S+			; or in new VDG bits
        STB     PIA1DB			; save back to PIA
	
        LDA     <GrDisplayStartAddr	; get MSB of graphic start page
        LSRA				; divide by 2, A now contains how many 512 blocks to start address
        JSR     >GrSetVDGOffset 	; set the SAM control register
	
        LDA     <GrCurrPmode		; get current pmode
        ADDA    #$03			; add bias for SAM register
        CMPA    #$07			; was it pmode 4?
        BNE     LA985			; no 
	
        DECA				; decrement A if pmode 4, same as pmode 3	
LA985   BSR     GrSetVDGMode 		; set the SAM's VDG register
        PULS    D,X,PC			; restore and return
	
; Enter with data to go in VDG register in bottom bits of A
GrSetVDGMode:
LA989   LDB     #$03			; 3 bits in SAM VDG control
        LDX     #SAMCV0			; point X at registers
LA98E   RORA				; get a bit into carry
        BCC     LA995			; branch if bit is 0
        STA     1,X			; set SAM bit to 1
        BRA     LA997			; move to next SAM bit

LA995   STA     ,X			; set SAM bit to 0
LA997   LEAX    2,X			; move pointer to next SAM bit
        DECB				; decrement bitcount
        BNE     LA98E			; loop if more
        RTS

GrSetVDGOffset:
LA99D   LDB     #$07			; 7 bits in VDG offset
        LDX     #SAMCF0			; point at SAM offset bits
        BRA     LA98E			; go set it

GrSelectVDGColSet:
LA9A4   LDA     PIA1DB			; get PIA1 side B
        ANDA    #~MaskCSS		; mask off CSS bit
        ORA     <GrColourSet		; or in current CSS bit
        STA     PIA1DB			; resave in PIA
        RTS

; basic PMODE
CmdPmode:
LA9AF   CMPA    #','			; check for comma, first argument may be missing
        BEQ     LA9DE			; yes comma, skip
	
        JSR     >VarGet8Bit 		; get first argument
        CMPB    #GrMaxPmode+1		; bigger than max pmode?
        BCC     LA9FB			; yep, illegal function call
	
        LDA     <GrStartPages		; get the start of graphics RAM
        STA     <GrDisplayStartAddr	; set start graphic page
        ASLB				; multiply mode by 2, table is 2 bytes / mode
        
	LDU     #PmodeTab+1		; point to pmode table
        ADDA    B,U			; add required number of 256 byte pages
        CMPA    <BasStartProg		; compare to base of basic prog
        BHI     LA9FB			; error, higher, not enough graphics RAM
	
        STA     <GrLastDisplayAddr	; update last display address
        LEAU    -1,U			; point at bytes / row entry
        LDA     B,U			; get it
        STA     <GrBytesPerLine		; update bytes per line
	
        LSRB				; restore pmode value
        STB     <GrCurrPmode		; save current pmode
        CLRA				; background colour
        STA     <GrBackground		; set background colour to zero
        LDA     #$03
        STA     <GrForeground		; set foreground colour to 3
        JSR     <BasChrGetCurr		; is there a starting page number?
        BEQ     LA9FA			; no, skip on
	
LA9DE   JSR     >VarGetComma8 		; get page number from basic
GrSelectPage:
LA9E1   TSTB				; page 0?
        BEQ     LA9FB			; yep invalid, pages are 1..8
        DECB				; make page zero based
        LDA     #$06			; each graphic page is 6x256 bytes (1.5K)
        MUL				; work it out
	
        ADDB    <GrStartPages		; add in start of graphics RAM
        PSHS    B			; save temp start addr
        ADDB    <GrLastDisplayAddr	; add current end address
        SUBB    <GrDisplayStartAddr	; sub out current start address (adds the size of one graphic page)
        CMPB    <BasStartProg		; start page+length of graphics ram > beginning of basic?
        BHI     LA9FB			; yep, FC error
	
        STB     <GrLastDisplayAddr	; update last graphic address
        PULS    B			; get temp start address
        STB     <GrDisplayStartAddr	; update graphic start address
LA9FA   RTS

LA9FB   JMP     >BasFCError 		; generate FC error

; basic SCREEN
CmdScreen:
LA9FE   CMPA    #','			; check for comma, first argument may be missing
        BEQ     LAA0D			; comma present, skip
	
        JSR     >VarGet8Bit 		; get alpha or graphics flag
        TSTB				; zero if alpha, nonzero if graphics
        JSR     >GrSelectDisplay 	; setup SAM and VDG for propper mode
	
        JSR     <BasChrGetCurr		; get next character
        BEQ     LA9FA			; none, exit
LAA0D   JSR     >VarGetComma8 		; get colour set 

GrSelectColourSet:
LAA10   TSTB				; check for colour set
        BEQ     LAA15			; if zero then css=0, else css=1
        LDB     #MaskCSS		; set CSS 1
LAA15   STB     <GrColourSet		; update it
        BRA     GrSelectVDGColSet 	; goa nad setup VDG

; basic PCLEAR
CmdPClear:
LAA19   JSR     >VarGet8Bit 		; evaluate expression and get no of pages
        TSTB				; test for zero pages
        BEQ     LA9FB			; FC error if zero
	
        CMPB    #GrMaxPages+1		; check less than maximum pages
        BCC     LA9FB			; more than max, FC error
	
GrReserveGrRam:
LAA23   LDA     #$06			; 6x256 bytes pages per graphics page
        MUL				; work out how many 256 pages to reserve
        ADDB    <GrStartPages		; add it to start of graphics RAM
        TFR     B,A			; move B to MSB of D
        LDB     #$01			; D now contains the top of pcleared space +1
	
        TFR     D,Y			; save in Y
        CMPD    <GrLastDisplayAddr	; compare to current end of graphics address
        LBCS    BasFCError 		; FC error if trying to clear less than current end of RAM
        
	SUBD    <BasStartProg		; subtract start of basic program
        ADDD    <BasVarSimpleAddr	; add end of basic program	
        TFR     D,X			; X=top of pcleared space +length of basic program
	
        ADDD    #$00C8			; add 200 bytes, default string stack
        SUBD    <AddrStack		; subtract top of cleared space
        BCC     LA9FB			; 'FC' error, no room left
        
	LDA     <BasCurrentLine		; get current basic line
	INCA				; will be $FFFF in direct mode, so $0000 after inc
        BEQ     LAA4F			; branch if direct mode
	
        TFR     Y,D			; get top of pcleared area
        SUBD    <BasStartProg		; subtract start of basic
        ADDD    <BasAddrSigByte		; add current input pointer
        STD     <BasAddrSigByte		; save new input pointer

LAA4F   LDU     <BasVarSimpleAddr	; get end of basic program
        STX     <BasVarSimpleAddr	; store new end of basic program
        CMPU    <BasVarSimpleAddr	; compare old end to new end
        BCC     LAA6F			; branch if old end > new end (moving down)
	
; move program to higher ram
LAA58   LDA     ,-U			; get a byte from program
        STA     ,-X			; save in new location
        CMPU    <BasStartProg		; moved all program yet?
        BNE     LAA58			; nope, keep going
	
        STY     <BasStartProg		; save new start address
        CLR     -1,Y			; clear byte just before program
LAA66   JSR     >BasVect2 		; re-link the program addresses
        JSR     >BasEraseVars		; clear the variables
        JMP     >BasRun 		; return to basic loop

;move program to lower ram
LAA6F   LDU     <BasStartProg		; get address of start of basic program
        STY     <BasStartProg		; store new start address
        CLR     -1,Y			; clear byte before program 
LAA76   LDA     ,U+			; get a byte from old location
        STA     ,Y+			; store it in new
        CMPY    <BasVarSimpleAddr	; reached end of program?
        BNE     LAA76			; nope, keep going
        BRA     LAA66			; go fixup lines, clear vars etc

LAA81   LDB     #BasStartPage		; setup basic program default start address $1e00
        STB     <BasStartProg
        LDA     #GrStartPage		; setup graphics default start address at $600
BasLocateScreen:
        STA     <GrStartPages		
        STA     <GrDisplayStartAddr
        CLRA				; default pmode 0
        STA     <GrCurrPmode
        LDA     #$10			; default bytes per line (in default mode)
        STA     <GrBytesPerLine
        LDA     #$03			; default foreground colour
        STA     <GrForeground
        LDA     #$0C			; default last display address $0c00
        STA     <GrLastDisplayAddr
        LDX     <BasStartProg		; clear byte before basic program
        CLR     -1,X
        JMP     >BasNew 		; go do a NEW, then re-enter basic loop

; Table of bytes per grapic row and 256 byte pages of memory required for each pmode
;		Byt, Pages		
PmodeTab
        FCB     $10,$06			; Pmode 0
        FCB     $20,$0C			; Pmode 1
        FCB     $10,$0C			; Pmode 2
        FCB     $20,$18			; Pmode 3
        FCB     $20,$18			; Pmode 4

; calculate absolute vertical last - first addresses
; CC.Carry set if result is -ve
LAAAB   LDD     <GrPixelNoY		; get 'last' Y co-ordinate
        SUBD    <GrCurrY		; subtract 'first' Y co-ordinate
LAAAF   BCC     LAAEC			; return if end >= start
        PSHS    CC			; save flags	
        JSR     >LB15E			; convert -ve to +ve
        PULS    CC,PC			; restore flags and return

; calculate absolute vertical last - first addresses
; CC.Carry set if result is -ve
LAAB8   LDD     <GrPixelNoX		; get 'last' X co-ordinate
        SUBD    <GrCurrX		; subtract 'first' X co-ordinate	
        BRA     LAAAF			; go process -ve 

; basic PCOPY
CmdPcopy:
LAABE   BSR     LAADA			; evaluate source page number and get MSB of address of start page in D
        PSHS    D			; save start page address
        LDB     #DTokTO			; is next character the 'TO' token?
        JSR     >VarCKChar 		; syntax check for it
	
        BSR     LAADA			; evaluate destination page number & get it's address in D
        PULS    X			; recover source page address int X
        TFR     D,U			; destination page address in U
        LDY     #(GrPageSize/2)		; transfer a page worth of bytes
	
LAAD1   LDD     ,X++			; get a word from source page
        STD     ,U++			; write it to destination page
        LEAY    -1,Y			; decrement count
        BNE     LAAD1			; loop if more to do
        RTS

LAADA   JSR     >VarGet8Bit 		; evaluate page number
        TSTB				; test for page 0
        BEQ     LAAED			; yep, FC error

; this is a flakey error check (CoCo and Dragon 32) - 
; it will let you pcopy over the top of the basic program in some instances.
; the Dragon 64 fixes this, by doing the check after calculating the actual
; address of the page to be copied  
	ifndef	Dragon64
        CMPB    <BasStartProg		; is page no > start address of basic program?
        BHI     LAAED			; yes, FC error if so (bad error check)
        endc
	
	DECB				; bump page number down one, make zero based
        LDA     #(GrPageSize/256)	; number of 256 byte pages per graphics page
        MUL				; work out offset within graphics memory
        ADDB    <GrStartPages		; add start of graphics memory
        
	ifdef	Dragon64
        CMPB    <BasStartProg		; is page no > start address of basic program?	
        BCC     LAAED			; yes, FC error if so 
	endc
	
	EXG     A,B			; now D has the address of the start of the page
LAAEC   RTS

LAAED   JMP     >BasFCError 		; generate an FC error

; basic GET command
CmdGet:
LAAF0   CLRB				; get flag for get
        BRA     LAAF5			; an FCB skip2 would save a byte here

; basic PUT command
CmdPut:
LAAF3   LDB     #$01			; flag for put 
LAAF5   STB     <EvalD8			; save get/put flag
        JSR     VectAccessScreen 	; call ram hook
	
        CMPA    #'@' 			; check for '@' sign
        BNE     LAB00			; no '@' found
	
        JSR     <BasChrGet		; get the character
LAB00   JSR     >LA71D			; go evaluate start and end points 
					; store start in GrCurrX,GrCurrY & end  in GrPixelNoX,GrPixelNoY
        JSR     >VarCKComma 		; syntax check for a comma
	
        JSR     >LAC67			; get pointer to array descriptor
        TFR     X,D			; save descriptor pointer in D
        LDU     ,X			; save offset to next array in U
        LEAU    -2,U			; point to end of array
        LEAU    D,U			; point U to end of array
        
	STU     <EvalD1			; save end of data (end of array)
        LEAX    2,X			; point X at number of dimensions
        LDB     ,X			; get number of dimenstions
        ASLB				; multiply by 2 as 2 bytes / dimension
        ABX				; point X at start of array data
        STX     <EvalCF			; save start of data
	
        LDA     <BasVarType		; check variable type
        BNE     LAAED			; FC error if not numeric
	
        CLR     <EvalD4			; get / put graphic action flag
        JSR     <BasChrGetCurr		; get current input char
        BEQ     LAB52			; branch if end of line
	
        COM     <EvalD4			; toggle get/put graphic/action flag
        JSR     >VarCKComma 		; syntax check for comma
	
        TST     <EvalD8			; check get/put flag
        BNE     LAB35			; branch if put
	
        LDB     #'G'			; check for full graphic option
        JSR     >VarCKChar 		; syntax check for 'G'
        BRA     LAB65			; skip around no G code

LAB35   LDB     #$05			; legal tokens at the end of PUT			
        LDX     #DABD4			; point X to lookup table
LAB3A   LDU     ,X++			; get clear bit action address
        LDY     ,X++			; set set bit action address
        CMPA    ,X+			; check to see if this token matches
        BEQ     LAB49			; yep matches
        DECB				; dec table counter
        BNE     LAB3A			; keep going if still tokens to check
        JMP     >BasSNError 		; invalid token, generate SN error

LAB49   STY     <EvalD5			; save set bit action address
        STU     <EvalD9			; save reset bit action address
        JSR     <BasChrGet		; get input char from basic
        BRA     LAB65			; skip around no G code

; no 'G' option or action specified on input line
LAB52   LDB     #$F8			; bottom 3 bits mask (8 pixels / byte)
        LDA     <GrCurrPmode		; get current graphics mode
        RORA				; bit 0 to carry
        BCC     LAB5B			; branch if mode 0, 2, 4 (2 colour modes)
	
        LDB     #$FC			; bottom 2 bits mask (4 pixels / byte)
LAB5B   TFR     B,A			; save mask in A
        
	ANDB    GrCurrX+1		; mask pixel counter bits off the horizontal
        STB     GrCurrX+1		; difference
        ANDA    <GrPixelNoX+1		
        STA     <GrPixelNoX+1
	
LAB65   JSR     >LAAB8			; calculate horizontal difference
        BCC     LAB6E			; branch if not end > start
	
        LDX     <GrPixelNoX		; make start = end if start > end
        STX     <GrCurrX
	
LAB6E   STD     <GrPixelNoX		; save horizontal difference
        JSR     >LAAAB			; calculate vertical difference
        BCC     LAB79			; branch if end > start
	
        LDX     <GrPixelNoY		; make start = end if start > end
        STX     <GrCurrY
	
LAB79   STD     <GrPixelNoY		; save vertical difference
        LDA     <GrCurrPmode		; get pmode
        RORA				; bit 0 to carry
        LDD     <GrPixelNoX		; get horizontal difference
        BCC     LAB86			; branch if mode 0, 2, 4 (2 colour modes)
        
	ADDD    <GrPixelNoX		; double horizontal difference, twice as many bytes for number of pixels
        STD     <GrPixelNoX		; in modes 1, 3
	
LAB86   JSR     >LA7AE			; normalize differences
        LDD     <GrPixelNoX		; get horizontal difference
        LDX     <GrPixelNoY		; get vertical difference
        LEAX    1,X			; add 1 to vertical difference
        STX     <GrPixelNoY		; save it
        TST     <EvalD4			; check for 'G' option or get action
        BNE     LABED			; and branch if given
	
        LSRA				; divide horizontal difference by 8
        RORB
        LSRA
        RORB
        LSRA
        RORB
	
        ADDD    #$0001			; add 1 to quotient
        STD     <GrPixelNoX		; save new horizontal difference
        JSR     >CoToAbsAddr		; get absolute screen address X and pixel mask A
	
LABA3   LDB     <GrPixelNoX+1		; get horizontal difference
        PSHS    X			; save screen position
LABA7   TST     <EvalD8			; test get/put flag
        BEQ     LABCC			; branch if get
        BSR     LABBE			; increment array data pointer
        LDA     ,U			; get data from array
        STA     ,X+			; put it on screen
LABB1   DECB				; decrement horizontal difference
        BNE     LABA7			; loop if more data on this line
        PULS    X			; get screen position back
	
        JSR     >LA677			; move absolute position down one row
        DEC     <GrPixelNoY+1		; decrement vertical difference
        BNE     LABA3			; loop again if more lines left
LABBD   RTS

LABBE   LDU     <EvalCF			; get array data pointer
        LEAU    1,U			; add one to it
        STU     <EvalCF			; update 
        CMPU    <EvalD1			; compare to end of data	
        BNE     LABBD			; no, continue
	
LABC9   JMP     >BasFCError 		; generate FC error

LABCC   LDA     ,X+			; get data from screen
        BSR     LABBE			; increment array data pointer
        STA     ,U			; store in array
        BRA     LABB1			; keep looping till done

; SET option token lookup table
; first word is reoutine to set a pixel
; second word is routine to reset a pixel
; last byte is token
DABD4   FDB	LAC2F,LAC36
	FCB     DTokPSET		; PSET
	        
        FDB     LAC36,LAC2F
	FCB	DTokPRESET		; PRESET
	
	FDB	LAC4C,LAC36
	FCB	DTokOR			; OR

	FDB	LAC2F,LAC4C
	FCB	DTokAND			; AND

	FDB	LAC3C,LAC3C
	FCB	DTokNOT			; NOT

; get / put with G option specified.
LABED   ADDD    #$0001			; add 1 to horizontal difference 
        STD     <GrPixelNoX		; and save it
        LDA     <EvalD8			; check get / put flag, branch if put
        BNE     LABFF			
	
; zero out entire GET array	
        LDU     <EvalD1			; get end of array
LABF8   STA     ,-U			; zero a byte		
        CMPU    <EvalCF			; reached beginning of array?
        BHI     LABF8			; nope keep looping
	
LABFF   JSR     >CoToAbsAddr		; get absolute address X and pixel mask A
        LDB     <GrCurrPmode		; get current pmode
        RORB				; bit 0 into carry
        BCC     LAC09			; branch if 2 colour mode (0,2,4)
	
        ANDA    #$AA			; use $AA as pixel mask in 4 colour mode
LAC09   LDB     #$01			; initialize shift counter
        LDY     <EvalCF			; point Y to array data
	
LAC0E   PSHS    A,X			; save pixel mask and screen address on stack
        LDU     <GrPixelNoX		; get the horizontal difference
LAC12   PSHS    A,U			; save pixel mask and horizontal difference

        LSRB				; shift bit counter right
        BCC     LAC1F			; branch if all 8 shifts done
        RORB				; shift carry back into b
        LEAY    1,Y			; increment array data pointer
        CMPY    <EvalD1			; compare pointer to end of array
        BEQ     LABC9			; yes : generate FC error
	
LAC1F   TST     <EvalD8			; check the get / put flag
        BEQ     LAC42			; branch if get	
        BITB    ,Y			; test a bit in array data
        BEQ     LAC2B			; branch if zero
        JMP     [EvalD5]		; jump to action routine for bit set 

LAC2B   JMP     [EvalD9]		; jump to action routine for bit reset

LAC2F	COMA				; mask off source data
	ANDA	,X			; off screen data
	STA	,X			; save on screen
	BRA	LAC4C			; skip ahead

LAC36	ORA	,X			; or source data with screen
	STA	,X			; save to screen
	BRA	LAC4C			; skip ahead

LAC3C	EORA	,X			; invert (not) pixel
	STA	,X			; save to screen
	BRA	LAC4C			; skip ahead
	
LAC42   BITA    ,X			; test the pixel
        BEQ     LAC4C			; branch if it is off
	
        TFR     B,A			; put shift counter back in A
        ORA     ,Y			; turn on the propper bit in the array data
        STA     ,Y
LAC4C   PULS    A,U			; restore pixel mask and horizontal difference
        JSR     >LA67B			; move screen pos and pixel mask one to the right, 2 colour mode
        
	LEAU    -1,U			; decrement horizontal difference
        CMPU    <DBZero	; is difference zero?
        BNE     LAC12			; nope loop again
        
	LDX     1,S			; get screen pos from stack
        LDA     <GrBytesPerLine		; get number of bytes per graphics line
        LEAX    A,X			; add to screen pointer
        PULS    A			; restore pixel mask
        LEAS    2,S			; drop saved X 
        DEC     <GrPixelNoY+1		; decrement vertical row count
        BNE     LAC0E			; loop for next line if not zero
        RTS

LAC67   JSR     >VarGetVar		; evaluate expression get descriptor pointer in X
        LDB     ,-X			; strip off variable name (2 characters)
        LDA     ,-X
        TFR     D,U			; save them in U
        LDX     <BasVarArrayAddr	; get the start of the arrays	

LAC72   CMPX    <BasVarEnd		; reached end of arrays?	
        LBEQ    BasFCError 		; yep, generate FC error
        CMPU    ,X			; compare target name to array name
        BEQ     LAC83			; same, so we've found it, return it
        
	LDD     2,X			; get offset to next array
        LEAX    D,X			; add to pointer
        BRA     LAC72			; look at next array descriptor

LAC83   LEAX    2,X			; move pointer to offset to next array
        RTS				; wasted byte

LAC86   RTS

; basic PAINT command

; paint up / down flags
PaintDone	equ	$00
PaintUP		equ	$01
PaintDown	equ	$FF
CmdPaint:
LAC87   CMPA    #'@'			; check for @ sign
        BNE     LAC8D			; skip if not
        JSR     <BasChrGet		; get next character
	
LAC8D   JSR     >BasGetPixel		; get start co-ordinates and save in GrCurrX, GrCurrY
        JSR     >NormalizeXY		; normalize co-ordinates
	
        LDA     #$01			; pset value
        STA     <GrPlotFlag		; set pset/preset flag to pset
        JSR     >BasGetColour		; get colour (for fill) from basic colour and all pixel bytes
        LDD     <GrColourTemp		; save them on stack
        PSHS    D			
	
        JSR     <BasChrGetCurr		; get current charcter from basic
        BEQ     LACA5			; none skip on
        JSR     >BasGetColour		; get edge colour / all pixel bytes
LACA5   LDA     <GrCurrColour		; get edge colour
        STA     <EvalD8			; save in EvalD8
        PULS    D			; restore foreground colour
        STD     <GrColourTemp
       
; store a block of paint data on stack which will act as an end of paint data flag.
; the CLRA will cause the up/down flag to be zero which is used to exit the paint routine.
 
	CLRA				
        PSHS    D,X,U

        JSR     >LA8B0			; get normalized max X & Y values, result in EvalD3
        JSR     >GetCoToAbsAddr		; point U to routine to select a pixel
	
; paint the first horizontal line from start co-ordinates	
        STU     <EvalD9			; save routine address
        JSR     >LAD7A			; paint from current X co-ordinate towards zero
        BEQ     LACCC			; branch if no painting done, hit border immediately
	
        JSR     >LAD66			; paint towards max X co-ordinate
        
	LDA     #PaintUP		; up/down flag = up
        STA     <EvalD7			; save it
        JSR     >LAD55			; save -ve going line on stack
        
	NEG     <EvalD7			; up/down flag = down
        JSR     >LAD55			; save +ve going line on stack

LACCC   STS     <EvalDC			; temp store stack pointer
LACCF   TST     <GrDirtyFlag		; see if painted colour different from original colour
        BNE     LACD6			; branch if data has been modified
	
        LDS     <EvalDC			; restore stack pointer
LACD6   PULS    D,X,U			; get data for next line segment to check

        CLR     <GrDirtyFlag		; clear change flag
        STS     <EvalDC			; temp save stack pointer
        
	LEAX    1,X			; add 1 to start X co-ordinate
        STX     <GrCurrX		; save it
        STU     <EvalD1			; length of parent line
        STA     <EvalD7			; up / down flag
        BEQ     LAC86			; exit if up / down flag = 0
        BMI     LACEF			; branch if up / down flag = down
	
; check line below current data	
        INCB				; increment Y co-ordinate
        CMPB    <EvalD6			; compare to maximum Y
        BLS     LACF3			; branch if not greater, process the line
	
        CLRB				; set Y co-ordinate to 0 to force wrap arround
LACEF   TSTB				; test Y co-ordinate
        BEQ     LACCF			; processs another block of paint data if wrapped around
					; discard any line below Y = 0 or Y = maxY
        DECB				; decrement Y co-ordinate

; process a horizontal line that was stored on stack, limit checks have been done	
LACF3   STB     <GrCurrY+1		; save current Y co-ordinate
        JSR     >LAD7A			; paint from current X to zero or border
        BEQ     LAD09			; if number of painted pixels = 0, complement length

; see if < 3 pixels where painted.  If fewer than 3 pixels painted, then there is no need to check for 
; for more data to be painted on the line to the left of the current position than the direction that the
; up/down flag is currently set to.
        CMPD    #$0003			
        BCS     LAD04			; branch if no need to check for more paintable data
        LEAX    -2,X			; move the X co-ordinate 2 pixels to the left
        BSR     LAD3C			; save a block of paint data in the oposite direction  to the up / down flag
	
LAD04   JSR     >LAD66			; continue painting line to the right
LAD07   BSR     LAD55			; save a block of paint data in the same direction to the up / down flag

; this code will insure that the current line is examined to the right for 'paintable' pixels for
; a length equal to the length of the 'parent' line
LAD09   COMA				; complement the length of the line just painted
        COMB
LAD0B   ADDD    <EvalD1			; add 1 to length of parent line
        STD     <EvalD1			; save difference of line just painted & parent line
        BLE     LAD27			; branch if parent is shorter
	
        JSR     >IncGrCurrX		; increment X co-ordinate
        JSR     >LADAD			; check for border colour
        BNE     LAD1E			; branch if not
	
        LDD     #-1			; go decrement length of difference
        BRA     LAD0B			; and keep looking for border colour

LAD1E   JSR     >DecGrCurrX		; decrement X co-ordinate
        BSR     LAD61			; go get current X co-ordinate
        BSR     LAD83			; paint forward to max X or border
        BRA     LAD07			; save a block of paint data and keep checking

; check to see if the current line extends further to the right than the parent line and 
; put a block of paint data on the stack if it is more than 2 pixels past the end of the parent line
LAD27   JSR     >IncGrCurrX		; increment current X co-ordinate
        LEAX    D,X			; point X at right hand end of parent line
        STX     <GrCurrX		; and save it as current X co-ordinate
	
        COMA				; D now contains a -ve number corrisponding to the
        COMB				; number of pixels the current line extends past the 
        SUBD    #$0001			; right hand end of the parent. Convert to +ve
        BLE     LAD39			; branch if line doesn't extend
	
        TFR     D,X			; save the position of the line to the right of the parent as the length
        BSR     LAD3C			;  save a block of paint data in the oposite direction to the up/down flag
LAD39   JMP     >LACCF			; process more paint data

; Blocks of paint data are stored on the stack so that paint can `remember' where it should go back to paint 
; up or down from the current line it is painting. These blocks of data represent horizontal lines above or 
; below the current line being painted and require six bytes of storage on the stack.
; The data are stored as follows: 
;   0,s=up/down flag 
;   1,s=ver coord of line
; 2 3,s=leftmost hor coord of line
; 4 5,s=length of line
;
; save a block of `paint' data for a line in the opposite direction of the current up/dn flag
LAD3C   STD     <GrCircleXCo		; save number of pixels painted
        PULS    Y			; put return address in Y
        LDD     <GrCurrX		; get current start co-ordinate
        PSHS    D,X			; put on stack
        LDA     <EvalD7			; get up/down flag 	
        NEGA				; reverse it
	
LAD47   LDB     <GrCurrY+1		; get Y co-ordinate
        PSHS    D			; save Y co-ordinate and up / down flag
        PSHS    Y			; save return address
	
; code below checks for ability to store four bytes in free ram, 
; however the paint routine will store six bytes in free ram.
; first instruction should be ldb #3
        LDB     #$02			; check to see if there's enough RAM for 4 bytes of storage
        JSR     >BasChkB2Free 		 
        LDD     <GrCircleXCo		; get length of right painted line
        RTS

; save a block of `paint' data for a line in the same direction as the current up/dn flag 
LAD55   STD     <GrCircleXCo		; set length of right horizontal painted line
        PULS    Y			; save return address in Y
        LDD     <GrPixelNoX		; start X co-ordinate
        PSHS    D,X			; save X co-orinate and length
        LDA     <EvalD7			; get up /down flag, 1/-1
        BRA     LAD47			; save paint data on stack

LAD61   LDX     <GrCurrX		; get current X co-ordinate
        STX     <GrPixelNoX		; save it
        RTS

; Go here to finish painting right after you have painted left
LAD66   STD     <GrCircleYCo		; save count of number of pixels painted
        LDY     <GrPixelNoX		; get last X co-ordinate
        BSR     LAD61			; save current X co-ordinate, now contains co-ordinate of 
					; left border of the horizontal line
        STY     <GrCurrX		; start painting to the right from the left border co-ordinate
        BSR     LAD83			; paint towards right
	
        LDX     <GrCircleYCo		; get number of pixels painted when moving left
        LEAX    D,X			; add the number painted towards the right 
        ADDD    #$0001			; add 1 to total count of pixels
        RTS

; paint from X co-ordinate to zero or hit border return with z = 1 if no painting done
LAD7A   JSR     >LAD61			; paint starting at GrCurrX
        LDY     #DecGrCurrX		; get pointer to decrement X routine
        BRA     LAD89			; go do it

; paint from x co-ordinate to max hor coord or hit border-return z=1 if no painting done
LAD83   LDY     #IncGrCurrX		; point to increment X routine
        JSR     ,Y			; call icrement/decrement X
	
LAD89   LDU     <DBZero	; U=0, initial pixel counter
        LDX     <GrCurrX		; get current X co-ordinate
	
LAD8D   BMI     LADA6			; branch if X < 0 or X > 127
        
	CMPX    <EvalD3			; compare X co-ordinate to max value
        BHI     LADA6			; branch if X > max
	
        PSHS    Y,U			; save paint counter, inc/dec counter
        BSR     LADAD			; check for border pixel
        BEQ     LADA4			; branch if boarder hit
	
        JSR     >LA705			; set pixel to paint colour, painting is done here
        PULS    Y,U			; restore paint counter, inc/dec counter
	
        LEAU    1,U			; add 1 to paint counter
        JSR     ,Y			; call increment or decrement routine
        BRA     LAD8D			; loop again to keep painting the line

LADA4   PULS    Y,U			; restore paint counter, inc/dec counter
LADA6   TFR     U,D			; save paint counter in D
        TFR     D,X			; save paint counter in X
        SUBD    <DBZero	; D-D-0, set flags according to paint counter 			
        RTS

; Check for border color 
; Enter with EvalD9 containing  address of routine to get abs screen address and pixel mask 
; Exit with z = 1 if hit border color pixel       
LADAD   JSR     [EvalD9]		; call routine to get address & mask
        TFR     A,B			; copy pixel mask to B
        ANDB    <EvalD8			; AND pixel mask with border colour, B=1 pixel of bordeer colour
        PSHS    D			; push pixel mask and border pixel
        ANDA    ,X			; put current pixel data into B
        CMPA    1,S			; compare to border colour, Z=1 if so
        PULS    D,PC			; restore and return

; Basic PLAY
CmdPlay:
LADBD   LDX     <DBZero	; X=0, default values for length of play and address	
        LDB     #$01			; of start of play string
        PSHS    B,X			; save them on stack
        
	JSR     >VarGetStr 		; get a string from basic
        CLRB
        JSR     >LBAF1			; set DtoA to pas through analog multiplexor
        JSR     >SndEnable 		; enable sound
	
LADCD   JSR     >BasGetStrLenAddr 	; get address X and length B of play string
        BRA     LADD4			; skip on, could save a byte using FCB skip2

LADD2   PULS    B,X			; restore length and addreess of play string

LADD4   STB     <EvalD8			; save length in EvalD8
        BEQ     LADD2			; get new string data if length = 2
	
        STX     <EvalD9			; save start of play string
        LBEQ    SndDisable 		; disable sound, and return if X=0
	
LADDE   TST     <EvalD8			; see if length of string = 0
        BEQ     LADD2			; yes : get new data
	
        JSR     >LAF33			; get a command (from string)
        CMPA    #';'			; sub command terminated
        BEQ     LADDE			; ignore semicolons
	
        CMPA    #$27			; check for single quote
        BEQ     LADDE			; ignore them too....
	
        CMPA    #'X'			; check for an executable sub-string
        LBEQ    LAFA5			; go process sub command
	
        BSR     LADF7			; check for other commands
        BRA     LADDE			; loop again for next

; adjust octave
LADF7   CMPA    #'O'			; adjust octave?
        BNE     LAE08			; no 
	
        LDB     <SndOctave		; get current octave value	
        INCB				; internally 0..4, basic uses 1..5
        BSR     LAE5B			; modifier check
        DECB				; compensate for incb above (make zero based again)
        CMPB    #$04			; maximum value of 4
        BHI     LAE68			; FC error if too high
        STB     <SndOctave		; update octave
        RTS				; return

; adjust volume
LAE08   CMPA    #'V'			; adjust volume?
        BNE     LAE26			; no
	
        LDB     <SndVolume		; get current volume
        LSRB				; shift 2 bits right as D/A is bits 2..7
        LSRB
        SUBB    #31			; subtract out mid value offset
        BSR     LAE5B			; check for modification
        CMPB    #31			; maximum allowed is 31
        BHI     LAE68			; generate FC error if higher
	
        ASLB				; shift bact to correct place for D/A
        ASLB
        PSHS    B			; save new volume on stack
	
	ifdef	Dragon64
        LDD     #$7E7C			; put mid value in high and low limit (Dragon 64)
	else
        LDD     #$7E7E			; put mid value in high and low limit (Dragon 32)
	endc
	
        ADDA    ,S			; add new volume to high limit
        SUBB    ,S+			; subtract new volume from low limit
        STD     <SndVolume		; set new volume limits
        RTS

; adjust note length
LAE26   CMPA    #'L'			; adjust note length?
        BNE     LAE4D			; no
	
        LDB     <SndNoteLen		; get current note length
        BSR     LAE5B			; check for adjustment
        TSTB				; check for length = 0
        BEQ     LAE68			; FC error if so
	
        STB     <SndNoteLen		; update note length
        CLR     <SndDotNoteScale	; clear dotted note scale factor
LAE35   BSR     LAE3A			; check for dotted note
        BCC     LAE35			; branch if dotted note
        RTS

; scale factor for dotted note
LAE3A   TST     <EvalD8			; check command length
        BEQ     LAE48			; branch if empty
	
        JSR     >LAF33			; get command character
        CMPA    #'.'			; check for dotted note
        BEQ     LAE4A			; branch on dotted note and clear carry
 
        JSR     >LAF7D			; move command string pointer back one, and add 1 to command length
LAE48   COMA				; set carry flag
        RTS

LAE4A   INC     <SndDotNoteScale	; add 1 to timer scale factor	
        RTS

; adjust tempo
LAE4D   CMPA    #'T'			; modify tempo?
        BNE     LAE5E			; nope
	
        LDB     <SndTempo		; get tempo
        BSR     LAE5B			; check for adjuustment
        TSTB				; is tempo zero?
        BEQ     LAE68			; FC error if so
	
        STB     <SndTempo		; update tempo
        RTS

LAE5B   JMP     >LAF47			; evaluate >,<,+,-,= operators

; pause
LAE5E   CMPA    #'P'			; pause command?
        BNE     LAE86			; nope
	
        JSR     >LB066			; evaluate decimal string value
        TSTB				; check for legal expression and branch if pause > 0
        BNE     LAE6B			; FC error if pause=0
LAE68   JMP     >BasFCError 

LAE6B   LDA     <SndDotNoteScale	; save current value of volume & nots scale	
        LDX     <SndVolume
        PSHS    A,X			; save on stack
	
	ifdef	Dragon64
	LDA	#$7C			; mid value of D/A (Dragon 64)
	else
        LDA     #$7E			; mid value of D/A (Dragon 32)
	endc
	
        STA     <SndVolume		; set volume = 0
        STA     <SndVolume+1
        CLR     <SndDotNoteScale	; reset dot note scale	
        BSR     LAE82			; go play a note of volume 0
	
        PULS    A,X			; restore volume and note scale from stack
        STA     <SndDotNoteScale	; restore their values
        STX     <SndVolume
        RTS

LAE82   CLR     ,-S			; push note 0 onto stack
        BRA     LAEC6			; go play it

; process a note
LAE86   CMPA    #'N'			; 'N' before note?
        BNE     LAE8D			; no : skip, it's optional
        JSR     >LAF33			; get next command character
	
LAE8D   CMPA    #'A'			; check for note 'A'
        BCS     LAE95			; branch if below
        CMPA    #'G'			; check for note 'G'
        BLS     SndPlayNote 		; note is in range 'A'..'G', go play it
	
LAE95   JSR     >LAF59			; evaluate decimal veluse in command string
        BRA     LAEBD			; go play it

; process a note here
SndPlayNote:
LAE9A   SUBA    #'A'			; make note value zero based 0..7
        LDX     #NoteNumberTab		; point X at note number table
        LDB     A,X			; get note number from table
	
        TST     <EvalD8			; any command characters left?
        BEQ     LAEBD			; no, skip ahead
	
        JSR     >LAF33			; get command character
        CMPA    #'#'			; sharp note?		
        BEQ     LAEB0			; yes
	
        CMPA    #'+'			; sharp note?
        BNE     LAEB3			; no
	
LAEB0   INCB				; add one to note number: sharp
        BRA     LAEBD			; process note

LAEB3   CMPA    #'-'			; flat note?
        BNE     LAEBA			; no
        
	DECB				; subtract 1 from note number
        BRA     LAEBD			; go process note

LAEBA   JSR     >LAF7D			; move command string pointer back one, and add 1 to length

LAEBD   DECB				; adjust note number, make zero based
        CMPB    #12-1			; maximum note number?
        BHI     LAE68			; FC error if invalid
	
        PSHS    B			; save note value
        LDB     <SndNoteLen		; get note length
LAEC6   LDA     <SndTempo		; get sound tempo
        MUL				; calculate duration
        STD     <EvalD5			; save duratiion

; the irq interrupt is used to provide a master timing reference for the `play' command.  
; when a note is done, the irq servicing routine will return control to the main `play' 
; command interpretation loop	
        LEAU    1,S			; get value of stack pointer +1, so we can reset 
					; when the IRQ vectors you out of the play routines (below)
        LDA     <SndOctave		; get current octave
        CMPA    #$01			; is it > 1
        BHI     LAEFF			; branch if so

; octaves 1 and 2 use a 2 byte delay tab to set the correct frequency.	
        LDX     #Octave1DelayTab	; point to table
        LDB     #2*12			; tables 12 words long for octave 1 & 2
        MUL				; calculate offset
        ABX				; point X at correct octave entry
	
        PULS    B			; get note value back
        ASLB				; multiply by 2, as 2 bytes / note
        ABX				; point to correct note
	
        LEAY    ,X			; get pointer to Y
        BSR     LAF27			; calculate note timer value
        STD     <SndTimerPlay		; save it
	
; main sound generation loop - only the irq service will get you out of this loop (octaves 1 and 2)	
LAEE4   BSR     LAEF2			; mid value to D/A and wait
        LDA     <SndVolume		; get high volume value
        BSR     LAEF5			; store to D/A and wait
        BSR     LAEF2			; high value to D/A and wait
        LDA     <SndVolume+1		; get low value
        BSR     LAEF5			; store to D/A and wait
        BRA     LAEE4			; loop again

LAEF2 
	ifdef	Dragon64
	LDA	#$7C			; D/A mid value + printer strobe (Dragon 64)
	else
	LDA     #$7E			; D/A mid value + printer strobe (Dragon 32)
	endc
	
        NOP				; small delay
LAEF5   STA     PIA1DA			; write to PIA
        LDX     ,Y			; get delay from octave table
LAEFA   LEAX    -1,X			; count delay down to zero	
        BNE     LAEFA			; keep goint until 0
        RTS

; handle notes in octaves 3-5
LAEFF   LDX     #Octave3DelayTab-(2*12)	; point before table as table starts at octave 3, 12bytes/ octave
        LDB     #$0C			; 12 bytes per ocatve, 1 byte per note 
        MUL				; calculate offset
        ABX				; point at correct octave table
        PULS    B			; restore note value
        ABX				; adjust pointer to correct note
        
	BSR     LAF27			; calculate note timer value
        STD     <SndTimerPlay		; save it
	
LAF0D   BSR     LAF1B			; mid value to D/A and wait
        LDA     <SndVolume		; get high volume
        BSR     LAF1E			; store to D/A and wait
        BSR     LAF1B			; mid value to D/A and wait
        LDA     <SndVolume+1		; get low volume
        BSR     LAF1E			; sent to D/A and wait
        BRA     LAF0D			; loop again

; put mid value to da converter and wait a while
LAF1B   
	ifdef	Dragon64
	LDA	#$7C			; D/A mid value + printer strobe (Dragon 64)
	else
	LDA     #$7E			; D/A mid value + printer strobe (Dragon 32)
	endc

        NOP
LAF1E   STA     PIA1DA			; write to PIA
        LDA     ,X			; get delay value from octave table
LAF23   DECA				; decrement it
        BNE     LAF23			; keep looping till zero
        RTS

 ; calculate note timer value 
 ; return with value in D the larger D is, the longer the note will play
LAF27   LDB     #$FF			; note time base value	
        LDA     <SndDotNoteScale	; get note timer scale factor
        BEQ     LAF32			; use default if 0
        ADDA    #$02			; add constant timer scale factor	
        MUL				; multiply scale factor by base
        LSRA				; divide D by 2, each increment in 
        RORB				; SndDotNoteScale will increse delay by 128
LAF32   RTS

;get next command - return value in A
LAF33   PSHS    X			; save X	
LAF35   TST     <EvalD8			; check command counter
        BEQ     LAF86			; FC error if no data left
	
        LDX     <EvalD9			; get command pointer address
        LDA     ,X+			; get next command byte
        STX     <EvalD9			; update saved command pointer
        DEC     <EvalD8			; decrement command counter
        CMPA    #' '			; check for space
        BEQ     LAF35			; ignore spaces, so get next
        PULS    X,PC			; restore and return

; evaluate the >,<,+,-,= operators 
; enter with the value to be operated on in accb, return new value in same
LAF47   BSR     LAF33			; get command character
        CMPA    #'+'			; add one ?
        BEQ     LAF89			; yep
	
        CMPA    #'-'			; subtract 1
        BEQ     LAF8D			; yep
	
        CMPA    #'>'			; multiply by 2
        BEQ     LAF97			; yep
	
        CMPA    #'<'			; divide by 2
        BEQ     LAF92			; yep
	
LAF59   CMPA    #'='			; check for variable equate, branch if so
        BEQ     LAF9C			; B will be set to the value of the variable in
					; the command string.
					; The variable must be numeric, less than 256 and
					; followed by a semicolon
        JSR     >LA438			; clear carry if numeric
        BCS     LAF86			; FC error if no numeric

* strip a decimal ascii value off of the command string and return binary value in B
        CLRB				; units digits=0
LAF63   SUBA    #'0'			; convert ASCII digit to binary	
        STA     <EvalD7			; save it
        LDA     #$0A			; multiply by 10
        MUL
        TSTA				; test result
        BNE     LAF86			; FC error if > 255
        
	ADDB    <EvalD7			; get temporary value
        BCS     LAF86			; FC error if > 255
	
        TST     <EvalD8			
        BEQ     LAF8C			; return if no commands left
	
        JSR     >LAF33			; get another command character
        JSR     >LA438			; clear carry if numeric
        BCC     LAF63			; loop again if numeric
	
LAF7D   INC     <EvalD8			; add 1 to command counter
        LDX     <EvalD9			; move command string pointer
        LEAX    -1,X			; back one
        STX     <EvalD9			; resave it
        RTS

LAF86   JMP     >BasFCError 		; generate FC error

LAF89   INCB				; add one to parameter 
        BEQ     LAF86			; FC error if adding 1 to 255
LAF8C   RTS

LAF8D   TSTB				; test B
        BEQ     LAF86			; FC error if trying to decrement 0
        DECB				; decrement 1 from parameter
        RTS

LAF92   TSTB				; test b
        BEQ     LAF86			; FC error if trying to divide zero
        LSRB				; devide B by 2
        RTS

LAF97   TSTB				; test b
        BMI     LAF86			; FC error f result would be > 255
        ASLB				; multiply by 2
        RTS

LAF9C   PSHS    Y,U			; save Y,U
        BSR     LAFB6			; interpret command string as if it where basic variable
        JSR     >FPA0toB		; convert FPA0 to integer in B
        PULS    Y,U,PC			; restore and return

LAFA5   JSR     >LAFB6			; evaluate expression in the command string
        LDB     #$02			; check for room for 4 bytes
        JSR     >BasChkB2Free 		; go check
	
        LDB     <EvalD8			; get current command length 
        LDX     <EvalD9			; and pointer
        PSHS    B,X			; save them on stack
        JMP     >LADCD			; go interpret the new play sub command

; interpret the present command string as if it were a basic variable
LAFB6   LDX     <EvalD9			; get command pointer
        PSHS    X			; save it
        JSR     >LAF33			; get a command character
        JSR     >CheckAAlpha		; check it's alphabetic
        BCS     LAF86			; FC error if not alphabetic
	
LAFC2   JSR     >LAF33			; get a command character
        CMPA    #';'			; check for semicolon command seperator
        BNE     LAFC2			; nope, keep looking
	
        PULS    X			; get saved command pointer
        LDU     <BasAddrSigByte		; get basic's input pointer
        PSHS    U			; save it
        STX     <BasAddrSigByte		; set basic's input pointer to command pointer
        
	JSR     >L89C1			; evaluate alpha expression, and get new string
        
	PULS    X			; restore basic's input pointer	
        STX     <BasAddrSigByte
        RTS

; Sound interrupt routine, called once per frame sync
SoundINT   
	CLRA
        TFR     A,DP			; DP=0
        LDD     <SndTimerPlay		; Get play timer
        LBEQ    LBB02			; done : continue
        
	SUBD    <EvalD5			; Subtract decrement
        STD     <SndTimerPlay		; re-save timer
        BHI     LAFF5			; branch if still playing
        
	CLR     <SndTimerPlay		; reset timer to 0
        CLR     <SndTimerPlay+1
        PULS    A			; recover saved CC
        LDS     7,S			; set S saved U from stack
        ANDA    #$7F			; clear entire flag, pretend we are an FIRQ
        PSHS    A				
LAFF5   RTI				; return


; Numerical values for notes A, B, C, D, E, F, G
NoteNumberTab   
	FCB     $0A,$0C,$01,$03,$05,$06,$08
	
; Delays for octave 1 notes
; 2 bytes per note for octaves 1 and 2
Octave1DelayTab   
	FDB     $01A8,$0190,$017A,$0164,$0150,$013D
        FDB     $012B,$011A,$010A,$00FB,$00ED,$00DF
	
Octave2DelayTab   
	FDB     $00D3,$00C7,$00BB,$00B1,$00A6,$009D
        FDB     $0094,$008B,$0083,$007C,$0075,$006E

; 1 byte per note for octaves 3, 4, 5	
Octave3DelayTab        
	FCB     $A6,$9C,$93,$8B,$83,$7B
        FCB     $74,$6D,$67,$61,$5B,$56
        FCB     $51,$4C,$47,$43,$3F,$3B
        FCB     $37,$34,$31,$2E,$2B,$28
        FCB     $26,$23,$21,$1F,$1D,$1B
        FCB     $19,$18,$16,$14,$13,$12

; basic DRAW command.
GrDraw:
LB051   LDX     <DBZero	; X=1		
        LDB     #$01			; B=1, End of draw command line values
        
	PSHS    B,X			; save on stack
        STB     <GrPlotFlag		; set PSET/PRESET flag to PSET
        STX     <EvalD5			; clear update & draw flag	
        JSR     >GrSetColours 		; set active colour byte
        JSR     >VarGetStr 		; evaluate expression, FC if not string
	
LB061   JSR     >BasGetStrLenAddr 	; get length and adress of command
        BRA     LB06E			; interpret command string

LB066   JSR     >LAF33			; get next value from command line
        JMP     >LAF59			; evaluate a decimal in command line

LB06C   PULS    B,X			; get command line to be interpreted from stack

LB06E   STB     <EvalD8			; set command length counter
        BEQ     LB06C			; get new command line if 0, end of current command line
	
        STX     <EvalD9			; save command line pointer
        LBEQ    LB162			; command pointer = 0, exit draw			
		
LB078   TST     <EvalD8			; test command length
        BEQ     LB06C			; if none left, get next command line
	
        JSR     >LAF33			; get a command character
        CMPA    #';'			; semicolon?
        BEQ     LB078			; yes, skip it
	
        CMPA    #$27			; single quote?
        BEQ     LB078			; yes skip it
	
        CMPA    #'N'			; update check?
        BNE     LB08F			; no, skip
	
        COM     <EvalD5			; toggle update flag, 0 = update, $FF = don't update
        BRA     LB078			; skip on

LB08F   CMPA    #'B'			; check draw flag
        BNE     LB097			; no, skip
	
        COM     <EvalD6			; toggle draw flag, 0 = draw, $FF = don't draw
        BRA     LB078

LB097   CMPA    #'X'			; substring?
        LBEQ    LB133			; yes go handle it
	
        CMPA    #'M'			; move draw position?
        LBEQ    LB1CD			; yes, go move it
	
        PSHS    A			; save current command
        LDB     #$01			; default value if no number follows command
        TST     <EvalD8			; check command length 
        BEQ     LB0BC			; branch if 0, no commands left
	
        JSR     >LAF33			; get a command char
        JSR     >CheckAAlpha		; set carry if not alphabetic
	
        PSHS    CC			; save flags
        JSR     >LAF7D			; move command pointer back one
        PULS    CC			; restore flags
        BCC     LB0BC			; carry clear, so alphabetic
        BSR     LB066			; evaluate a decimal command line value, return in B
	
LB0BC   PULS    A			; get current command back
        CMPA    #'C'			; change colour?
        BEQ     LB0EA			; yep, do it
	
        CMPA    #'A'			; change angle?
        BEQ     LB0F4			; yep, do it
	
        CMPA    #'S'			; change scale?
        BEQ     LB0FC			; yep, do it
	
        CMPA    #'U'			; go up?
        BEQ     LB12A			; yep, do it
	
        CMPA    #'D'			; go down?
        BEQ     LB127			; yep, do it
	
        CMPA    #'L'			; go left?
        BEQ     LB122			; yep, do it
	
        CMPA    #'R'			; go right?
        BEQ     LB11D			; yep, do it
	
        SUBA    #'E'			; convert to binary for 'E'..'H' command checks
        
	BEQ     LB10D			; do 'E', 45 degrees
        DECA
	
        BEQ     LB108			; do 'F', 135 degrees
        DECA
	
        BEQ     LB116			; do 'G', 225 degrees
        DECA
	
        BEQ     LB104			; do 'H', 315 degrees
LB0E7   JMP     >BasFCError 		; invalid command FC error

; change colour
LB0EA   JSR     >LA8EB			; adjust colour code for mode
        STB     <GrForeground		; set new forground colour
        JSR     >GrSetColours 		; set colour bytes
LB0F2   BRA     LB078			; do next command

; change angle
LB0F4   CMPB    #$04			; only 0..3 are legal
        BCC     LB0E7			; FC error if angle > 3
        STB     <GrDrawAngle		; else set new angle
        BRA     LB0F2			; do next command

; change scale
LB0FC   CMPB    #63			; only 0..63 are legal scales
        BCC     LB0E7			; FC error if > 63
        STB     <GrDrawScale		; set new scale
        BRA     LB0F2			; do next command

; set 315 degrees
LB104   CLRA				; negate D, make horizontal			
        BSR     LB15F
        FCB	Skip1			; skip 1

;135 degrees	
LB108   CLRA				; clear MSB of horizontal difference
        TFR     D,X			; copy horizontal difference to vertical difference
        BRA     LB166			; move & draw

; 45 degrees
LB10D   CLRA				; clear MSB of horizontal difference
        TFR     D,X			; copy horizontal difference to vertical difference
        BSR     LB15F			; negate D, make horizontal difference -ve
        EXG     D,X			; exchange horizontal & vertical differences
        BRA     LB166			; move & draw

; 225 degrees
LB116
	CLRA				; clear MSB of horizontal difference
        TFR     D,X			; copy horizontal difference to vertical difference
        BSR     LB15F			; negate D, make horizontal difference -ve
	BRA     LB166			; move & draw

; go right
LB11D   CLRA				; clear MSB of horizontal difference
LB11E   LDX     <DBZero	; set vertical difference = 0
        BRA     LB166			; move & draw

; go left
LB122   CLRA				; clear MSB of horizontal difference
        BSR     LB15F			; negate D, make horizontal difference -ve
        BRA     LB11E			; make vertical difference 0 & continue 

; go down
LB127   CLRA				; clear MSB of horizontal difference			
        BRA     LB12D			; make vertical difference = 0, exchange horizontal & vertical differences

; go up
LB12A   CLRA				; clear MSB of horizontal difference
        BSR     LB15F			; negate difference
	
LB12D   LDX     <DBZero	; X=0 , horizontal difference = 0	
        EXG     X,D			; exchange horizontal and vertical differences
        BRA     LB166			; move & draw

; execute a command substring
LB133   JSR     >LAFB6			; interpret command as if it where basic
        LDB     #$02			; check for 4 bytes of free ram
        JSR     >BasChkB2Free 		; OM error if not
	
        LDB     <EvalD8			; get current command length
        LDX     <EvalD9			; get current command pointer
        PSHS    B,X			; save them
        JMP     >LB061			; evaluate numerical value on command line
	
; multiply horizontal or vertical difference by scale factor.
; divide product by 4 and return value in D
LB144   LDB     <GrDrawScale		; get current draw scale
        BEQ     LB163			; branch if 0, this will cause a 0 scale to draw at full scale
	
        CLRA				; clear MSB
        EXG     D,X			; exchange difference & scale factor	
        STA     ,-S			; save MSB of difference on stack (sign information)
        BPL     LB151			; branch if +ve
        BSR     LB15E			; negate D
	
LB151   JSR     >LB350			; multiply difference by scale factor
        TFR     U,D			; save 2 MSB in D
        LSRA				; device by 4, each scale is 1/4 full scale
        RORB
DB158   LSRA
        RORB
        TST     ,S+			; check sign of original difference 
        BPL     LB162			; and return if +ve

LB15E   NEGA				; negate D
LB15F   NEGB
        SBCA    #$00			; and set flags
LB162   RTS

LB163   TFR     X,D			; transfer unchanged difference to D
        RTS
; move the draw position 
; add the orthogonal differences in D (horizontal) and X (vertical) to
; the current position, draw a line after the move
LB166	PSHS    D			; save horizontal difference
        BSR     LB144			; apply scale factor to vertical
	
        PULS    X			; restore horizontal difference
        PSHS    D			; save vertical difference
        BSR     LB144			; apply scale factor to horizontal
	
        PULS    X			; restore vertical difference
        LDY     <GrDrawAngle		; get draw angle and scale, 
        PSHS    Y			; save them on the stack (Y used as only unused register!)
	
; applies a transform to the draw angle multiple times, decrementing angle each time.
LB177   TST     ,S			; check draw angle
        BEQ     LB183			; branch if no angle
	
        EXG     X,D			; exchange horizontal and vertical difference
        BSR     LB15E			; negate D
	
        DEC     ,S			; decrement angle
        BRA     LB177			; loop again

		
LB183   PULS    Y			; pull angle and scale back of stack
        LDU     <DBZero	; U=0, default horizontal end position = 0
        ADDD    <GrCurrXCo		; add difference to current X co-ordinate
        BMI     LB18D			; set X co-ordinate to 0 if -ve
	
        TFR     D,U			; save X co-ordinate in U
	
LB18D   TFR     X,D			; get vertical difference
        LDX     <DBZero	; X=0, default vertical end position = 0
        ADDD    <GrCurrYCo		; add current Y co-ordinate to vertical difference
        BMI     LB197			; if Y is -ve set it to 0
	
        TFR     D,X			; transfer Y co-ordinate to X
					; new co-ordinates x,y in registers U,X
					
; move the draw position; enter with absolute horizontal position
; in U register and absolute vertical positlon in X register.
LB197   CMPU    #GrMaxX+1		; is X co-ordinate within range?
        BCS     LB1A0			; yes 
	
        LDU     #GrMaxX			; otherwise clip it to GrMaxX (255)
	
LB1A0   CMPX    #GrMaxY+1		; is Y co-ordinate within range?
        BCS     LB1A8			; yes
	
        LDX     #GrMaxY			; otherwise clip it to GrMaxY (191)
 		
LB1A8   LDD     <GrCurrXCo		; copy horizontal and vertical pointers 
        STD     <GrCurrX		; into draw line start position
        LDD     <GrCurrYCo
        STD     <GrCurrY
	
        STX     <GrPixelNoY		; set the draw line end position
        STU     <GrPixelNoX
	
        TST     <EvalD5			; check update flag
        BNE     LB1BC			; branch if no update
	
        STX     <GrCurrYCo		; update position of draw pointer
        STU     <GrCurrXCo
	
LB1BC   JSR     >LA7AE			; normalise start and end line co-ordinates
        TST     <EvalD6			; get draw flag
        BNE     LB1C6			; branch if no draw
	
        JSR     >LA82F			; draw the line, using previous code from 'LINE' comamnd
LB1C6   CLR     <EvalD5			; reset the update flag
        CLR     <EvalD6			; reset the draw flag
        JMP     >LB078			; go do another draw command

; set the draw position
LB1CD   JSR     >LAF33			; get a char from the command line
        
	PSHS    A			; save it
        JSR     >LB1F9			; evaluate horizontal difference
        
	PSHS    D			; save difference
        JSR     >LAF33			; get a character from command line
	
        CMPA    #','			; check for comma	
        LBNE    LB0E7			; FC error if no comma
	
        JSR     >LB1F6			; evaluate vertical difference
        TFR     D,X			; save vertical difference in X
        PULS    U			; get horizontal difference in U
        PULS    A			; get the first command character
	
        CMPA    #'+'			; if the first character is '+' or '-'
        BEQ     LB1F1			; treat the read values in U and X as relative
        CMPA    #'-'			; otherwise they are absolute
        BNE     LB197

; relative differences in U and X	
LB1F1   TFR     U,D			; put horizontal difference in D
        JMP     >LB166			; move & draw

LB1F6   JSR     >LAF33			; get a char from the command line

LB1F9   CMPA    #'+'			; check for leading '+' relative motion
        BEQ     LB204			; branch if relative
	
        CMPA    #'-'			; check for leading '-' relative motion
        BEQ     LB205			; branch if relative
	
        JSR     >LAF7D			; move command pointer back one if not relative
	
LB204   CLRA				; A is 0 if +ve, A<>0 if -ve
LB205   PSHS    A			; save add / subtract flag
        JSR     >LB066			; evaluate number in command string into B
        PULS    A			; restore flag
	
        TSTA				; test add / subtract flag
        BEQ     LB213			; return if add
        CLRA				; clear add / subtract flag
        NEGB				; negate offset (so adding it subtracts)
        SBCA    #$00			; set flags
LB213   RTS

; table of sines and cosines for circle
CircleSineTab
	FDB	$0000,$0001	; sub-arc 0
	FDB	$FEC5,$1919	; sub-arc 1
        FDB     $FB16,$31F2	; sub-arc 2
        FDB     $F4FB,$4A51	; sub-arc 3
        FDB     $EC84,$61F9	; sub-arc 4
        FDB     $E1C7,$78AE	; sub-arc 5
        FDB     $D4DC,$8E3B	; sub-arc 6
        FDB     $C5E5,$A269	; sub-arc 7
        FDB     $B506,$B506	; sub-arc 8

; basic CIRCLE comamnd
; the circle is actually drawn as a 64 sided polygon.  it is composed of 64 LINE commands
CmdCircle:
LB238   CMPA    #'@'			; check for '@' sign
        BNE     LB23E			; nope 
        JSR     <BasChrGet		; get a charcter, ignore @
	
LB23E   JSR     >LA8B0			; get X and Y co-ordinates into EvalD3 and EvalD5 
        JSR     >BasGetPixel		; get centre co-ordinates 	
        JSR     >NormalizeXY		; normalize them to current mode
	
        LDX     ,U			; get centre co-ordinates into correct place
        STX     <GrCircleXCo
        LDX     2,U
        STX     <GrCircleYCo
	
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >VarGet16Bit 		; get radius value, in X
        LDU     #EvalCF			; save radius 		
        STX     ,U
	
        JSR     >LA6AE			; normalize radius
        
	LDA     #$01			; set pset/preset flag to pset
        STA     <GrPlotFlag
	
        JSR     >BasGetColour		; get colour
        LDX     #$100			; height/width ratio default value 
        JSR     <BasChrGetCurr		; get a character from basic
        BEQ     LB27A			; branch if none 
	
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >L8872			; evaluate expression
        
	LDA     <FP0EXP			; get FPA0 exponent
        ADDA    #$08			; add 8 to it and resave, this will effectively multiply by 256
        STA     <BasVarFPAcc1
	
        JSR     >L8E86			; evaluate expression, result in X
	
LB27A   LDA     <GrCurrPmode		; get current pmode
        BITA    #$02			; test for pmode 0,1,4
        BEQ     LB284			; branch if so
	
        TFR     X,D			; multiply X by 2 for pmodes 2,3 as the horizontal pixels are 2x the width
        LEAX    D,X			; of pmodes 0,1,4.

LB284   STX     <EvalD1			; save hardware ratio
        
	LDB     #$01			; set pset/preset flag to pset
        STB     <GrPlotFlag
        STB     <EvalD8			; first time flag, set to 0 after arc drawn
	
        JSR     >LB37D			; evaluate circle start point, octant, subarc	
        PSHS    D			; save circle start point
        JSR     >LB37D			; evaluate circle end point, octant, subarc	
        STD     <EvalD9			; save circle end point
        PULS    D			; restore circle start point
	
LB298   PSHS    D			; store current circle position
        LDX     <GrPixelNoX		; move old end co-ordinates to new start co-ordinates
        STX     <GrCurrX
        LDX     <GrPixelNoY
        STX     <GrCurrY

        LDU     #CircleSineTab+2	; point to table of sines and cosines
        ANDA    #$01			; get octant number
        BEQ     LB2AC			; branch if even
	
        NEGB				; convert 0..7 to 8..1 for odd octant numbers
        ADDB    #$08
	
LB2AC   ASLB				; multiply by 4 as 4 bytes per sine/cosine table entry
        ASLB
        LEAU    B,U			; point U to correct table entry
        PSHS    U			; save sine/cosine table entry
	
        JSR     >LB342			; calculate horizontal offset
        PULS    U			; restore table pointer
        LEAU    -2,U			; move to cosine for vertical offset
        PSHS    X			; save horizontal offset
	
        JSR     >LB342			; calculate vertical offset
        PULS    Y			; restore horizontal offset
        
	LDA     ,S			; get subarc
        ANDA    #$03			; check to see if octant is 0,3,4,7
        BEQ     LB2CC			; branch if octant is 0 or 4
        CMPA    #$03
        BEQ     LB2CC			; branch if octant is 3 or 7

; octant 1,2,5,6	
        EXG     X,Y			; swap horizontal and vertical offsets
LB2CC   STX     <GrPixelNoX		; save horizontal offset

; the height/width ratio will only modify the vertical c-ordinate
        TFR     Y,X			; load X with calculated vertical offset
        LDD     <EvalD1			; get height/width ratio
        JSR     >LB350			; multiply vertical offset by height/width ratio
        
	TFR     Y,D			; transfer product to D
        TSTA				; check overflow flag of MSB of result
        LBNE    BasFCError 		; FC error if overflow (> 255)
	
        STB     <GrPixelNoY		; save as vertical offset MSB	
        TFR     U,D			; get LSB of result
        STA     <GrPixelNoY+1		; save LSB
	
        LDA     ,S			; get subarc
        CMPA    #$02			; branch if octant 0,1,6,7 subarc X > X of centre 
        BCS     LB2F6
	
        CMPA    #$06			; branch if octant 0,1,6,7 subarc X > X of centre 
        BCC     LB2F6
	
        LDD     <GrCircleXCo		; get X co-ordinate of centre
        SUBD    <GrPixelNoX		; subtract horizontal difference
        BCC     LB303			; branch if no underflow
	
        CLRA				; D=0
        CLRB				; if new X < 0 clip it to 0	
        BRA     LB303			; skip forward

LB2F6   LDD     <GrCircleXCo		; get X co-ordinate of centre	
        ADDD    <GrPixelNoX		; add horizontal difference
        BCS     LB301			; branch if overflow
	
        CMPD    <EvalD3			; compare to max X co-ordinate
        BCS     LB303			; branch if X < max
LB301   LDD     <EvalD3			; otherwise clip it to max
LB303   STD     <GrPixelNoX		; update X co-ordinate

        LDA     ,S			; get subarc
        CMPA    #$04			; branch if octant 0,1,2,3
        BCS     LB315			; subarc vertical end >= vertical centre
	
        LDD     <GrCircleYCo		; get Y co-ordinate of centre
        SUBD    <GrPixelNoY		; subtract vertical difference
        BCC     LB322			; branch if no underflow
        CLRA				; underflow so clip Y to 0
        CLRB
        BRA     LB322			; jump ahead

LB315   LDD     <GrCircleYCo		; get Y co-ordinate of centre
        ADDD    <GrPixelNoY		; subtract vertical difference
        BCS     LB320			; branch if overflow
	
        CMPD    <EvalD5			; compare to max Y
        BCS     LB322			; branch if less than max
	
LB320   LDD     <EvalD5			; otherwise clip it to the maximum
LB322   STD     <GrPixelNoY

        TST     <EvalD8			; check first time flag
        BNE     LB32A			; not first time, as you would draw a line from centre to edge of circle
        BSR     LB37A			; otherwise draw a line
	
LB32A   PULS    D			; get end co-ordinates
        LSR     <EvalD8			; shift first time flag
        BCS     LB335			; do not chec for end point after drawing first arc
        
	CMPD    <EvalD9			; compare current position to end point
        BEQ     LB341			; if so circle drawing finished
	
LB335   INCB				; increment subarc counter
        CMPB    #$08			; done all subarcs?
        BNE     LB33E			; no keep drawing circle
	
        INCA				; increment octant counter
        CLRB				; reset subarc counter
        ANDA    #$07			; keep octant counter in the range 0..7, once A = B, this will make A = 0
					; so the end point will be 0,0 and the circle routine will end.
LB33E   JMP     >LB298			; keep drawing circle

LB341   RTS				; exit circle routine

; multiply radius by sine/cosine value and return offset in X
LB342   LDX     <EvalCF			; get radius
        LDD     ,U			; get sine / cosine table modifier
        BEQ     LB34F			; branch if (0 - offset) = radius
        
	SUBD    #$0001			
        BSR     LB350			; multiply radius by sine/cosine
        TFR     Y,X			; return result in X
LB34F   RTS


; multiply (unsigned) two 16 bit numbers together 
; enter with one number in d, the other in x
; the 4 byte product will be stored in 4,s-7,s
; (y, u reg on the stack). 
; i.e. (aa ab) x (xh xl) = 256*aa*xh+16*(aa*xl+ab*xh)+ab*xl. 
; the 2 byte  multiplier and multiplicand are treated as a 1 byte integer part (msb) 
; with a 1 byte fractional part (lsb)
LB350   PSHS    D,X,Y,U			; save registers & reserve storage space on stack
        CLR     4,S			; reset overflow flag
        LDA     3,S			; calculate B * XL and store result in 6,S
        MUL
        STD     6,S
	
        LDD     1,S			; calculate B * XH 
        MUL
        ADDB    6,S			
        ADCA    #$00			; add carry from first multiplcation to second
        STD     5,S			; svae result at 5,S
        
	LDB     ,S			; calculate A * XL
        LDA     3,S
        MUL
        ADDD    5,S			; add result to result of previous 2 multiplcations
        STD     5,S			; save it
        BCC     LB36F			; branch if no overflow
        INC     4,S			; set overflow flag D > $FFFF
	
LB36F   LDA     ,S			; calculate A * XH
        LDB     2,S
        MUL
        ADDD    4,S			; add to previous result
        STD     4,S
        PULS    D,X,Y,U,PC		; return result in U,Y

LB37A   JMP     >LA82F			; go draw a line from from GrCurrX, GrCurrY  to  GrPixelNoX, GrPixelNoY

; calculate start or end point which is a number from 0 to 63 saved as an octant number (0-7) and a subarc number (0-7)
LB37D   CLRB				; default value of 0
        JSR     <BasChrGetCurr		; get current input char
        BEQ     LB393			; none, branch ahead
	
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >L8872			; evaluate numeric expression
        LDA     <FP0EXP			; get exponent of FPA0
        ADDA    #$06			; add 6 to exponent, multiply number by 64
        STA     <FP0EXP			; save it back
	
        JSR     >FPA0toB		; convert to integer in B
        ANDB    #$3F			; clip it to < 63
	
LB393   TFR     B,A			; save value in A also
        ANDB    #$07			; mask out octant number just leaving subarc
        LSRA				; divide by 8 leaving octant number in bottom 3 bits
        LSRA
        LSRA
        RTS

	ifdef	Dragon64ram
; Initialization routine used by the Dragon 64 when the basic ROM is copied into the top 16K of RAM
; this leaves 48K of ram available for basic, but with no access to the cartridge area.
; On a dragon 64, entry into this mode is initiated by type EXEC 48000 (this is the default exec address).
; booting into RAM basic mode preserves any basic program currently loaded. 
; it is easy to distinguish if you are in this mode as the cursor will be blue and the keyboard will
; auto-repeat if a key is held down.
  
DoHWInit
LF39B   LDX     #DF40C			; Fill in some low mem vars, data table
        LDU     #BasExecAddr 		; destination address
        LDB     #$0E			; byte count
        JSR     >UtilCopyBXtoU		; go copy them
	
        LDU     #SecVecIRQ 		; And some more, destination
        LDB     #$0E			; byte count
        JSR     >UtilCopyBXtoU		; go copy them
	
        LEAU    5,U			; yet more, move destination
        LDB     #$0B			; byte count
        JSR     >UtilCopyBXtoU		; go copy them
	
        LDX     #BasSNError		; sintax error routine
        STX     3,U
        STX     8,U
        
        LDX     #VectDevOpen            ; Set all vectors to 'RTS'
        LDD     #$394B			; code for RTS=$39, and byte count=$4B
LF3C2   STA     ,X+			; save an RTS, increment pointer
        DECB				; decrement count
        BNE     LF3C2			; loop if more to do
        
        JSR     >TextCls                ; clear screen
        JSR     >BasEraseVars		; erase basic variables
        ANDCC   #IntsEnable             ; Enable interrupts
        LDX     #BasSignonMess-1        ; print signon message
        JSR     >TextOutString
        
        LDA     #FFlagTrue             	; Flag warm start next time
        STA     <WarmStartFlag                  
        
        STA     Flag64			; flag booted into RAM 64 mode
        BRA     LF3E1			; continue to software init				

DoSWInit:
LF3DE   JSR     >TextCls		; clear screen
LF3E1   CLR     <SndTimerPlay		; clear play timer
        CLR     <SndTimerPlay+1
	
        LDA     >PIA0CRB		; get pia0 control register A
        ORA     #CRIRQ			; enable CA1 IRQ generation on /HS
        STA     >PIA0CRB		; save it back to PIA0
        CLR     <TextDevN		; reset device no to screen
        JSR     >BasResetStack		; reset basic
        ANDCC   #IntsEnable		; enable inturrupts
        JMP     >BasCmdMode		; go to basic command mode

; default FIRO handler D64 ram
DefaultFIRQ
	TST	>PIA1CRB		; test for CART interrupt
	BMI	LF3FD			; yes 
	RTI				; else return from interrupt
	
LF3FD	JSR	>Delay			; delay 65536 counts of X	

LF400   CLR     <WarmStartFlag		; clear warm start flag, cold start next time
        JMP     >CartEntryFIRQ		; go enter the cartridge at $C000

Delay	LDX	<DBZero	; X=0
LF407	LEAX	-1,X			; loops 65536 times
	BNE	LF407			
	RTS

; data copied into low ram by RAM basic boot routine
DF40C   FDB	BasFCError		; Exec address		
	INC	<$A7			; charget
	BNE	LF414
	INC	<BasAddrSigByte
LF414	LDA	>$0000
	JMP	>LBB26
	
	JMP	D64IRQ			; IRQ vector
        JMP	DefaultFIRQ		; FIRQ vector
	
        FDB     $0000			; Timer value
        FCB     $00			; unused
        FCB     $80,$4F,$C7,$52,$59	; Random number seeds
        
	FCB     $05			
		
        FCB     $4E			; no reserved words
        FDB     BasCommandWords		; word table
        FDB     BasCommandDisp		; dispatch address
        FCB     $22			; no of functions
        FDB     BasFunctionWords	; function table
        FDB     BasFunctionDisp		; Function dispatch
	
BasSignonMess:
	FCC	/(C) 1983 DRAGON DATA LTD /
        FCB     $0D
        FCC     /16K BASIC INTERPRETER 1.0      /
	FCB     $0D
MessByMicrosoft
        FCC     /(C) 1983 BY MICROSOFT/
        FCB     $0D,$0D
	FCB	$00

;*********************************************************************
; end of Dragon 64 RAM basic section                                **
;*********************************************************************
	else

LB39B   LDS     #BasBuffer		; temporary stack pointer  			
        LDA     #$37			; Enable FIRQ from PIA 
        STA     PIA1CRB	
	
        LDA     <WarmStartFlag		; Check warm start flag
        CMPA    #FFlagTrue		; Warm start ?
	BNE     DoColdStart		; No, skip on
        
	LDX     <IndVecReset		; Get secondary reset vector
        LDA     ,X			; first byte of code
        CMPA    #NOPFlag		; is it a NOP ?
        BNE     DoColdStart		; No, do cold reset
        JMP     ,X			; else jump to secondary reset

;
; Called on system power on or reset, pointed to by reset vector.
;

SysReset:
LB3B4   LEAY    <LB39B,PCR		; Point to routine to call after hw init
        JMP     >BasicHWInit 		; Do hwinit


DoColdStart   
	LDX     #$0401			; Clear sys vars area $0000-$0400
LB3BD   CLR     ,--X			; clear 2 bytes
        LEAX    1,X			; decrement counter
        BNE     LB3BD			; keep going until zero
	
        JSR     >TextCls 		; Clear text screen
        CLR     ,X+					
        STX     <BasStartProg		; Set start of basic (asumes no graphics ram)
	
; Test and size ram
	
RamTestLoop   
	LDA     2,X			; Get a byte from ram				
        COMA				; change it
        STA     2,X			; store back
        CMPA    2,X			; did the store work, is it ram ?
        BNE     LB3D9			; nope, we have hit rom
	
        LEAX    1,X			; increment pointer
        COM     1,X			; restore ram contents (cleaver !)
        BRA     RamTestLoop		; loop again

LB3D9   STX     <AddrRamTop		; Save top of RAM (physical)				
        STX     <AddrFWareRamTop	; Save top of ram (cleared)
        STX     <BasVarStrTop		; Set top of string area
        LEAX    -200,X			; Set initial 200 byte string space
        STX     <AddrStack		; Set pointer to machine stack, below string area				
        TFR     X,S			; Point Stack there
	
        JSR     >BasicSWInit 		; Do basic software init
        LDX     #DB487			; Init Exec addr, charget etc, source address
        LDU     #BasExecAddr 		; destination address
        LDB     #$0E			; byte count
        JSR     >UtilCopyBXtoU 		; go copy it

        LDU     #SecVecIRQ 		; Init FIRQ vector, basic stub etc, destination address
        LDB     #$1E			; byte count
        JSR     >UtilCopyBXtoU 		; go copy it
        LDX     #BasSNError 		; set things to point to SN error routine
	
BasBootBasic:
LB400   STX     3,U			; initialize disk command pointer to X
        STX     8,U			; initialize disk function pointer to X
	
        LDX     #VectDevOpen 		; Init RAM hooks
        LDD     #$394B			; make all $4b of them RTS :)
LB40A   STA     ,X+			; store RTS
        DECB				; decrement count
        BNE     LB40A			; loop if not all done
	
        STA     BasLinInpHead-1		; init basic input buffer
        JSR     >BasNew 		; Perform a 'NEW'
        JSR     >InitSndGraph		; initialize sound and graphics constants
        LDX     #BasUsrVecNoDisk 	; Get address of USR table
        STX     <BasUSRTableAddr	; Set it
        LDU     #BasFCError 		; Set all USR vectors to ?FC Error initially
        LDB     #$0A			; 10 vectors
LB422   STU     ,X++			; Store address
        DECB				; dec counter
        BNE     LB422			; Loop again if not all done
	
        JSR     >LAA81			; locate basic and screen memory, perform a 'NEW'
	
        LDA     PIA0CRB			; Enable Timer IRQ ????
        ORA     #CRIRQ			; enable CA1 IRQ generation on /HS
        STA     >PIA0CRB		; save it back to PIA0
        
	LDX     #CartDOSFlag		; Check for 'DK' flag for DOS cart
	CMPX    CartBase
        LBEQ    CartEntryDOS		; yes, found, jump to Dos init
	
        ANDCC   #IntsEnable		; Enable IRQ and FIRQ
	
        LDX     #BasSignonMess-1 	; Display basic signon message
        JSR     >TextOutString 
        LDX     #WarmStart 		; Setup Warmstart vector 
        STX     <IndVecReset
        LDA     #FFlagTrue		; setup warmstart flag
        STA     <WarmStartFlag		; Flag warm start
        BRA     LB466			; Enter basic command loop

WarmStart:
LB44F   NOP				; NOP, all warmstart routines must start with NOP.
        CLR     <SndTimerPlay		; Stop play timer
        CLR     <SndTimerPlay+1
	
        LDA     PIA0CRB			; Enable IRQ
        ORA     #CRIRQ			; enable CA1 IRQ generation on /HS
        STA     >PIA0CRB		; save it back to PIA0
        
	CLR     <TextDevN		; Set device no back to screen/keyboard
        JSR     >BasResetStack 		; Reset basic stack
        ANDCC   #IntsEnable		; Enable interrupts
	
        JSR     >TextCls 		; clear screen
LB466   JMP     >BasCmdMode 		; Enter basic command loop


;
; FIRQ vector, used to auto-start cartrages
;
;LB469
FIRQVector   
	TST     PIA1CRB			; FIRQ on PIA ?
        BMI     LB46F			; yes : do it
        RTI				; nope, return
	
LB46F   JSR     >LB480			; Delay
        JSR     >LB480			; Delay
        LEAY    <DoCartBoot,PCR		; Point to cart boot routine
        JMP     >BasicHWInit 		; Do hardware init, then boot cart

DoCartBoot   
	CLR     <WarmStartFlag		; Mark always cold start
        JMP     CartEntryFIRQ		; Jump to cart

LB480   LDX     <DBZero	; zero x
LB482   LEAX    -1,X			; Decrement X
        BNE     LB482			; loop again if not zero
        RTS
	
;
; The following are copied into low ram by the initialization routines
;

DB487
	ifdef	Dragon64
	FDB     DoBoot64		; Initial EXEC address
	else
	FDB     BasFCError		; Initial EXEC address
	endc
	
LB489   INC     <$A7			; Basic charget routine, copied into ram
        BNE     LB48F			; not zero, don't need to increment MSB
        INC     <BasAddrSigByte		; increment MSB
LB48F   LDA     >$0000			; get a byte from basic into A
        JMP     >LBB26			; go deal with it

LB495   JMP     >BasIRQVec 		; initial IRQ vector
LB498   JMP     >FIRQVector		; initial FIRQ vector

        FDB     $0000			; Init timer value
        FCB     $00			; unused
        FCB     $80,$4F,$C7,$52,$59	; Random number seeds
        FCB     $00,$00,$00,$00,$00	; unused
	
	ifdef	Dragon64
	FCB	$05
	else
	FCB	$00
	endc
	
        FCB     $4E			; No of reserved words
	FDB	BasCommandWords		; Pointer to command word table
	FDB	BasCommandDisp		; Pointer to dispatch routine
        FCB     $22			; No of functions
	FDB	BasFunctionWords	; Pointer to function word table
	FDB	BasFunctionDisp		; Pointer to function dispatch table

BasSignonMess:
	FCC	/(C) 1982 DRAGON DATA LTD /
        FCB     $0D
        FCC     /16K BASIC INTERPRETER 1.0      /
	FCB     $0D
MessByMicrosoft
        FCC     /(C) 1982 BY MICROSOFT/
        FCB     $0D,$0D
	
	FCB	$00

	endc

TextWaitKeyCurs2:
LB505   BSR     TextWaitKeyCurs2a	; get a character from console
        ANDA    #$7F			; mask off bit 7
        RTS

;LB50A
TextWaitKeyCurs2a   
        JSR     VectInChar 		; Check char in vector
        CLR     <CasEOFFlag             ; flag not eof
        TST     <TextDevN               ; check device no
        BEQ     LB538                   ; zero : scan keyboard
        
        TST     <CasIOBuffSize          ; Test io buffer size
        BNE     LB51A                   ; not zero : skip on, read some data
        COM     <CasEOFFlag             ; flag EOF
        RTS

LB51A   PSHS    B,X,Y,U                 ; save regs
        LDX     <CasHeadBuffAddr        ; get address of tape header
        LDA     ,X+                     ; get first byte & save on stack
        PSHS    A
        
        STX     <CasHeadBuffAddr        ; resave head buff addr (having incremented it)
        DEC     <CasIOBuffSize          ; decrement buffer size
        BNE     LB531                   ; skip if still space
        
        LDA     <TextDevN               ; get device no
        CMPA    #$FD                    ; is it -2
        BEQ     LB533                   ; yep : skip
        
        JSR     >CasOnReadBlockOff	; turn on tape, read a block, then turn it off again
LB531   PULS    D,X,Y,U,PC              ; restore and return

LB533   JSR     >LA106
        BRA     LB531

; blink cursor whilst waiting for a character from keyboard
LB538   PSHS    B,X                  	; save regs
LB53A   JSR     >BasicCursorB L		; Flash cursor?
        JSR     >BasicKbdIn 		; read keyboard
        BEQ     LB53A                   ; keep waiting
        
        LDB     #$60                    ; erase cursor ($60=VDG space).
        STB     [TextVDUCursAddr] 	; write it to screen	
        PULS    B,X,PC			; restore & return

; console output
TextOutChar:
LB54A   JSR     VectOutChar 		; call ram hook		
        PSHS    B			; save B
        LDB     <TextDevN		; get device number
        CMPB    #-3			; check for dev -3
        BNE     LB557			; nope skip
        PULS    B,PC			; yep restore & return

LB557   INCB				; inc device number
        PULS    B			; restore b, flags unaffected
        LBMI    BasicPrintOut 		; device number was -2 send to printer
        BNE     LB58F			; device number was -1 send to screen
	
        PSHS    D,X			; save regs
        LDB     <CasStatus		; check casette status
        DECB				; status will be 1 for input, so after dec will be 0
        BEQ     LB576			; branch if input, can't send output to cassette
	
        LDB     <CasIOBuffSize		; get buffer character counter
        INCB				; increment
        BNE     LB56E			; buffer not full, no need to write
        BSR     LB578			; write buffer to cassette
	
LB56E   LDX     <CasHeadBuffAddr	; get buffer address pointer
        STA     ,X+			; save character in buffer
        STX     <CasHeadBuffAddr	; put pointer back
        INC     <CasIOBuffSize		; increment character counter
LB576   PULS    D,X,PC			; restore and return

; write the full buffer to casette
LB578   LDB     #FtDataFile             ; Set tape block type to data
LB57A   STB     <CasBlockType		; set block type

        LDX     #CasIOBuff         	; reset io buffer address
        STX     <CasIOBuffAddr

        LDB     <CasIOBuffSize          ; Set character count for block
        STB     <CasBlockLen

        PSHS    A,Y,U                   ; Save regs
        JSR     >CasWriteBlock1         ; go write block
        PULS    A,Y,U           	; restore regs
        JMP     >LB882			; reset buffer for next char

; send the character to the screen
LB58F   JSR     >TextResetVDU 		; reset to text mode
        JMP     >BasicScreenOut 	; send character to screen

; set up tab field width, tab zone, current position and line width according 
; to the device selected
SetPRINTParams   
	JSR     VectDevInit 		; call RAM hook
	
        PSHS    D,X			; save regs
        CLR     <CasIOFlag		; reset print device number
        LDA     <TextDevN		; get text device number
        BEQ     LB5A9			; branch if screen
	
        INCA				; test for tape
        BEQ     LB5BA			; branch if tape
	
        LDX     <TextPrnCommaW		; get tab field width and tab zone
        LDD     <TextPrnLineW		; get printer width and position
        BRA     LB5B2			; set print parameters

; console parameters
LB5A9   LDB     <TextVDUCursAddr+1	; get cursor location LSB
        ANDB    #$1F			; keep only column number 
        LDX     #$1010			; tab field width & last tab zone
        LDA     #$20			; display screen line width
	
LB5B2   STX     <TextVDUCommaW		; save tab field width and tab zone
        STB     <TextVDUCurrCol		; save print position
        STA     <TextVDULineW		; save print width
        PULS    D,X,PC			; restore and return

; cassette params
LB5BA   COM     <CasIOFlag		; set to $FF for cassette
        LDX     #$0100			; tab field width = 1, all others 0 
        CLRA
        CLRB
        BRA     LB5B2			; set them

; this is the routine that gets an input line for basic
; exit with break key: carry = 1
; exit with enter key: carry = 0

LB5C3   JSR     >TextCls 		; clear screen
LB5C6   JSR     VectLineInputFile 	; call RAM hook
        CLR     <TextLastKey		; reset break key check, temp key storage
        LDX     #$02DD			; point to line input buffer
        LDB     #$01			; character counter, set to 1 to allow backspace
LB5D0   JSR     >TextWaitKeyCurs2 	; go get a character from console

BasInBuffFromX:
        TST     <CasEOFFlag		; test buffer flag
        BNE     LB602			; branch if no more characters in input file
	
        TST     <TextDevN		; test device number
        BNE     LB5FE			; branch if not screen
	
        CMPA    #FF			; form feed (clear key) pressed?
        BEQ     LB5C3			; yes clear screen
	
        CMPA    #BS			; backspace?			
        BNE     LB5EA			; nope, skip
	
        DECB				; decrement character count
        BEQ     LB5C6			; branch if at beginning of buffer
        LEAX    -1,X			; move buffer pointer back one
        BRA     LB61E			; skip on

LB5EA   CMPA    #$15			; shift right arrow?
        BNE     LB5F8			; nope, skip
	
LB5EE   DECB				; decrement character counter
        BEQ     LB5C6			; branch if at beggining of buffer
        LDA     #BS			; output a backspace character
        JSR     >TextOutChar 		; send it to screen
        BRA     LB5EE			; loop until at beginning of buffer

LB5F8   CMPA    #$03			; break key
        ORCC    #FlagCarry		; flag break pressed
        BEQ     LB603			; yes exit, CC.Z=1
	
LB5FE   CMPA    #CR			; enter key?
        BNE     LB60F			; no, skip
	
LB602   CLRA				; clear carry, flag enter pressed
LB603   PSHS    CC			; save flags
        JSR     >TextOutCRLF 		; output a new line
        CLR     ,X			; clear buffer terminator
        LDX     #BasLinInpBuff 		; point at beginning of buffer
        PULS    CC,PC			; restore flags and return

; insert a character into the input buffer if it is valid, and the buffer has space 
LB60F   CMPA    #' '			; control character? (before space)
        BCS     LB5D0			; branch if so
	
        CMPA    #'z'+1			; greater than lower case z?
        BCC     LB5D0			; if so ignore
	
        CMPB    #LineBufMax		; check to see if buffer full?
        BCC     LB5D0			; yes, ignore character
	
        STA     ,X+			; save character in buffer
        INCB				; increment character count
LB61E   JSR     >TextOutChar 		; output the character to the screen
        BRA     LB5D0			; loop again for next

; input device number check
LB623   JSR     VectInputFile		; call RAM hook 
        LDA     <TextDevN		; get device number	
        BEQ     LB64B			; branch if screen, return
	
        INCA				; increment device no, chech for casette
        BNE     LB639			; not cassette, generate FM error
        LDA     <CasStatus		; get cassette status
        BNE     LB636			; cassete file open, check if open for input
BasErrNO   
	LDB     #ErrNO			; not open error
        JMP     >SysErr 

LB636   DECA				; decrement cassette status
        BEQ     LB64B			; if 0 is open for input, branch to return
	
LB639   JMP     >BasFMError 		; else generate FM error	

; print devcie number check
LB63C   JSR     VectOutputFile 		; call RAM hook
        LDA     <TextDevN		; get device number	
        INCA				; inc to see if casette
        BNE     LB64B			; skip if not
	
        LDA     <CasStatus		; get caseete status
        BEQ     BasErrNO		; branch to error if not open
        
	DECA				; decrement status check for output 
        BEQ     LB639			; branch if open for input, generate FM error
LB64B   RTS				; return

; close 
LB64C   BEQ     LB65C			; branch if no name specified
        JSR     >LB7D7			; check device number
	
LB651   BSR     LB663			; go close a file
        JSR     <BasChrGetCurr		; get current basic character
        BEQ     LB681			; return if no more files
	
        JSR     >BasGetDevNo 		; check syntax and device number
        BRA     LB651			; loop to close next 

; close all files handler
LB65C   JSR     VectCloseAllFiles 	; call RAM hook
CasClosFiles:
LB65F   LDA     #-1			; casette device no
        STA     <TextDevN		; set device no

LB663   JSR     VectCloseFile 		; Close file
        LDA     <TextDevN               ; Get file device no
        CLR     <TextDevN               ; reset it
        INCA
        BNE     LB681                   ; Check for tape (-1), branch if not
        
        LDA     <CasStatus              ; get tape status
        CMPA    #$02                    ; open for output?                                 
        BNE     LB67F                   ; nope : skip
        
        LDA     <CasIOBuffSize          ; get size of IO buffer
        BEQ     LB67A                   ; empty?
        JSR     >LB578                  ; nope
        
LB67A   LDB     #BtEOF                 	; signal EOF block
        JSR     >LB57A			; go write the final block
        
LB67F   CLR     <CasStatus              ; Resate tape status to inactive
LB681   RTS

CmdCsave
	CMPA    #$4D			; CSAVEM?
        LBEQ    L9900			; yes, deal with it
	
        JSR     >GetCasFnameToBuff	; get filename
        JSR     <BasChrGetCurr		; get next character in basic line
        BEQ     CasWriteBasic 		; none, go save the file
	
        JSR     >VarCKComma 		; syntax check for comma
        LDB     #'A'			; check for ASCII save
        JSR     >VarCKChar 		; syntax check for it
        BNE     LB681			; return if not end of line
        
; save a program as ASCII, this basically does a LIST to tape.
; basic programs saved in this way can be loaded onto a CoCo or Dragon
; without being incorrectly tokenized.
	CLRA				; file type = 0
        JSR     >LB88E			; write out header block
        LDA     #DevCasette		; set device number to cassette
        STA     <TextDevN
        CLRA				; clear carry, force list to begin at program start
        JMP     >CmdList 		; do a list to tape

; save a program to tape in tokenized form
CasWriteBasic:
LB6A5   CLRA				; file type 0
        LDX     <DBZero	; zero out ascci flag and mode
        JSR     >CasWriteHeader		; write out header block
	
        CLR     <CasStatus		; force close files
        INC     <CasBlockType		; block type data
        JSR     >CasWriteLeader 	; write leader
	
        LDX     <BasStartProg		; point at start of basic program
LB6B4   STX     <CasIOBuffAddr		; set cassette data address
        LDA     #$FF			; block size 255 bytes
        STA     <CasBlockLen		; set block size
	
        LDD     <BasVarSimpleAddr	; get address of end of basic program
        SUBD    <CasIOBuffAddr		; subtract current block save address
        BEQ     LB6CD			; branch if it came out exact
	
        CMPD    #$00FF			; more than 255 bytes left to save?
        BCC     LB6C8			; yes, skip
	
        STB     <CasBlockLen		; else update blocksize for last block
LB6C8   JSR     >CasBlockOut 		; write block to tape
        BRA     LB6B4			; loop for next cassette block

LB6CD   NEG     <CasBlockType		; make block number -ve, EOF block
        CLR     <CasBlockLen		; set block length = 0
        JMP     >LB994			; write EOF block

; Basic CLOAD 
CmdCload
	CLR     <CasStatus		; reset tape status
        CMPA    #'M'			; check for 'M'
        LBEQ    CmdCloadM		; yep, go do it
	
        LEAS    2,S			; drop return address
        JSR     >CasGetFilename		; get filename
		
LB6E1   JSR     >CasCheckFindOpen	; Check filename, search and open file
        TST     CasGapFlag 		; check file mode, <> 0 is data or ascii
        BEQ     LB706			; zero, branch
        
	LDA     CasASCIIFlag 		; ASCII?
        BEQ     LB70B			; yes branch, bad file mode, 0 = crunched basic / machine code

;ASCII file, set device no to -1 tape		
        JSR     >BasNew 		; do a 'new'
        LDA     #-1			; device no -1
        STA     <TextDevN				
        INC     <CasStatus		; file status = 1, input					
        JSR     >CasOnReadBlockOff	; go load an ASCII record				
        JMP     >BasCmdMode2		; put it into the program 

LB6FD   JSR     VectCloseFileCmd 	; call RAM close file vector
        JSR     >LB663			; check and close file
        JMP     >BasCmdMode 		; return to command mode

; load a tokenized basic program
LB706   LDA     CasFType 		; get file type
        BEQ     LB70E			; if zero it is a CSAVE file
LB70B   JMP     >BasFMError 		; generate bad file mode error

LB70E   JSR     >BasNew L8417		; do a 'new'
        JSR     >BasicCassOnRd 		; turn on tape for reading
        LDX     <BasStartProg		; load address
LB716   STX     <CasIOBuffAddr		; set it
        LDD     <CasIOBuffAddr
        INCA
        JSR     >L8335			; Check we have space for block
        JSR     >CasBlockIn 		; go read it
        BNE     LB736
		
        LDA     <CasBlockType		; check block type
        BEQ     LB736			; zero : error -> exit
        BPL     LB716			; valid : loop again
        STX     <BasVarSimpleAddr	; address of simple vars just beyond program
        BSR     LB76D
        LDX     #MessOK-1		; print 'OK' message
        JSR     >TextOutString 
        JMP     >L83E7			; jump to interpreter

; come here on load error
LB736   JSR     >BasNew 		; do a new	
LB739   JMP     >BasIOError		; generate IO error	

LB73C   LDX     <DBZero	; X=0	
        JSR     <BasChrGetCurr		; get next character from basic 
        BEQ     CasReadBin 		; no character just read it
        JSR     >VarCKComma 		; syntax check for comma
        JSR     >VarGet16Bit 		; get offset (if any)
	
CasReadBin:
LB748   LDA     CasFType 		; get filetype from header	
        CMPA    #FtMachineCode		; is it a machine code program?
        BNE     LB70B			; nope, generate FM error
	
        LDD     CasEntryAddr 		; get exec address from header
        LEAU    D,X			; add supplied offset
        STU     <BasExecAddr		; save in exec address
        LDD     CasLoadAddr 		; get tape load address from header
        LEAX    D,X			; add supplied offset
        STX     <CasIOBuffAddr		; save new load address
	
        JSR     >BasicCassOnRd 		; turn tape on prepare for read
LB760   JSR     >CasBlockIn 		; load a block from tape
        BNE     LB739			; branch if error
	
        STX     <CasIOBuffAddr		; update load address for next block
        TST     <CasBlockType		; check for end of file block
        BEQ     LB739			; branch if I/O (header block)
        BPL     LB760			; go read some more blocks, EOF block is type -1

LB76D   JMP     >BasicCassOff 		; turn off the tape, load done

; basic EXEC
CmdExec
	BEQ     LB777			; check for parameter (exec address)
					; if none, use the last supplied value
        JSR     >VarGet16Bit 		; get new exec address
        STX     <BasExecAddr		; save it in the exec vector
LB777   JMP     [BasExecAddr] 		; call exec vector

; break check
LB77B   JSR     VectCheckKeys 		; call RAM hook
        LDA     <TextDevN		; check input devioce number
        INCA				; inc it, tape is -1 so will become 0
        BEQ     LB7D3			; branch if tape don't check keyboard
        JMP     >BasPollKeyboard 	; go poll the keyboard for break etc

; basic PRINT @
DoPrintAT
	JSR     >L8B21			; evaluate supplied location
        SUBD    #(TextScreenLen)-1	; only 512 character positions 0..511
        LBHI    BasFCError 		; if higher than 511, FC error
	
        ADDD    #TextScreenLast		; add offset back in (subtracted above) +
					; base of text video ram
        STD     <TextVDUCursAddr	; set the cursor address
        RTS

; basic INKEY$ function		
CmdINKEYS	
	LDA     <TextLastKey		; was a key down in the break check (above)
        BNE     LB79D			; yes, go get it....
	
        JSR     >BasicKbdIn 		; scan keyboard
LB79D   CLR     <TextLastKey		; clear last key
        STA     <FPA0+3			; store key in FAP0+3
        LBNE    L8DD5			; convert FPA0+3 to a string
	
        STA     <StrDesc		; set length of string = 0, if no key pressed
        JMP     >L8DE1			; return string to basic


GetCasFnameToBuff   
	LDX     #CasFNameLen		; Clear out name buffer
        CLR     ,X+                     ; zero first byte        
        LDA     #$20                    ; fill rest with space
LB7B1   STA     ,X+
        CMPX    #CasIOBuff              ; do  all of name buffer        
        BNE     LB7B1
        
        JSR     <BasChrGetCurr          ; Get next character from basic
        BEQ     LB7D3                   ; any : no skip, load first file we come to
        
        JSR     >VarGetStr              ; Get string from basic
        JSR     >BasGetStrLenAddr       ; get addr
        LDU     #CasFNameLen 		; put in filename
        STB     ,U+
        BEQ     LB7D3

	FCB	Skip2			; skip 2 bytes
LB7CA   LDB	#CasFilenameLen		; entry to copy filename to filename cassette header

; used several times througout this ROM and the DragonDOS ROM	
UtilCopyBXtoU:
LB7CC   LDA     ,X+                    	; get a byte from [X]
        STA     ,U+                     ; put in [U]
        DECB                            ; decrement count
        BNE     UtilCopyBXtoU 	        ; loop if more
LB7D3   RTS

; get device number from basic line and check validity
BasGetDevNo:
LB7D4   JSR     >VarCKComma 		; syntax check for comma
LB7D7   CMPA    #'#'			; is next character a number
        BNE     LB7DD			; no, skip
	
        JSR     <BasChrGet		; get next character
LB7DD   JSR     >L8872			; evaluate expression			
LB7E0   JSR     >INTConv		; convert FPA0 to an integer in D

        ROLB				; MSB of B to carry
        ADCA    #$00
        BNE     BasDNError		; device no error if A<$FF80 or >$007F
        RORB				; restore B
	
        STB     <TextDevN		; set device number
        JSR     VectDevNo 		; call RAM hook
	
        BEQ     LB7F6			; branch if device = screen
        BPL     BasDNError		; DN error if device number is +ve
        CMPB    #-2			; lowest legal device number
        BLT     BasDNError		; error if lower
LB7F6   RTS

; this routine will scan off the file name from a basic line and generate 
; a syntax error if there are any characters following the end of the name
CasGetFilename
	BSR     GetCasFnameToBuff	; get filename into filname bufffer
LB7F9   JSR     <BasChrGetCurr		; get current char from command line
LB7FB   BEQ     LB7F6                   ; nothing : return
        JMP     >BasSNError 		; else SN error

; basic EOF function
CmdEOF  JSR     VectCheckEOF 		; call RAM hook
        LDA     <TextDevN		; get device number
        PSHS    A			; save on stack
        BSR     LB7E0			; check device number
	
        JSR     >LB623			; check for propper file and mode
        CLRB				; not EOF, flag=0
        LDA     <TextDevN		; get device number
        BEQ     LB816			; branch if not console
	
        TST     <CasIOBuffSize		; any characters in buffer?
        BNE     LB816			; yes, skip
        COMB				; set EOF flag = $FF
LB816   PULS    A			; restore device number
        STA     <TextDevN		; save it
LB81A   SEX				; uuhuhuhuhuhh uhuhuhuhuhuhu
					; convert 8 bit signed to 16 bit signed....
        JMP     >VarAssign16Bit2 	; return it to basic

; basic SKIPF command
CmdSKIPF   
	BSR     CasGetFilename		; get a filename if any supplied
        BSR     CasCheckFindOpen	; go find it	
        JSR     >LB903			; read the file
        BNE     BasIOError		; generate IO error, if error reading
        RTS

; basic OPEN command
CmdOPEN JSR     VectDevOpen 		; call RAM hook
        JSR     >VarGetStr 		; get file status : input,output
        JSR     >BasGetStrFirst 	; get first byte of status string to B
        PSHS    B			; save it on satck
        BSR     BasGetDevNo 		; get device number
	
CmdOpenEntry:
        JSR     >VarCKComma 		; syntax check for comman
        BSR     CasGetFilename		; get filename 
        LDA     <TextDevN		; get device number
        CLR     <TextDevN		; clear device number
        PULS    B			; recover first byte of status
	
        CMPB    #'I'			; open input?
        BEQ     LB856			; yes
	
        CMPB    #'O'			; output mode?
        BEQ     LB88A			; yes
BasFMError:
LB848   LDB     #ErrFM			; FM (File Mode) error
        FCB     Skip2			; skip 2

BasIOError   
LB84B   LDB     #ErrIO			; IO (Input/Output) error
        FCB	Skip2			; skip 2

BasAOError
LB84E   LDB	#ErrAO			; AO (already open) error
        FCB	Skip2			; skip 2

BasDNError        
LB851   LDB	#ErrDN			; DN (Device Number) error
	
        JMP     >SysErr L8344

LB856   INCA				; device number set to cassette?
        BMI     BasFMError 		; FM error if device -ve but not tape
        BNE     LB889			; return if devnum set to screen or disk

; set to cassette	
        BSR     CasCheckFindOpen	; get header block
        LDA     CasASCIIFlag 		; is it ASCII?
        ANDA    CasGapFlag 		; and with file mode
        BEQ     BasFMError 		; bad file mode if crunched basic or machine language
        INC     <CasStatus		; open file for input
	
; 
; Turn on tape relay, read a block from tape and then turn off relay.
; will read next block if current block is zero length, will update
; ioblock size
;
;LB867  
CasOnReadBlockOff 
        JSR     >CasReadBlock1          ; Turn on tape, read one block, turn off tape
        BNE     BasIOError              ; branch if error
        
        TST     <CasBlockType           ; check block type
        BEQ     BasIOError       	; IO error if header block	                     
        BMI     LB889			; branch if this is the EOF block, return
        
        LDA     <CasBlockLen            ; Get block length
        BEQ     CasOnReadBlockOff       ; zero, loop again
        
LB876   STA     <CasIOBuffSize          ; save in io buff size
        BRA     LB884                           

; search for name in Casbuf
CasCheckFindOpen
	TST     <CasStatus		; is the file open?
        BNE     BasAOError		; yes : error
	
        BSR     CasFindFile 		; go find the file
        BNE     BasIOError		; branch if error
	
LB882   CLR     <CasIOBuffSize          ; Reset io buffer size

LB884   LDX     #CasIOBuff              ; get address of tape buffer
        STX     <CasHeadBuffAddr        ; point to header
LB889   RTS                             ; return

; write out the header block
;	offset		size		use
;	casbuf 		8		file name
;	casbuf+8	1		file type
;	casbuf+9	1		ascii flag
;	casbuf+10	1		file mode
;	casbuf+11	2		transfer (exec) address
;	casbuf+13	2		start address
;
;	casbuf     		+8      +9    +10
;				type   ascii   mode
;	basic crunched          $00     $00     $00
;	basic ascii             $00     $ff     $ff
;	data                    $01     $ff     $ff
;	machine language        $02     $00     $00
;	machine blk load	$02	$00	$ff
;

; enter here for data files w/device number in acca
LB88A   INCA          			; check for cassette device number                  
        BNE     LB889			; nope, return
	
        INCA				; make file type 1
; enter here for ASCII files	
LB88E   LDX     #$FFFF			; set ASCII flag and mode 
CasWriteHeader
	TST     <CasStatus		; is file open?
        BNE     BasAOError		; yes: AO error
	
        LDU     #CasIOBuff 		; set cassett io buffer address 
        STU     <CasIOBuffAddr
        STA     8,U			; file type in CasIOBuff+8
        STX     9,U			; ASCII flag and mode in CasIOBuff+9

        LDX     #CasFName 		; point to filename buffer
        JSR     >LB7CA			; copy filename to buffer
	
        CLR     <CasBlockType		; zero block number
        LDA     #FNameBlockLen		; set filename block length (15)
        STA     <CasBlockLen
        JSR     >CasWriteBlock1 	; go write filename block
        LDA     #CasOutputFile		; set file to output
        STA     <CasStatus
        BRA     LB882			; reset buffer pointers for next block

; search for the correct cassett file
CasFindFile:
LB8B3   LDX     #CasIOBuff 		; point to cassette io buffer 
        STX     <CasIOBuffAddr		; save it in io address pointer
	
LB8B8   LDA     <BasCurrentLine		; get current line number
        INCA				; direct mode if A=$FF
        BNE     LB8C8			; branch if not direct mode
	
        JSR     >TextCls 		; clear the screen
        LDX     <TextVDUCursAddr	; get the cursor pos in X	
        LDB     #'S'			; S for searching
        STB     ,X++			; write it to screen, advance cursor 2 places
        STX     <TextVDUCursAddr	; save cursor address back
	
LB8C8   BSR     CasReadBlock1 		; go and read a cassette block
        ORB     <CasBlockType		; or error flag with block number
        
; Major difference between Dragon 32 and Dragon 64 here, Dragon 32, like the CoCo
; gives up on error. The Dragon 64 keeps trying to read the tape.

	ifdef	Dragon64
	BNE	LB8C8			; branch if not block 0 or error
	else
	BNE     LB902			; branch if not block 0 or error
	endc

; check the filename from the loaded block with the searched for filename, each character
; from the found filename has the character from the searched for filename subtracted 
; from it. If they are the same the result of this will be zero. This result is then
; ored into a (previously cleared) byte on the stack. Once the comparison has taken place
; this byte is examined, and if it is zero the filenames match, because all the subtractions
; where zero.
	
        LDX     #CasIOBuff 		; get pointer to loaded filename block
        LDU     #CasFName 		; get pointer to searched for filename
        LDB     #CasFilenameLen		; get filename (max) length
        CLR     ,-S			; clear a byte on stack
	
LB8D8   LDA     ,X+			; get a byte from the loaded filename
        LDY     <BasCurrentLine		; get current basic line ($FFFF if direct mode)
        LEAY    1,Y			; increment by 1, will now be $0000 if direct mode
        BNE     LB8E6			; not direct mode, skip on
	
        CLR     <TextDevN		; set device number to screen
        JSR     >TextOutChar 		; output character of filename
LB8E6   SUBA    ,U+			; subtract searched for name from loaded name character
        ORA     ,S			; mask any on bits with stack byte
        STA     ,S			; update stack byte
        DECB				; decrement character counter
        BNE     LB8D8			; loop for next filename character
	
        LDA     ,S+			; load check byte from stack
        BEQ     LB8FD			; zero so filenames match!
	
        TST     -9,U			; check the number of characters of the searched for filename
        BEQ     LB8FD			; zero, so we succeed as this is the first file we found

; we didn't find the right file :(	
        BSR     LB903			; look for file			
        BNE     LB902			; return if error
        BRA     LB8B8			; go look at more files 

LB8FD   LDA     #'F'			; put a 'F' on the screen if direct mode to flag we found it
        BSR     LB92A			; go do it
        CLRA				; flag no error
LB902   RTS				; return

LB903   TST     CasGapFlag 		; check file mode
        BNE     LB911			; branch if ASCII or data
	
        JSR     >BasicCassOnRd 		; turn on cassette
LB90B   BSR     CasBlockIn 		; read next block
        BSR     LB917			; check for error or last block
        BRA     LB90B			; keep going reading blocks

LB911   BSR     CasReadBlock1 		; read next block (gapped?)
        BSR     LB917			; check for error or last block
        BRA     LB911			; keep reading blocks

LB917   BNE     LB91F			; got an error reading block
        LDA     <CasBlockType		; get block type
        NEGA				; check for EOF block
        BMI     LB932			; no, return
        DECA				; if block type was EOF ($FF), A is now zero
	
LB91F   STA     <CasIOErrorCode		; save cassette io code
        LEAS    2,S			; drop return address from stack
        BRA     LB938			; turn off motor, and return

LB925   LDA     TextScreenBase			; Get char from top LSH of screen
        EORA    #$40			; Invert it
LB92A   LDB     <BasCurrentLine		; get count
        INCB				; increment	
        BNE     LB932			; only save flopped byte back if count=0
        STA     TextScreenBase			; save inverted byte
LB932   RTS

CasReadBlock1:
LB933   JSR     >BasicCassOnRd		; Turn on tape relay
        BSR     CasBlockIn 	        ; read block in

LB938   JSR     >BasicCassOff 		; turn off tape
        LDB     <CasIOErrorCode		; save error code
        RTS

CasBlockIn:
LB93E   ORCC    #IntsDisable		; disable inturrupts
        BSR     LB925
        LDX     <CasIOBuffAddr		; point to io address
        CLRA

; Sync with beggining of block

LB945   JSR     >BasicCassBitIn 	; Get a bit from tape
        RORA				; rotate 
        CMPA    #BlockBegin		; Begining of block marker ?
        BNE     LB945			; nope : keep looking
	
        JSR     >BasicCassByIn 		; get block type
        STA     <CasBlockType
        JSR     >BasicCassByIn 		; get block len
        STA     <CasBlockLen			
        ADDA    <CasBlockType					
        STA     <CasCkSum		; calculate checksum 	
        LDA     <CasBlockLen		; Get len
        STA     <CasIOErrorCode		; save in error code, used as count
        BEQ     LB972			; zero len !
	
LB961   JSR     >BasicCassByIn 		; get a byte
        STA     ,X			; store in buffer
        CMPA    ,X+			; check stored ok ! (check for rom)
        BNE     LB97C			; nope : flag io error
        ADDA    <CasCkSum		; add read byte to checksum
        STA     <CasCkSum			
        DEC     <CasIOErrorCode		; decrement byte count
        BNE     LB961			; if more bytes loop again
	
LB972   JSR     >BasicCassByIn 		; read checksum byte
        SUBA    <CasCkSum		; check agains calculated checksum
        BEQ     LB97E			; if zero, then block valid, skip ahead
        
	LDA     #$01			; else flag io error	
        
	FCB	Skip2
LB97C   LDA	#$02

LB97E   STA     <CasIOErrorCode		; save error code
        RTS				; return

CmdMOTOR   
	TFR     A,B			; save current token in B
        JSR     <BasChrGet		; get next character from basic
	
        CMPB    #DTokOFF		; off token?
        BEQ     LB996			; yep turn motor off
	
        CMPB    #DTokON			; on token?
        JSR     >LB7FB			; syntax error if not on or off
        JMP     >BasicCassOn 		; turn motor on

CasWriteBlock1:
LB991   JSR     >CasWriteLeader 	; write leader tone
LB994   BSR     CasBlockOut 		; write block out
LB996   JMP     >BasicCassOff 		; turn motor off

; write a block to cassette
; buffer size in CasBlockLen
; starting addr in CasIOBuffAddr
; block number in CasBlockType

CasBlockOut:
LB999   ORCC    #IntsDisable		; disable interrupts
        LDB     <CasBlockLen		; get block length
        STB     <CasIOErrorCode		; use error code as temp byte count
        LDA     <CasBlockLen		; get block length (included in checksum)
        BEQ     LB9AA			; branch if nothing to do (no characters)
        
	LDX     <CasIOBuffAddr		; point to block buffer
LB9A5   ADDA    ,X+			; add character from block to checksum
        DECB				; decrement counter
        BNE     LB9A5			; loop till all of block summed
	
LB9AA   ADDA    <CasBlockType		; add block type to checksum
        STA     <CasCkSum		; save in checksum
        
	LDX     <CasIOBuffAddr		; point to cassette bufffer 
        BSR     LB9CD			; send a SyncByte ($55) to tape
        
	LDA     #BlockBegin		; send BlockBegin ($3C) to tape 
        BSR     LB9CF
	
        LDA     <CasBlockType		; send block type to tape
        BSR     LB9CF
	
        LDA     <CasBlockLen		; send block length to tape
        BSR     LB9CF
        
	TSTA				; test length and set flags
        BEQ     LB9C9			; branch if block is zero length
	
LB9C1   LDA     ,X+			; get a byte from block buffer
        BSR     LB9CF			; send it to tape
        DEC     <CasIOErrorCode		; decrement byte count
        BNE     LB9C1			; keep going if more bytes to send

LB9C9   LDA     <CasCkSum		; get block checksum
        BSR     LB9CF			; send it to tape
	
LB9CD   LDA     #SyncByte		; get a SyncByte ($55)
LB9CF   JMP     >BasicCassByOut 	; send it to tape	


; basic SET command
CmdSET  BSR     GetTestLRGCoOrds	; go and get co-ordinates to operate on			
        PSHS    X			; save display address of character cell
        JSR     >VarGetComma8 		; syntax check for comma and get colour
        PULS    X			; restore screen pointer
        CMPB    #LoMaxColour		; check colour valid	
        BHI     LBA24			; generate FC error if invalid
	
GrSetLRGPixel:
LB9DF   DECB				; change colour from 0..8 			
        BMI     LB9E7			; branch if setting pixel black 
	
        LDA     #$10			; multiply colour to set by 16, 
        MUL				; as offset between differnt colours in SG4	
        BRA     LB9EF			; skip ahead

LB9E7   LDB     ,X			; get current colour from screen
        BPL     LB9EE			; branch if not graphic
        ANDB    #$70			; save only the colour info

        FCB	Skip1			; skip 1 byte
LB9EE   CLRB				; reset ASCII byte to zero colour

LB9EF   PSHS    B			; save colour info (to be set)
        BSR     LBA5C			; syntax check for ')'
			
        LDA     ,X			; get current character from screen
        BMI     LB9F8			; branch if graphic
	
        CLRA				; reset ASCII character to all pixels off
	
LB9F8   ANDA    #$0F			; save only on off info
        ORA     <GrSetResetData		; or with pixel to turn on
        ORA     ,S+			; or with saved colour info
LB9FE   ORA     #$80			; make sure it's SG4 grapic char
        STA     ,X			; write it back to the screen
        RTS

; basic RESET command
CmdRESET
	BSR     GetTestLRGCoOrds	; go and get co-ordinates to operate on	 
        BSR     LBA5C			; syntax check for ')'
GrResetLRGPixel:
LBA07   CLRA				; make a zero graphic block for the case you
					; are trying to reset a non graphic block
        LDB     ,X			; get character from screen
        BPL     LB9FE			; branch if it's non-graphic
        COM     <GrSetResetData		; invert pixel on/off mask
        ANDB    <GrSetResetData		; mask off the required pixel
        STB     ,X			; save back to screen
        RTS

; this routine will check syntax and check for legal values
; of set,reset & point horizontal and vertical parameters
; it will return their absolute screen address in the x register.
; which of the four pixels of the graphic block selected is returned in
; GrSetResetData.
;
; get set / reset / point co-ordinates
GetTestLRGCoOrds   
	JSR     >VarCKOpBrac 		; syntax check for open bracket
LBA16   JSR     >VarGet8Bit 		; get X co-ordinate
        CMPB    #LoMaxX			; check X co-ordinate in range
        BHI     LBA24			; generate FC error if too big
	
        PSHS    B			; save X co-ordinate on stack
        JSR     >VarGetComma8 		; syntax check for comma & get Y co-ordinate
        CMPB    #LoMaxY			; check Y co-ordinate in range
LBA24   BHI     LBA97			; generate FC error if too big
        PSHS    B			; save Y co-ordinate on stack
	
GrCalcPixelPos:
LBA28   LSRB				; divide Y co-ordinate by 2 to get character cell
        LDA     #TextCharsLine		; 32 character per row
        MUL				; multiply to get row offset into display
        
	LDX     #TextScreenBase		; point X at base of text screen
        LEAX    D,X			; add Y offset calculated above
	
        LDB     1,S			; get X co-ordinate 
        LSRB				; divide by 2 to get correct character cell
        ABX				; add to X to get address of correct character cell
        PULS    D			; restore X (in B) and Y (in A) co-ordinates
	
        ANDA    #$01			; keep only bottom bit of Y co-ordinate
        RORB				; bottom bit of X co-ordinate into carry
        ROLA				; and then into bottom bit of A
        LDB     #$10			; make a bitmask to turn on bit 4

LBA3D   LSRB				; shift right once
        DECA				; shifted enough ?
        BPL     LBA3D			; no loop again

; B now contains pixel number as bitmask :
; 8 = upper left,
; 4 = upper right
; 2 = lower left
; 1 = lower right	
        STB     <GrSetResetData		; save it for SET/RESET/POINT
        RTS

; basic POINT command
CmdPoint   
	BSR     LBA16			; go get lo-res co-ordinates
        LDB     #$FF			; initial value of on/off flag, all off
        LDA     ,X			; get character from the screen
        BPL     LBA59			; it's not a graphic, so all off
	
        ANDA    <GrSetResetData		; extract the pixel in question from screen byte
DBA4E   BEQ     LBA58			; it's off, so skip

        LDB     ,X			; read screen data into b
        LSRB				; shift colour data to bottom nibble of b
        LSRB
        LSRB
        LSRB
        ANDB    #$07			; mask the colour part out of it
LBA58   INCB				; return as a valid colour 0..8
LBA59   JSR     >LB81A			; convert to 16 bit number & return it to basic

LBA5C   JMP     >VarCKClBrac 		; syntax check for ')'

; basic CLS command
CmdCLS  JSR     VectAccessScreen 	; call RAM hook
        BEQ     TextCls 		; branch if no colour specified
	
        JSR     >VarGet8Bit 		; get colour to clear
        CMPB    #LoMaxColour		; validate supplied colour
        BHI     LBA86			; colour invalid
	
        TSTB				; check for colour 0	
        BEQ     LBA74			; yep go do it
	
        DECB				; make colour zero based
        LDA     #LRGColourDiff		; generate the correct colour block
        MUL
        ORB     #MaskAllOn		; mask on bottom bits, so all pixels on
LBA74   ORB     #MaskLRG		; force it to be a graphic

        FCB     Skip2			; skip 2 bytes

TextCls:
LBA77   LDB     #$60			; VDG space
TextClsChar:
LBA79   LDX     #TextScreenBase		; point X at base of screen
        STX     <TextVDUCursAddr	; reset cursor address to base of screen
LBA7E   STB     ,X+			; save character in screen
        CMPX    #TextScreenLast		; end of screen?
        BLS     LBA7E			; nope keep going
        RTS

; for CLS x where X is greater than 8.....
LBA86   BSR     TextCls 		; clear the screen
        LDX     #MessByMicrosoft-1	; point to MS message
        JMP     >TextOutString 		; output it on the screen

LBA8E   JSR     >VarCKComma 		; syntax check for comma
LBA91   JSR     >VarGet8Bit 		; get 8 bit value	
        TSTB				; is it zero ?
        BNE     LBAD3			; no, return 
LBA97   JMP     >BasFCError 		; else generate FC error

; basic SOUND command
CmdSOUND
	BSR     LBA91			; evaluate expression, sound frequency
        STB     <SndPitch		; save it
        BSR     LBA8E			; evaluate expression sound length
	
SndBeep:
LBAA0   LDA     #$04			; constant factor
        MUL				; calculate sound length
        STD     <SndLength		; save it
	
        LDA     PIA0CRB			; enable 50/60Hz interrupt
        ORA     #CRIRQ			; from PIA0, IRQ
        STA     PIA0CRB
	
	ifdef	Dragon64
	NOP				
	CLRB
	else
        CLR     <BasDisArraySearch	; clear disable adday searc (for no apparent reason!)
	endc
	
        BSR     LBAF1			; enable D/A output of sound mux
        BSR     SndEnable 		; turn on audio, enable sound mux
LBAB3   BSR     SysResetDA 		; store half value (2.5v) to D/A and wait
        
	LDA     #$FC			; data to make D/A max value
        BSR     SysWriteDA 		; write it
        
	BSR     SysResetDA 		; make D/A half value
        
	LDA     #$00			; make D/A minimum value
        BSR     SysWriteDA 		; write it
        LDX     <SndLength		; get sound length, IRQ routine decrements it
        BNE     LBAB3			; if sound not done yet, loop again

SndDisable:
LBAC3   CLRA				; disable analog mus
	FCB	Skip2			; CMPX    #D8608

SndEnable:
LBAC5   LDA	#$08			; enable analog mux
        STA     ,-S			; save enable / disable flag on stack
	
        LDA     PIA1CRB			; get control regsiter of PIA 1 side B
        ANDA    #$F7			; reset bit 3, turn off mux
        ORA     ,S+			; or in mux on/off flag
        STA     PIA1CRB			; write back to PIA
LBAD3   RTS

SysResetDA:
LBAD4   
; the Dragon 32 uses the same value as the CoCo, this has the effect of
; also writing to the printer strobe, which is not what we want!
	ifdef	Dragon64
	LDA     #$7C			; Middle value Dragon 64
	else
	LDA     #$7E			; Middle value Dragon 32
	endc
	
SysWriteDA:
LBAD6   STA     PIA1DA			; write the value to the D/A
        LDA     <SndPitch		; get sound pitch 
LBADB   INCA				; increment it
        BNE     LBADB			; loop until it reaches zero
        RTS

; basic AUDIO command
CmdAudio:
LBADF   TFR     A,B			; save on/off token in B	
        JSR     <BasChrGet		; get next token
	
        CMPB    #DTokOFF		; off token?
        BEQ     SndDisable 		; yes disable sound
	
        SUBB    #DTokON			; on token?
        JSR     >LB7FB			; syntax error if not 'ON'
	
CasAudioOn:
LBAEC   INCB				; now B=1 for on
SndDTOAOn:
        BSR     LBAF1			; route cassette sound to audio mux
        BRA     SndEnable 		; go enable it
	
; this routine will transfer bit 0 of B to sel 1 of
; the analog multiplexer and bit 1 of B to sel 2.
LBAF1   LDU     #PIA0CRA		; point at PIA
        BSR     LBAF6			; program first control register
	
LBAF6   LDA     ,U			; get PIA control register
        ANDA    #$F7			; mask off bit 3
        ASRB				; move bit 0 from B into carry
        BCC     LBAFF			; carry clear leave bit in CR at 0
        ORA     #$08			; set bit in CR
LBAFF   STA     ,U++			; save it back to PIA
        RTS

; part of IRQ routine that handles SOUND command
LBB02   LDX     >SndLength 		; get sound length
        BEQ     LBB0C			; zero, exit IRQ
        LEAX    -1,X			; decrement length
        STX     >SndLength 		; save it back
LBB0C   RTI

CmdJoystk:
LBB0D   JSR     >FPA0toB		; evaluate joystk argument
        CMPB    #$03			; compare to max number of joystick directions
        LBHI    BasFCError 		; higher, FC error
	
        TSTB				; set flags
        BNE     LBB1C			; get new data only if JOYSTK(0)
        JSR     >BasicJoyIn 		; get new data for all joysticks
	
LBB1C   LDX     #BasJoyVal0 		; point at joystick values
        LDB     <FPA0+3			; get joystick number
        LDB     B,X			; read value of joystick
        JMP     >VarAssign8Bit 		; and return it to basic

; joystk data at:
;	BasJoyVal0 ($15A)      	Left, vertical (Y)
;	BasJoyVal1 ($15B)      	Left, horizontal (X)
;	BasJoyVal2 ($15C)      	Right, vertical (Y)
;	BasJoyVal3 ($15D)	Right, horizontal (X)


;
; set carry if numeric 
; return with
; zero flag set if acca = 0 or 3a(:) - end of basic line or sub line
; part of charget routine, that gets characters from basic program 
LBB26   CMPA    #'9'+1			; is this character greater than ASCII '9' ?
        BCC     LBB34			; branch if > '9'
	
        CMPA    #' '			; is this a space?
        BNE     LBB30			; no, skip on 
        JMP     <BasChrGet		; yes, fetch next character spaces ignored.

LBB30   SUBA    #'0'			; Set carry if character > '0'
        SUBA    #-'0'
LBB34   RTS

; spare filler bytes?
	ifndef	Dragon64ram
        ifdef	Dragon64
	FILL	$00,7
	else
	FCC     /9999999n/
        FCB     $A4
        FCB     $39
        FCB     $39
	endc
	else
	endc
	
;
; Do initial hardware init, program PIAs, SAM & VDG.
; Dragon 32, Dragon 64 & Alpha - ROM mode.
;

	ifndef	Dragon64ram
;LBB40
DoHWInit  
	LDD     #$0034			; Setup PIA0
        LDX     #PIA0DA
        STA     1,X			; zero ctrl regs, selects DDRs
        STA     3,X
        STA     ,X			; A=$00, $FF00 all inputs
        COMA				
        STA     2,X			; A=$FF, $FF02 all output
        STB     1,X			; $34, CB=output, IRQ disabled, Data reg selected
        STB     3,X

        LDX     #PIA1DA			; Setup PIA1
        CLR     1,X			; zero ctrl regs, selects DDRs
        CLR     3,X
        DECA				 
        STA     ,X			; A=$FE, B7..1=output, B0=input
        LDA     #$F8			
        STA     2,X			; A=$F8, B7..3=output, B2..0=input
        STB     1,X			; $34, CB=output, IRQ disabled, Data reg selected
        STB     3,X			
        CLR     ,X			; Zero outputs of PIA1DA
        CLR     2,X			; Zero outputs of $ff22
	
	ifdef	Dragon64
        LDD     #$0A98			; init ACIA
        STD     AciaCmd
        LDA     AciaData
	else
        LDA     2,X			; Read memory config bit from PIA, $ff22
	endc
	
        LDX     #SAMCV0			; Zero SAM bits for VDG mode & Display offset
        LDB     #$10
LBB70   STA     ,X++
        DECB
        BNE     LBB70
	
        STB     SAMSF1			; Set display offset to $0400

	ifndef	Dragon64
	
        BITA    #$04			; Check memory config Dragon 32 only
        BEQ     LBB81			; jump ahead if 64K rams
        STB     SAMSM0			; Setup for 1 or 2 banks of 16K
	
LBB7F   BRA     LBB84			; skip 
	endc

LBB81   STB     SAMSM1			; Setup for 1 bank of 64K (or half good 64K)
	
	ifdef	Dragon64
	BRA	LBB83

	
SysBoot64:				
LBB80   JMP     >DoBoot64		; Boot to all RAM mode, 64/Alpha

LBB83	clrb
	endc

LBB84   
	TFR     B,DP			; Set DP=0
        TFR     Y,PC			; chain routine ($B39B)

DoSWInit   
	LDX     #DBB9F			; Init some stuff in low ram
        LDU     #TextCursFalshCnt 
        LDB     #$0D
        BSR     LBB97
	
        ifdef	Dragon64		
        JSR     >LBEFF			; do Dragon 64 only init
	else
	LDU     #TextPrnAutoCRLF 	; point at table
	endc
	
        LDB     #$09
	
LBB97   LDA     ,X+			; get a byte from X
        STA     ,U+			; copy it to U
        DECB				; decrement B
        BNE     LBB97			; loop if more
        RTS
	
;
; Data copied into ram by above
;
	

	ifdef	Dragon64
DBB9F   FCB     $32			; cursor flash count
        FDB     $0100			; No of leader bytes on cassette files
        FCB     $12			; Min cycle width 1200Hz
        FCB     $0A			; Min pulse width 1200Hz
        FCB     $12			; Max pulse width 1200Hz
        FDB     $DA5C			; Cassett motor delay
        FDB     $045E			; Keyboard debounce value
        FCB     $10			; Line printer comma width
        FCB     $74			; Line printer last comma field
        FCB     $84			; Line printer width
	
        FCB     $FF			; Auto line feed flag	
        FCB     $FF			; Alpha lock flag
        FCB     $01			; Printer EOL length
        FCB     $0D			; CR
        FCB     $0A			; LF
        FCB     $00,$00,$00,$00		; D.N.Smeed wasn't here !
	else
DBB9F   FCB     $32			; cursor flash count
        FDB     $0080			; No of leader bytes on cassette files
        FCB     $12			; Min cycle width 1200Hz
        FCB     $0A			; Min pulse width 1200Hz
        FCB     $12			; Max pulse width 1200Hz
        FDB     $DA5C			; Cassett motor delay
        FDB     $045E			; Keyboard debounce value
        FCB     $10			; Line printer comma width
        FCB     $74			; Line printer last comma field
        FCB     $84			; Line printer width
	
        FCB     $FF			; Auto line feed flag	
        FCB     $FF			; Alpha lock flag
        FCB     $01			; Printer EOL length
        FCB     $0D			; CR
        FCB     $0A			; LF
        FCC     / DNS/			; D.N.Smeed was here !
	endc

	endc
	
TextUpdateCurs:
LBBB5   DEC     <TextCursFalshCnt	; decrement blink delay
        BNE     LBBC5			; not time for cursor blink
	
        LDA     #$32			; re-initialize blink delay
        STA     <TextCursFalshCnt
        LDX     <TextVDUCursAddr	; get cursor address to X	
        LDA     ,X			; get character at cursor address

; The Dragon 32 and Dragon 64 in ROM mode, display a standard flashing black cusor.
; The Dragon 64 in RAM basic mode displays a flashing blue cusor as a visual clue you
; are in RAM mode.
	ifdef 	Dragon64ram
        CMPA    #$AF			; is it blue block?
	BEQ     LFAC7			; yes, flip bit to green block
	LDA     #$8F			; load with green block
LFAC7   EORA    #$20			; flip bit 5
	else
        EORA    #$40			; Dragon 32  Dragon 64 ROM mode, just invert bit 6
	endc
	
        STA     ,X			; save back in screen RAM
LBBC5   LDX     #$045E
LBBC8   LEAX    -1,X			; Small keyboard delay routine
        BNE     LBBC8			; Loops X times
        RTS				

; Dragon keyboard map
;
;       LSB              $FF02                    MSB
;        | PB0   PB1   PB2   PB3   PB4   PB5   PB6   PB7 <- column
;    ----|----------------------------------------------
;    PA0 |   0     1     2     3     4     5     6     7    LSB
;    PA1 |   8     9     :     ;     ,     -     .     /     $
;    PA2 |   @     A     B     C     D     E     F     G     F
;    PA3 |   H     I     J     K     L     M     N     O     F
;    PA4 |   P     Q     R     S     T     U     V     W     0
;    PA5 |   X     Y     Z    Up  Down  Left Right Space     0
;    PA6 | ENT   CLR   BRK   N/C   N/C   N/C   N/C  SHFT
;    PA7 - Comparator input                                 MSB
;     ^
;     |

LBBCD   LDB     PIA0DA			; Read Keyrows
        ORB     #$80			; Mask out joystick comparitor
        TST     PIA0DB			; Reading last row ?
        BMI     LBBD9			; No, skip
        ORB     #$40			; Make sure shift off
LBBD9   RTS				; return

; test for shift key down, returns with bit 6 of B clear if shift down
TestShiftKey   
	LDB     #$7F			; force all keyboard output lines except b7 high
        STB     PIA0DB			; this strobe's the column SHIFT is on
        LDB     PIA0DA			; read keyboard input lines
        ANDB    #$40			; mask out all but bit 6
        RTS

TextScanKbd:
LBBE5   PSHS    B,X			; save regs
        BSR     LBBEC
	
	ifdef	Dragon64
	JMP     >LBEE0			; If Dragon64, check for repeat
	else
        TSTA				; Check for keypress, flag it
        PULS    B,X,PC			; Restore and return
	endc
	
LBBEC   LEAS    -2,S			; Make room on stack
        LDX     #TextKbdRollover	; Point to keyboard rollover table		
        CLR     PIA0DB			; Force all columns low
        LDB     PIA0DA			; Check for any key down 
        ORB     #$80			; Mask out joystick comparitor input
        CMPB    ,X			; Any row changed from last scan ?
        BEQ     LBC6F			; No : restore and return
	
        TFR     B,A			; Save row mask
        COM     PIA0DB			; Reset all columns to high
        BSR     LBBCD			; Scan the row
	
        CMPB    #$FF			; Any keys down ?
        BNE     LBC6F			
	
        STA     ,X+			; Put keyrown in rollover table
        CLR     ,S			; Zero column count
        LDB     #$FE			; Start scanning first column
        STB     PIA0DB			; Output column
LBC11   BSR     LBBCD			; Scan the row

        STB     1,S			; save read row on stack
        EORB    ,X			; Set bits if keystate has changed
        ANDB    ,X			; IF a=0, then no new keys down, if <70 key released
        LDA     1,S			; Restore read row
        STA     ,X+			; Save in rollover table
        TSTB				; Key down ?
        BNE     LBC2A			; yes, debounce, and check
	
        INC     ,S			; increment column counter
        COMA				
        ROL     PIA0DB			; Move to next column
        BCS     LBC11			; Done all ?, nope go read next
        BRA     LBC6F			; Finished : exit

LBC2A   LDX     <TextKbdDelay		; Wait for key to debounce	
        BSR     LBBC8			; Keyboard delay
	
        EXG     A,B			; Save previously scanned row
        BSR     LBBCD			; Scan it again
	
        CMPB    1,S			; is it the same ?
        EXG     A,B			; Restore previously scanned row
        BNE     LBC6F
	
        LDA     ,S			; get bitmap of row we just scanned
        SUBA    #$08			; compensate for add below

; at this point a contains the column (-8) and B contains the row	
; we add 8 (as there are 8 keys per row) to the column whilst shifting the 
; row counter one place right until it is shifted into the carry flag.
; this way we end up with A containing :
; 00rrrccc where ccc is the column and rrr is the row
LBC3C   ADDA    #$08			; add 8 to column
        LSRB				; shift bitmap one bit right
        BCC     LBC3C			; loop if not zero
	
        TSTA				;  A=0, row 0, col 0
        BEQ     LBC76			; yep.....
	
        CMPA    #$0C			; row 1, column 4 (,)
        BCS     LBC5F			; branch if lower ?
	
        CMPA    #$11			; row 2 column 1, 'A'
        BCS     LBC74			; branch if lower
        CMPA    #$2A			; row 5 column 2, 'Z'
        BHI     LBC72			; branch if higher

; we get here if the key is a letter 'A'..'Z'
        ADDA    #$30			; convert keycode to ASCII
        BSR     TestShiftKey		; is shift key pressed?	
        BEQ     LBC68			; branch if so
	
        TST     TextCapsLock 		; is caps lock on ?
        BNE     LBC68			; branch if not
	
        ORA     #$20			; if so make it upper case / inverse
        BRA     LBC68			; jump ahead

; this section deals with the top row of the keyboard, number keys + ':',';'
LBC5F   ADDA    #$30			; convert to ASCII numbers + ':',';'
        LBSR    TestShiftKey		; is shift pressed?
        BNE     LBC68			; no, skip ahead	
        SUBA    #$10			; shift pressed, convert to symbols
	
LBC68   CMPA    #$12			; shift 0 pressed?
        BNE     LBC70			; no, skip ahead
        COM     TextCapsLock 		; yes, toggle caps lock flag
LBC6F   CLRA				; clear A, return no key for caps shift
LBC70   PULS    X,PC			; restore and return

; deal with up, down, left, right, space, enter clear and break 
LBC72   SUBA    #$1A
; deal with ',','-','/','@'
LBC74   SUBA    #$0B

LBC76   ASLA				; multiply by 2, 2 bytes per entry
        LBSR    TestShiftKey		; test if shift is down
        BNE     LBC7D			; nope
        INCA				; move to shifted code
LBC7D   LDX     #Keytable		; point at keytable
        LDA     A,X			; get code
        BRA     LBC68			; return it

; translation table row/column to keycode
;		unshifted,shifted		
Keytable   
	FCB     $30,$12			; '0',CAPS-lock
        FCB     $2C,$3C			; ',','<'
        FCB     $2D,$3D			; '-','='
        FCB     $2E,$3E			; '.','>'
        FCB     $2F,$3F			; '/','?'
        FCB     $40,$13			; '@',
        FCB     $5E,$5F			; '^','_'	
        FCB     $0A,$5B			; down arrow, 
        FCB     $08,$15			; left arrow, 
        FCB     $09,$5D			; right arrow,
        FCB     $20,$20			; space, space
        FCB     $0D,$0D			; enter, enter
        FCB     $0C,$5C			; clear, '\'
        FCB     $03,$03			; break, break

TextClearLine:
LBCA0   LDA     #$60			; vdg space
        STA     ,X+			; save it in screen memory
        TFR     X,D			; get X into D
        ANDB    #TextCharsLine-1	; mask out all but column
        BNE     TextClearLine 		; keep going if column <> 0
        RTS


TextVDUOut:
LBCAB   PSHS    D,X			; save regs
        LDX     <TextVDUCursAddr	; get cursor position in X
        CMPA    #BS			; is character backspace?
        BNE     LBCBE			; branch if not
	
        CMPX    #TextScreenBase		; are we at the beginning of the screen already?
        BEQ     LBCF3			; yes, do nothing, return
	
        LDA     #$60			; load VDG space char
        STA     ,-X			; put it on the screen & move pointer back one
        BRA     LBCDB			; skip ahead

LBCBE   CMPA    #CR			; is it carrige return?
        BNE     LBCC6			; branch if not
	
        BSR     TextClearLine 		; clear the rest of this line
        BRA     LBCDB			; save updated cursor pos and check for scroll

LBCC6   CMPA    #' '			; before space? (control character)
        BCS     LBCF3			; branch if so
	
        TSTA				; set flags
        BMI     LBCD9			; branch if semi-graphic character
	
        CMPA    #$40			; is it a number or spacial character?
        BCS     LBCD7			; branch if so
	
        CMPA    #$60			; Is it alphabetic uppercase?
        BCS     LBCD9			; branch if upper case
	
        ANDA    #$DF			; clear bit 5 force ASCII lower case to be upper
LBCD7   EORA    #$40			; invert bit 6 swap upper / lower case 
LBCD9   STA     ,X+			; save it in video RAM
LBCDB   STX     <TextVDUCursAddr	; save cursor address
	
        CMPX    #TextScreenLast		; are we on the last character of the screen?
        BLS     LBCF3			; no, don't scroll
	
        LDX     #TextScreenBase		; point X at the beginning of the screen RAM
LBCE5   LDD     $20,X			; get 2 bytes from next line down
        STD     ,X++			; save them in this line
        CMPX    #$05E0			; on the last line?
        BCS     LBCE5			; no keep copying
	
        STX     <TextVDUCursAddr	; update the cursor address	
        BSR     TextClearLine 		; clear the last line
LBCF3   PULS    D,X,PC			; restore and return

PrinterDirOut:
LBCF5   PSHS    B			; save B
	
	ifdef	Dragon64
        TST     TextPrnSelFlag 		; check printer select flag
        JMP     >LBEC5			; jump to main routine
	else
LBCF7   LDB     PIA1DB			; read PIA, get busy flag
        RORB				; move busy flag into carry
        BCS     LBCF7			; printer busy, keep waiting.....
	endc
	
LBCFD   STA     PIA0DB			; send byte to send out on PIA0 port B
        LDB     #$02			; printer strobe bit
        STB     PIA1DA			; assert strobe to printer
        CLR     PIA1DA			; clear strobe
        PULS    B,PC			; restore and return

PrinterCRLF:
LBD0A   LDX     #TextPrnEOLCnt 		; point at printer EOL sequence
        LDB     ,X+			; get byte count
LBD0F   

	ifndef	Dragon64ram
	TSTB				; test count
	endc
	
        BEQ     LBD19			; no bytes to send, exit
        LDA     ,X+			; get next EOL byte
        BSR     PrinterDirOut 		; send it

	ifdef	Dragon64
        JMP     >LBF0C			
	else
        DECB				; decrement byte count
        BRA     LBD0F			; loop again untill all done
	endc

LBD19   RTS

; Output a character to the printer
PrinterOut:
LBD1A   PSHS    D,X			; save registers
        CMPA    #CR			; carrige return?
        BEQ     LBD33			; yes, send EOL sequence
	
        CMPA    #$20			; space
        BCS     LBD26			; skip if below
        
	INC     <TextPrnCurrCol		; increment current printer column
LBD26   BSR     PrinterDirOut 		; send the character to the printer
        
	LDB     <TextPrnCurrCol		; get the current print column
        CMPB    <TextPrnLineW		; is it less than the line width?
        BCS     LBD3F			; branch if less
	
        TST     TextPrnAutoCRLF 	; test if we should do an automatic EOL?
        BNE     LBD3D			; no, exit				
	
LBD33   TST     <TextPrnCurrCol		; test current printer column
        BNE     LBD3B			; branch not zero
	
        LDA     #' '			; send a space
        BSR     PrinterDirOut 		
	
LBD3B   BSR     PrinterCRLF 		; send an EOL sequence
LBD3D   CLR     <TextPrnCurrCol		; reset current column
LBD3F   PULS    D,X,PC			; restore and return


; this routine will transfer bit 0 of B to sel 1 of the analog multiplexer 
; and bit 1 of B to sel 2.
; select the appropriate joystick
SysSelJoystick:
LBD41   LDU     #PIA0CRA		; point U at PIAs
        BSR     LBD46
LBD46   LDA     ,U			; get the control register
        ANDA    #$F7			; mask out current bit
        RORB				; get bit 0 of B into carry
        BCC     LBD4F			; carry clear, skip on, bit is zero
        ORA     #$08			; set bit in control register
LBD4F   STA     ,U++			; save it back and select side B control
        RTS

; this is a 6 bit software a/d conversion routine
SysReadJoystick:
LBD52   LEAS    -3,S			; make room on stack for temporary values
        LDX     #VectDevOpen 		; call RAM hook
        LDB     #$03			; get values for 3+1 joystick axies
	
LBD59   LDA     #$0A			; 10 retries to get a stable value
        STD     1,S			; store joystic axis number and try number on stack
        BSR     SysSelJoystick 		; select the joystick to read

; A is a shift counter, how many bits to convert and will be  $40 (6 bits).
; B contains a value equal to 1/2 the current trial difference, initially $80 (2.5V).
LBD5F   LDD     #$4080			
LBD62   STA     ,S			; store shift counter on stack	
        STB     PIA1DA			; send value to D/A converter
        TST     PIA0DA			; read result value, comparito output in bit 7
        BMI     LBD70			; branch if comparitor output is high
	
        SUBB    ,S			; subtract half the current trial difference
        BRA     LBD72			; branch ahead

LBD70   ADDB    ,S			; add half the current trial difference
LBD72   LSRA				; shift it right once
        CMPA    #$01			; have all the shifts been done?
        BNE     LBD62			; no go get next bit
        
	LSRB				; move data from top 6 bits of B
        LSRB				; to bottom 6 bits of B
        CMPB    -1,X			; is this value equal to last try?
        BEQ     LBD81			; yes, go save the value
        
	DEC     1,S			; else decrement the retry counter
        BNE     LBD5F			; branch if we havn't tried 10 times
	
; if you get here you have failed to read the same value twice after 10 times.
; as a result we just fall through and use the last read value
	
LBD81   STB     ,-X			; save the digitized value
        LDB     2,S			; get the current joystick axis number
        DECB				; decrement
        BPL     LBD59			; loop if still have axies to read
        PULS    A,X,PC			; restore and return

; read cassette input bit of the pia
ReadCasInBit   
	INC     <CasTemp		; increment the period timer
        LDB     PIA1DA			; read the cassette input bit from PIA
        RORB				; put the input bit in the carry flag
        RTS

; main timing loop
LBD91   CLR     <CasTemp		; reset period timer
        TST     <CasPhaseFlag		; check to see if it's synced in on the hi->low or lo->hi 
        BNE     LBD9E			; branch on hi->lo
	
; lo->high transition
LBD97   BSR     LBDA0			; wait until bit is low
LBD99   BSR     ReadCasInBit		; read cassette input bit
        BCC     LBD99			; loop until bit is high
        RTS

; hi->lo trasition
LBD9E   BSR     LBD99			; wait until bit is high
LBDA0   BSR     ReadCasInBit		; read cassette input bit
        BCS     LBDA0			; loop until bit is low
        RTS

; CasPartrt contains 18 initially and is used to determine if a bit read is a one or a zero
; if the period timer is < 18 the bit is considdered a one
; if the period timer is > 18 the bit is considdered a zero
CasBitIn:
LBDA5   BSR     LBD91			; get time between transitions
        LDB     <CasTemp		; get period timer
        DECB				; and decrement it
        CMPB    <CasPartrt		; see note above....
        RTS

; read a byte in from cassette
CasByteIn:
LBDAD   LDA     #$08			; 8 bits in a byte
        STA     <CasBitCount		; setup bit count
	
LBDB1   BSR     CasBitIn 		; read a bit from the cassette
        RORA				; rotate bit into A (from carry)
        DEC     <CasBitCount		; decrement bit count
        BNE     LBDB1			; loop until all bits done
        RTS

LBDB9   CLR     <CasTemp		; reset period timer
        BSR     LBDA0			; wait until bit goes low
        BRA     LBDC3			; skip ahead

LBDBF   CLR     <CasTemp		; reset period timer
        BSR     LBD99			; wait until bit is high
	
LBDC3   LDB     <CasTemp		; get period timer
        CMPB    <CasMax24		; upper limit for 1200Hz
        BHI     LBDCC			; branch if cassette not up to speed, or dropout
	
        CMPB    <CasMax12		; upper limit of 2400Hz
        RTS

LBDCC   CLR     <CasBitCount		; reset up to speed counter
        RTS

; turn cassette motor on, and delay for it to get up to speed
CasMotorOn:
LBDCF   LDA     PIA1CRA			; get the PIA control register
        ORA     #$08			; turn bit 3 on
        STA     PIA1CRA			; write to PIA
	
        LDX     <CasMotorDelay		; get cassette motor on delay
        LBRA    LBBC8			; go do the delay loop

; turn the cassette motor off
CasMotorOff:
LBDDC   LDA     PIA1CRA			; get PIA control register
        ANDA    #$F7			; mask off bit 3
        STA     PIA1CRA			; save it back to PIA
        ANDCC   #IntsEnable
        RTS

; look for the sync bytes 
; return with :
; A = $00 if sync'ed on hi - lo transition of the input signal from the cassette.
; A = $a0 if sync'ed on lo - hi transition of the input signal from the cassette.
CasReadLeader:
LBDE7   ORCC    #IntsDisable		; disable interrupts
        BSR     CasMotorOn 		; turn the cassete motor on, and wait for it to reach speed
        CLR     <CasBitCount		; clear bit counter
	
LBDED   BSR     LBD97			; wait for lo->hi transition
LBDEF   BSR     LBDB9			; wait for hi->lo transition
        BHI     LBDFF			; cassette speed in range for 1200Hz
	
LBDF3   BSR     LBDBF			; wait for lo->hi transition
        BCS     LBE03			; cassette speed in range for 2400Hz
        
	INC     <CasBitCount		; increment cassette bit count	
        LDA     <CasBitCount		; get cassette bit count
        CMPA    #$60			; have we had 96 bits yet?
        BRA     LBE0D			; skip ahead

LBDFF   BSR     LBDBF			; wait for lo->hi transition
        BHI     LBDEF			; loop back for next bit
	
LBE03   BSR     LBDB9			; loop back
        BCS     LBDF3
	
        DEC     <CasBitCount		; decrement bit count
        LDA     <CasBitCount		; get bit count
        ADDA    #$60			; make it +ve???
	
LBE0D   BNE     LBDED			; no wait for another transition
        STA     <CasPhaseFlag		; save phase flag
        RTS

* this routine sends the A reg to tape
CasByteOut:
LBE12   PSHS    A			; save output byte
        LDB     #$01			; B contains the bitmask that is used to determine
					; if the corresponding bit of A is set or not 

LBE16   LDY     #TapeSineTab		; point Y at sine wave table
        LDA     <CasLastSine		; get the value of the last sine
        STA     PIA1DA			; send sine value to D/A via PIA
	
        BITB    ,S			; test the bit in byte to send
        BNE     LBE30			; if it's a 1 do high frequency
	
; low frequency lookup
LBE23   LDA     ,Y+			; get next sine value
        CMPY    #EndTapeSineTab		; end of sine table?
        BCC     LBE3D			; yes, skip on
        STA     PIA1DA			; send sine value to D/A via PIA
        BRA     LBE23			; loop for next value

; high frequency lookup
LBE30   LDA     ,Y++			; get next sine value
        CMPY    #EndTapeSineTab		; end of sine table?
        BCC     LBE3D			; yes, skip on
        STA     PIA1DA			; send sine value to D/A via PIA
        BRA     LBE30			; loop for next value

LBE3D   STA     <CasLastSine		; save last sine value sent
        ASLB				; move on to next bit of byte to send
        BCC     LBE16			; carry will be set when byte is done,
					; else loop again for the next bit
        PULS    A,PC			; restore and return

;
; sinewave table for outputting FSK to tape.
;

TapeSineTab   
	FCB     $80,$90,$A8,$B8,$C8,$D8
        FCB     $E8,$F0,$F8,$F8,$F8,$F0
        FCB     $E8,$D8,$C8,$B8,$A8,$90
        FCB     $78,$68,$50,$40,$30,$20
        FCB     $10,$08,$00,$00,$00,$08
        FCB     $10,$20,$30,$40,$50,$68
EndTapeSineTab

; write leader to tape, a block of CasLeadCount leader bytes of $55
LBE68   PSHS    B,Y			; save regs
        ORCC    #IntsDisable		; disable interrupts
        LBSR    CasMotorOn 		; turn cassette motor on
	
        LDA     #SyncByte		; load SyncByte ($55)
        LDX     <CasLeadCount		; get number of sync bytes to write
	
LBE73   BSR     CasByteOut 		; write a byte
        LEAX    -1,X			; decrement count
        BNE     LBE73			; loop if more bytes to do
        
	PULS    B,Y,PC			; restore and return

	ifdef	Dragon64
	
; Dragon 64, read a byte from 6551 serial port.
; Asserts DTR, waits for data then clears DTR.
DoSerialIn   
	PSHS    CC,B			; save registers
        ORCC    #IntsDisable		; disable interrrupts
        LDA     #AciaSRxFull		; check for receiver register full (below)
        
	LDB     AciaCmd			; get command register
        ORB     #AciaDTR		; set DTR low
        STB     AciaCmd			; send it
	
        ANDB    #~AciaDTR		; set DTR bit high again
	
LBE8B   BITA    AciaStat		; test status of ACIA
        BEQ     LBE8B			; no data, keep waiting
	
        STB     AciaCmd			; set DTR digh again
        LDA     AciaData		; get the received data	
        PULS    CC,B,PC			; restore and return
	
; Dragon 64, write a byte to 6551 serial port.
; waits until transmit register is empty before sending byte	
DoSerialOut:
	PSHS    CC,B			; save regs
        LDB     #AciaSTxEmpty		; check for space in transmit register
LBE9C   BITB    AciaStat		; is there space?
        BEQ     LBE9C			; nop, loop until current byte transmitted
	
        STA     AciaData		; write data to be transmitted	
        PULS    CC,B,PC			; restore and return

; Dragon 64, set the baud rate of the 6551 serial port.
DoSetBaud
	CMPB    #$07			; check for a valid board rate number
        BCC     LBEBC			; error, exit
	
        LDX     #BaudRateTable		; point to base of baud rate table
        ABX				; move to correct entry
        LDB     TextSerBaudRate 	; get the baud rate to set
        ANDB    #~AciaBrdMask		; mask out baud rate bits
        ORB     ,X			; merge with rate from table
        STB     TextSerBaudRate 	; set the baud rate	
        ANDCC   #~FlagCarry		; clear carry
        BRA     LBEBD			; return
	
LBEBC   COMB				; flag error
LBEBD   RTS

;
; baud rate table, see 6551 datasheet for details.
;
; Note the 6551 is capable of other baud rates if programmed 
; directly
;
BaudRateTable   
	FCB     AciaCBrd110	; 110
        FCB     AciaCBrd300	; 300
        FCB     AciaCBrd600	; 600
        FCB     AciaCBrd1200	; 1200
        FCB     AciaCBrd2400	; 2400
        FCB     AciaCBrd4800	; 4800
        FCB     AciaCBrd9600	; 9600

LBEC5   BNE     LBEDB			; branch if serial printer
LBEC7   LDB     PIA1DB			; read PIA, get busy flag
        RORB				; move busy flag into carry
        BCS     LBEC7			; busy wait for it

; wait for all keyboard keys to be up
LBECD   CLR     PIA0DB			; clear all keyboard output lines
        LDB     PIA0DA			; read keyboard input lines
        ORB     #$80			; force bit 7 high, B will be 
					; $FF if no keypressed, due to input pullups
        INCB				; increment B, should now be zero if no key pressed
        BNE     LBECD			; if key down, wait till all keys released
        JMP     >LBCFD			; jump back to rest of printer output

; serial printer output
LBEDB   PULS    B			; restore saved B (pushed in main printer routine)
        JMP     >DoSerialOut		; just call serial output routine

; Keyboard auto-repeat code, Dragon64 RAM mode only, on entry A will contain the keypress (if any)
LBEE0   LDB     TextKbdRollover		; get last keyrow with key pressed
        INCB				; will be $FF with no key pressed, inc will make it $00
        BNE     LBEE9			; skip if not zero

        STB     KbdLSTKEY		; save in last key

LBEE9   TSTA				; was a key pressed?
        BEQ     LBEFD			; no, return
 
        LDB     KbdREPDLY		; get repeat dealy
        CMPA    KbdLSTKEY		; is this keypress the same as the last one?
        BEQ     LBEF7			; yes, count down to repeat
        ASLB				; B=B*8
        ASLB
        ASLB
	
LBEF7   STB     KbdCNTDWN		; save keycountdown
        STA     KbdLSTKEY		; save last key
LBEFD   PULS    B,X,PC			; restore and return

	ifndef	Dragon64ram
LBEFF   CLR     TextPrnSelFlag 		; clear printer selection, select parallel
        CLR     TextSerEOLDelay 	; clear serial EOL delay
        CLR     TextSerEOLDelay+1
        LDU     #TextPrnAutoCRLF 	; point at data table	
        RTS				; return to normal init code
	endc
	
	else
DoSerialIn   				; stub routine, unused in Dragon32
	RTS
	
DoSerialOut   				; stub routine, unused in Dragon32
	RTS

DoSetBaud   				; stub routine, unused in Dragon32
	COMB
        RTS
	endc
	
	ifdef	Dragon64
LBF0C   DECB				; decrement byte count
        BEQ     LBF12			; if at end of sequence do EOL delay
        JMP     >LBD0F			; loop for next byte

LBF12   LDY     TextSerEOLDelay 	; get EOL delay
        BEQ     LBF1F			; no delay, exit
LBF18   JSR     >LBBC5			; do small delay
        LEAY    -1,Y			; decrement repeat count
        BNE     LBF18			; loop again if repeat count not zero
LBF1F   RTS			

D64IRQ  LDB     AciaStat		; read serial status register
        BPL     LBF32			; branch if IRQ from ACIA 
        
	ANDB    #AciaSRxFull		; check for receive register full
        BEQ     LBF31			; branch if not full
	
        LDA     AciaCmd			; get ACIA command register
        ANDA    #~AciaDTR		; set DTR high
        STA     AciaCmd			; write it back
LBF31   RTI				; return from int

LBF32   LSR     TextKbdRollover		; shift first byte of rollover table
        DEC     KbdCNTDWN		; decrement repeat countdown
        BNE     LBF46			; branch if not repeat time yet....
	
        LDA     #$FF			; flag no keys pressed
        LDX     #TextKbdRollover	; point at rollover table
LBF3F   STA     ,X+			; save byte in table
        CMPX    #BasJoyVal0 		; end of table reached?
        BCS     LBF3F			; no keep going
LBF46   JMP     >BasIRQVec 		; jump to standard IRQ handler

	ifndef	Dragon64ram

; Boot routine used to boot the Dragon 64 into RAM basic mode.
; used in RAM mode to check that the version of the basic ROM that resides in RAM
; has not been corrupted....

ROMOffset	EQU	$4000		; offset between ROM and RAM copy of ROM

DoBoot64
	NOP				; First byte of reset routine must be NOP....
	ORCC    #IntsDisable		; disable interrupts
        LDX     #LBF5A			; point to routine below (source)
        LDU     #CasIOBuff 		; point to cassette buffer (destination)
        LDB     #(D64BootEnd-LBF5A)	; length to copy
        LBSR    LBB97			; go copy from X to U
        JMP     CasIOBuff 		; jump to copied code	

; the following code is copied into the cassete buffer and executed from there
LBF5A   LDA     Flag64			; check 64K mode flag
        CMPA    #FFlagTrue		; is it true?
        BNE     LBF77			; no, copy ROM->RAM
	
        STA     SAMSTY			; tell the SAM to select ram mode 1
        LDX     #D64RAMBase		; point to base of RAM copy of ROM
        LDD     #$0000			; initialize checksum
LBF6A   ADDD    ,X++			; add a word to the checksum, advance pointer
        CMPX    #D64RAMTop+1		; checksummed all area?
        BCS     LBF6A			; no, keep going
	
        CMPD    Checksum64		; is the checksum the same as the one calculated at boot time?
        BEQ     LBFE6			; yes, no need to re-copy
	
LBF77   STA     SAMCTY			; tell the SAM to select ram mode 0 
        LDA     PIA1CRB			; get control register of PIA1, port B
        ANDA    #~CRDDRDATA		; access DDR of port b
        STA     PIA1CRB			; update the PIA

        LDB     PIA1DB			; get DDR register
        ORB     #MaskROMSEL		; make ROMSEL bit an output
        STB     PIA1DB			; write DDR
        ORA     #CRDDRDATA		; access data register of port B
        STA     PIA1CRB			; tell PIA
	
        LDA     PIA1DB			; get data register of PIA
        ANDA    #~MaskROMSEL		; select second ROM, containing code to be copied to RAM	
        STA     PIA1DB			; write it to PIA
	
        LDU     >DBFF0			; point to end of ROM
        CMPU    #$3634			; check for '64' magic number
        BEQ     LBFA8			; branch if found
	
        ORA     #MaskROMSEL		; select normal ROM
        STA     PIA1DB			; write to PIA
        JMP     >BasFCError 		; generate FC error
	
LBFA8   CLR     Checksum64		; clear checksum
        CLR     Checksum64+1
	
        LDX     #RomBase		; point at base of ROM 
        LDY     #(RomBase+ROMOffset)	; point at destination address

LBFB5   LDD     ,X++			; get a word from ROM
        STA     SAMSTY			; tell the SAM to select ram mode 1
        STD     ,Y++			; put a word in RAM
        ADDD    Checksum64		; add word to checksum
        STD     Checksum64		; and save checksum
        CMPY    #IO			; reached base of IO area yet?
        BCC     LBFCD			; branch if so, copy done
	
        STA     SAMCTY			; tell the SAM to select ram mode 0
        BRA     LBFB5			; loop for next word

LBFCD   LDX     #DBFF0			; setup new ramtop
        STX     <AddrRamTop
        STX     <AddrFWareRamTop
        STX     <BasVarStrTop
	
        LEAX    -200,X			; setup stack, 200 below ramtop, for string space
        STX     <AddrStack
        TFR     X,S			; S=X
        LDX     #DoBoot64		; point reset vector at this routine
        STX     <IndVecReset
        JMP     (BasicHWInit+ROMOffset)	; jump to initialize copied ROM


LBFE6   JMP     (BasicSWInit+ROMOffset)	; jump to ROM if no new copy needeed
	endc

	else
	FDB     $0000
	endc
D64BootEnd
	
	ifdef	Dragon64ram
	FILL	$00,$FFF0-*		; Fill spare bytes with zeros
	else
	FILL	$00,$BFF0-*		; Fill spare bytes with zeros
	endc
	
; Interrupt vector table	
DBFF0	
	ifdef	Dragon64
	FDB	$3634			; '64', magic number used by RAM boot code 
	else
        FDB     $0000			; zero on Dragon 32	
	endc
        FDB     SecVecSWI3		; SoftWare Interrupt 3
        FDB     SecVecSWI2		; SoftWare Interrupt 2
        FDB     SecVecFIRQ		; Fast Interrupt ReQuest
        FDB     SecVecIRQ		; Interrupt ReQuest
        FDB     SecVecSWI		; SoftWare Interrupt (1)
        FDB     SecVecNMI		; Non Maskable Interrupt
	
	ifdef	Dragon64ram
	FDB	D64RAMBase		; Dragon 64 RAM version of Reset
	else
        FDB     SysReset		; normal Reset used otherwise
	endc
