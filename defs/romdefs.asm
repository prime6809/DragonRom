
StubResWordsOfs		EQU	$0000		Offset of number of reserved words
StubResLookupOfs	EQU	$0001		Offset of reserved word lookup table
StubResJumpOfs		EQU	$0003		Offset of reserved word jump table
StubFuncsOfs		EQU	$0005		Offset of nummber of functions
StubFuncsLookupOfs	EQU	$0006		Offset of function lookup table
StubFuncsJumpOfs	EQU	$0008		Offset of functions jump table

Skip1			EQU	$0021		Skip 1 byte (BRN)
Skip2			EQU	$008C		Skip 2 bytes (CMPX)
Skip1LD			EQU	$0086		Skip 1 byte (LDA)
Skip2TST		EQU	$007D		Skip 2 bytes (TST)
Skip2LDX		EQU	$008E		Skip 2 bytes (LDX)

CoCoVec167		EQU	$0000		Vector dest for 167 
CoCoVect16A		EQU	$0000		Vector dest for 16A
CoCoVect176		EQU	$0000		Vector dest for 176
CoCoVect179		EQU	$0000		Vector dest for 179
CoCoVect18B		EQU	$0000		Vector dest for 18B
CoCoVect191		EQU	$0000		Vector dest for 191
CoCoVect194		EQU	$0000		Vector Dest for 194
CoCoVect197		EQU	$0000		Vector Dest for 197
CoCoVect19A		EQU	$0000		Vector Dest for 19A
CoCoVect1A3		EQU	$0000		Vector Dest for 1A3
SerDLTimeout		EQU	$0000		Timeourt for DLOAD, unknown for Dragon
SerDLBaud		EQU	$0000		Baud rate for DLOAD, unknown for Dragon
BasBreakFlag		EQU	$0000		Break flag, +ve=stop,-ve=end
BasDelim1		EQU	$0001		First string delimiter
BasDelim2		EQU	$0002		Second string delimiter
BasGenCount		EQU	$0003		General count/scratch var
BasIfCount		EQU	$0004		If count - how many in a line
BasArrayEval		EQU	$0005		Array evaluation flag, 0=eval, 1=dimensioning
BasVarType		EQU	$0006		Variable type flag 0=numeric, $ff=string
BasGarbageFlag		EQU	$0007		Garbage collection flag
BasDisArraySearch	EQU	$0008		Disable array search flag, 0=allow 0<>disable
BasInputFlag		EQU	$0009		Iinput/read flag, 0=input 0<>read
BasRelateFlag		EQU	$000A		Relational operator flag
BasStrFirstFreeTemp	EQU	$000B		First free temory string space pointer
BasStrLastUsedTemp	EQU	$000D		Last used tempory string space pointer
BasTempPtr		EQU	$000F		Tempory pointer
BasTempPtr1		EQU	$0011		Tempory discriptor pointer (stack search)
BasTempFPA2		EQU	$0013		Tempory FPA Mantissa for FPA2
BasBotStack		EQU	$0017		Bottom of stack at last check
BasStartProg		EQU	$0019		Start addr of basic program
BasVarSimpleAddr	EQU	$001B		Start address of simple variables
BasVarArrayAddr		EQU	$001D		Start address of Array table
BasVarEnd		EQU	$001F		End of storage in use by basic
BasVarStringBase	EQU	$0021		Base address of string space (and stack)
AddrStack		EQU	$0021		Address of top of machine stack
BasVarStrTop		EQU	$0023		Top of string space in use
BasStrUtil		EQU	$0025		Utility string pointer
AddrFWareRamTop		EQU	$0027		Top of firmware RAM CLEAR xxx,yyyy set this to yyyy
BasContLine		EQU	$0029		Line no used by CONT
BasTempLine		EQU	$002B		Tempory line no
BasOldInputPtr		EQU	$002D		Pointer to saved input during a STOP
BasDirectTextPtr	EQU	$002F		Direct mode text pointer
BasVarDataLine		EQU	$0031		Line number of current data statement
BasVarDataAddr		EQU	$0033		Address of next item in data
TextKbdBuffAddr		EQU	$0035		Address of keyboard input buffer
BasVarLastInUse		EQU	$0037		Pointer to variable last in use
BasVarPtrLast		EQU	$0039		Poiinter to VARPTR last in use
BasTempVarDesc		EQU	$003B		Pointer to a tempory var descriptor
BasTempRelateFlag	EQU	$003F		Tempory relational operator flag
BasVarFPAcc3		EQU	$0040		Floating point accumulator 3 (packed)
BasVarFPAcc4		EQU	$0045		Floating point accumulator 4 (packed)
BasVarFPAcc5		EQU	$004A		Floating point accumulator 5 (packed)
BasVarFPAcc1		EQU	$004F		Floating point acumulator 1
BasVarAssign16		EQU	$0052		Part of FPA1, used for 16bit assigns
BasVarFPAcc2		EQU	$005C		Floating point acumulator 2
BasListLine		EQU	$0066		Current line during list
BasCurrentLine		EQU	$0068		Current line no $FFFF in direct mode
TextVDUCommaW		EQU	$006A		VDU comma width field
TextVDULastComma	EQU	$006B		VDU last comma field, should be VDU line width - VDU comma width
TextVDUCurrCol		EQU	$006C		Current column for VDU output
TextVDULineW		EQU	$006D		VDU line width, normally 32
CasIOFlag		EQU	$006E		Cassette IO Flag, set to $FF when IO in progress
TextDevN		EQU	$006F		Current device number
CasEOFFlag		EQU	$0070		Cassette IO Flag, nonzero if EOF reached
WarmStartFlag		EQU	$0071		Warm start flag $55=warm start, else cold start
IndVecReset		EQU	$0072		Secondary Reset vector address, must point to NOP
AddrRamTop		EQU	$0074		Physical end of RAM (4K, 16K, 32K or 64K).
BasUnused1		EQU	$0076		2 unused bytes
CasStatus		EQU	$0078		Cassette status byte, 0=cassette closed, 1=open for input, 2=open for output
CasIOBuffSize		EQU	$0079		Size of cassette IO buffer
CasHeadBuffAddr		EQU	$007A		Address of cassette file header
CasBlockType		EQU	$007C		Cassete block type, 0=filename, 1=data, 255=EOF
CasBlockLen		EQU	$007D		Cassete block length, number of bytes read, or to be written
CasIOBuffAddr		EQU	$007E		Cassette IO buffer address, where data will be read/written
CasCkSum		EQU	$0080		Used by cassette routines for calculating checksum
CasIOErrorCode		EQU	$0081		Cassette IO error code 0=no error, 1=CRC, 2=attempt to load in non-ram area
CasTemp			EQU	$0082		Cassette tempory storage
CasBitCount		EQU	$0083		Cassette bit counter
CasPhaseFlag		EQU	$0084		Cassette Phase flag
CasLastSine		EQU	$0085		Casette last sine tabe entry
GrSetResetData		EQU	$0086		Data for Lo-res set/reset
TextLastKey		EQU	$0087		ASCII code of last keypress, not cleard by key release
TextVDUCursAddr		EQU	$0088		Current VDU cursor address
Misc16BitScratch	EQU	$008A		Misc 16 bit scratch register (always zero ??)
SndPitch		EQU	$008C		Sound pitch value
SndLength		EQU	$008D		Sound duration
TextCursFalshCnt	EQU	$008F		Cusrsor flash counter
CasLeadCount		EQU	$0090		Cassete leader count, number of $55 bytes in the leader
CasPartrt		EQU	$0092		Cassette 1200/2400 partition
CasMax12		EQU	$0093		Cassette Upper limit of 1200Hz
CasMax24		EQU	$0094		Cassette Upper limit of 2400Hz
CasMotorDelay		EQU	$0095		Cassette motor on delay (also inter-block gap)
TextKbdDelay		EQU	$0097		Keyboard scan delay constant, used to debounce
TextPrnCommaW		EQU	$0099		Printer comma width
TextPrnLastComma	EQU	$009A		Printer last comma width, should be printer line width - prinnter comma width
TextPrnLineW		EQU	$009B		Printer line width
TextPrnCurrCol		EQU	$009C		Printer current column
BasExecAddr		EQU	$009D		Exec address, on D64, at startup points to routine to boot all ram mode
BasChrGet		EQU	$009F		Get next basic character routine
BasChrGetCurr		EQU	$00A5		Get current basic ccharacter
BasAddrSigByte		EQU	$00A6		Address of current significant bit in command line
BasRndData		EQU	$00AB		Used by RND
BasTronFlag		EQU	$00AF		Tron flag nonzero=trace on
BasUSRTableAddr		EQU	$00B0		Address of USR address table
GrForeground		EQU	$00B2		Current foreground colour
GrBackground		EQU	$00B3		Current background colour
GrColourTemp		EQU	$00B4		Tempory colour in use
GrCurrColour		EQU	$00B5		Byte value for current colour, to set all pixels in byte to that colour
GrCurrPmode		EQU	$00B6		Current PMODE number
GrLastDisplayAddr	EQU	$00B7		Address of last byte in current display
GrBytesPerLine		EQU	$00B9		Number of byts/lin in current mode
GrDisplayStartAddr	EQU	$00BA		Address of first byte in current display
GrStartPages		EQU	$00BC		Page number of Start of graphics pages
GrCurrX			EQU	$00BD		Current X cursor pos
GrCurrY			EQU	$00BF		Current Y cursor pos
GrColourSet		EQU	$00C1		Colour set currently in use
GrPlotFlag		EQU	$00C2		Plot/Unplot flag, 0=reset, nonzero=set
GrPixelNoX		EQU	$00C3		Current horizontal pixel no
GrPixelNoY		EQU	$00C5		Current vertical pixel number
GrCurrXCo		EQU	$00C7		Current Cursor X
GrCurrYCo		EQU	$00C9		Current Cursor Y
GrCircleXCo		EQU	$00CB		Circle command X
GrCircleYCo		EQU	$00CD		Circle command Y
BasRenumVal		EQU	$00CF		Renum increment value
GrCircleRadius		EQU	$00D0		Circle radius
BasRenumStart		EQU	$00D1		Renum start line no
BasCloadMOffs		EQU	$00D3		2s complement of CLOADM offset
BasRenumStartLine	EQU	$00D5		Renum start line number
BasEditorLineLen	EQU	$00D7		Editor line length
GrDirtyFlag		EQU	$00DB		Flag to tell if graphics screen has changed
SndOctave		EQU	$00DE		Sound octave value for PLAY
SndVolume		EQU	$00DF		Sound volume for PLAY
SndNoteLen		EQU	$00E1		Note length for PLAY
SndTempo		EQU	$00E2		Tempo for PLAY
SndTimerPlay		EQU	$00E3		Timer for the Play command
SndDotNoteScale		EQU	$00E5		Dotted note scale factor for Play
GrDrawAngle		EQU	$00E8		Current angle for DRAW command
GrDrawScale		EQU	$00E9		Current scale for DRAW command
SecVecSWI3		EQU	$0100		Secondary SWI3 vector JMP+ address
SecVecSWI2		EQU	$0103		Secondary SWI2 vector JMP+ address
SecVecSWI		EQU	$0106		Secondary NMI vector JMP+ address
SecVecNMI		EQU	$0109		Secondary NMI vector JMP+ address
SecVecIRQ		EQU	$010C		Secondary IRQ vector JMP+ address
SecVecFIRQ		EQU	$010F		Secondary FIRQ vector JMP+ address
SysTimeVal		EQU	$0112		Current value of system timer
BasRandomSeed		EQU	$0115		Random number seed for RND function
Flag64			EQU	$011A		Dragon 64 in RAM 64 k mode $FF=64K mode
Checksum64		EQU	$011B		Dragon 64 RAM copy of ROM checksum
KbdLSTKEY		EQU	$011D		Dragon 64, keyboard last key
KbdCNTDWN		EQU	$011E		Dragon 64, keybord repeat countdown
KbdREPDLY		EQU	$011F		Dragon 64, keybord repeat delay
BasNumCmds		EQU	$0120		Number of basic commands
BasStub0		EQU	$0120		Basic Stub 0 (All basic on Dragon, Colour basic on Tandy)
BasAddrCmdList		EQU	$0121		Address of basic command list
BasAddrCmdDisp		EQU	$0123		Address of basic command dispatch
BasNumFuncs		EQU	$0125		Number of basic functions
BasAddrFuncList		EQU	$0126		Address of basic function list
BasAddrFuncDisp		EQU	$0128		Address of basic function dispatcher
BasNumDskCmds		EQU	$012A		Number of disk basic commands
BasStub1		EQU	$012A		Basic stub 1 (Disk basic on Dragon, Extended basic on Tandy)
BasAddrDskCmdList	EQU	$012B		Address of disk basic command list
BasAddrDskCmdDisp	EQU	$012D		Address of disk basic command dispatch
BasNumDskFuncs		EQU	$012F		Number of disk basic functions
BasAddrDskFuncList	EQU	$0130		Address of disk basic function list
BasAddrDskFuncDisp	EQU	$0132		Address of disk basic function dispatcher
BasUsrVecNoDisk		EQU	$0134		USR vector tabl when basic not installed
BasStub2		EQU	$0134		Basic Stub 2 (Null on dragon, Disk basic on Tandy)
BasStub3		EQU	$013E		Basic Stub 3 (do not use on dragon, user stub on Tandy)
TextPrnAutoCRLF		EQU	$0148		Printer auto EOL flag, nonzero will send EOL sequence at end of line
TextCapsLock		EQU	$0149		Capslock flag, nonzero=uppercase
TextPrnEOLCnt		EQU	$014A		Number of characters in EOL sequence 1..4
TextPrnEOLSeq		EQU	$014B		End of line characters
TextKbdRollover		EQU	$0151		Rollover table, to check for key releases
BasJoyVal0		EQU	$015A		Joystick(0) value
BasJoyVal1		EQU	$015B		Joystick(1) value
BasJoyVal2		EQU	$015C		Joystick(2) value
BasJoyVal3		EQU	$015D		Joystick(3) value
VectDevOpen		EQU	$015E		Called before a device is opened
VectBase		EQU	$015E		Base address of ram hooks/vectors
VectDevNo		EQU	$0161		Called when a device number is verified
VectDevInit		EQU	$0164		Called before initialising a device
VectOutChar		EQU	$0167		Called before outputting char in A to a device
VectInChar		EQU	$016A		Called before inputting a char to A
VectInputFile		EQU	$016D		Called before inputting from a file
VectOutputFile		EQU	$0170		Called before outputting to a file
VectCloseAllFiles	EQU	$0173		Called before closing all files
VectCloseFile		EQU	$0176		Called before closing a file
VectCmdInterp		EQU	$0179		Called before interpreting a token in A
VectReReqestIn		EQU	$017C		Called before re-requesing input from keyboard
VectCheckKeys		EQU	$017F		Called before keyboard is scanned for BREAK,SHIFT-@
VectLineInputFile	EQU	$0182		Called before LINE INPUT is executed
VectCloseFileCmd	EQU	$0185		Called before closing an ASCII file read in as basic
VectCheckEOF		EQU	$0188		called before checking for end of file
VectEvaluateExpr	EQU	$018B		Called before evaluating expression
VectUserError		EQU	$018E		Can be patched by user to trap error messages
VectSysError		EQU	$0191		Can be patched by system to trap error messages
VectRunLink		EQU	$0194		Called when RUN about to be executed
VectResetBasMem		EQU	$0197		Called before changing BASIC memory vectors after LOAD etc
VectGetNextCmd		EQU	$019A		Called before fetching next command to be executed by BASIC
VectAssignStr		EQU	$019D		Called before assigning string to string variable
VectAccessScreen	EQU	$01A0		Called before CLS, GET & PUT are executed
VectTokenize		EQU	$01A3		Called before an ASCII line is tokenized
VectDeTokenize		EQU	$01A6		Called before a line is de-tokenized
BasStrDescStack		EQU	$01A9		String descriptor stack
CasFNameLen		EQU	$01D1		Length of cassette filename can be 0 to 8
CasFName		EQU	$01D2		Cassete filename to search for or write out
CasFNameFound		EQU	$01DA		Filename found, when reading
CasIOBuff		EQU	$01DA		COS default IO buffer, if this contains filename block then folloing are valid
CasFType		EQU	$01E2		File type 0=tokenized basic, 1=ASCII data, 2=Binary
CasASCIIFlag		EQU	$01E3		ASCII flag byte
CasGapFlag		EQU	$01E4		Gap flag byte
CasEntryAddr		EQU	$01E5		Entry address for MC programs
CasLoadAddr		EQU	$01E7		Load address
BasLinInpHead		EQU	$02DA		Basic line input buffer header
BasLinInpBuff		EQU	$02DC		Basic line input buffer
BasBuffer		EQU	$03D7		Basic buffer space
TextSerEOLDelay		EQU	$03FD		End of line delay for serial port on Dragon 64 & CoCo
TextPrnSelFlag		EQU	$03FF		Dragon 64 printer selection flag, 0=paralell port, nonzero=RS232

			ifdef	norom
BasicHWInit		EQU	$8000		Hardware initialisation
BasicSWInit		EQU	$8003		Software initialisation
BasicKbdIn		EQU	$8006		Keyboard input
BasicCursorB		EQU	$8009		Cursor blink
BasicScreenOut		EQU	$800C		Screen output
BasicPrintOut		EQU	$800F		Printer output
BasicJoyIn		EQU	$8012		Joystick input
BasicCassOn		EQU	$8015		Cassette player motor on
BasicCassOff		EQU	$8018		Cassette player motor off
BasicWriteLead		EQU	$801B		Cassette write leader
CasWriteLeader		EQU	$801B		Turn on motor and write out leader
BasicCassByOut		EQU	$801E		Cassette byte output
BasicCassOnRd		EQU	$8021		Cassette on for reading
BasicCassByIn		EQU	$8024		Cassette byte input
BasicCassBitIn		EQU	$8027		Cassette bit input
BasicSerIn		EQU	$802A		Read a byte from serial
BasicSerOut		EQU	$802D		Write a byte to serial port
BasicSetBaud		EQU	$8030		Set baud rate
BasErrorCodeTable	EQU	$82A9		List of 2 byte error codes eg 'SN' 'OM' 'UL' etc
BasChkArrSpaceMv	EQU	$831C		Check memory space at top of arrays + move arrays
BasChkB2Free		EQU	$8331		Check B*2 bytes free above Arrays, OM error if not
BasOMError		EQU	$8342		Print ?OM Error and return to basic
SysErr			EQU	$8344		Report error code in B register, cleanup and return to basic
SysErr2			EQU	$835E		Report error in B, do NOT hook to RAM, or turn of cas etc
BasCmdMode		EQU	$8371		Return to command mode
BasVect2		EQU	$83ED		Finalises setup of basic vectors (after load), should be preceeded by call to BasVect1
BasFindLineNo		EQU	$83FF		Find a line number in basic program
CmdNew			EQU	$8415		Basic Command
BasNew			EQU	$8417		Remove current basic program from meory, like NEW command
BasVect1		EQU	$841F		Sets up various basic vectors (after load), should be followed by call to BasVect2
BasResetStack		EQU	$8434		Reset basic stack to initial position
CmdFor			EQU	$8448		Basic Command
BasRun			EQU	$849F		Run basic program in memory, like RUN
BasBRARun		EQU	$84DA		BRA to main loop, used by DOS
BasDoDipatch		EQU	$84ED		Do command dispatech, X must point to dispatch table
CmdRestore		EQU	$8514		Basic Command
BasPollKeyboard		EQU	$851B		Basic, poll keyboard and check for break
TextWaitKey		EQU	$852B		Wait for a keypress, calls TextScanKbd, also handles break
CmdEnd			EQU	$8532		Basic Command
CmdStop			EQU	$8539		Basic Command
CmdCont			EQU	$8560		Basic Command
CmdClear		EQU	$8571		Basic Command
CmdRun			EQU	$85A5		Basic Command
CmdGo			EQU	$85B9		Basic Command
BasSkipLineNo		EQU	$85E7		Skip past line no in basic line, UL error if no line no.
BasSetProgPtrX		EQU	$85EE		Sets basic program pointer to X-1
CmdReturn		EQU	$85F3		Basic Command
CmdData			EQU	$8613		Basic Command
CmdREM			EQU	$8616		Basic Command
CmdIF			EQU	$8647		Basic Command
CmdON			EQU	$8675		Basic Command
BasGetLineNo		EQU	$869A		Get line no and store in BasTempLine
CmdLet			EQU	$86BC		Basic Command
CmdInput		EQU	$872B		Basic Command
CmdRead			EQU	$8777		Basic Command
CmdReadFromX		EQU	$877A		As basic READ command but ptr in X supplied by caller
CmdNext			EQU	$8829		Basic Command
VarGetExprCC		EQU	$8874		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry clear)
VarGetExpr		EQU	$8877		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry set)
BasTMError		EQU	$8882		Print ?TM Error and return to basic
VarGetStr		EQU	$8887		Compiles string and moves to free string space, should be followed by VarGetExpr
VarCKClBrac		EQU	$89A4		Check for Close bracket ')' in command line, SNError if not
VarCKOpBrac		EQU	$89A7		Check for Open bracket '(' in command line, SNError if not
VarCKComma		EQU	$89AA		Check for Comma in command line, SNError if not
VarCKChar		EQU	$89AC		Check for char in B register in command line, SNError if not
BasSNError		EQU	$89B4		Print ?SN Error and return to basic
CmdOR			EQU	$8A11		Basic Command
CmdAND			EQU	$8A12		Basic Command
CmdDim			EQU	$8A8B		Basic Command
VarGetVar		EQU	$8A94		Gets VARPTR address of following name and places in BasVarPtrLast
VarGetUsr		EQU	$8B29		Returns argument to USRnn as a 16bit no in D
BasFCError		EQU	$8B8D		Print ?FC Error and return to basic
CmdMEM			EQU	$8C31		Basic Command
VarAssign16Bit		EQU	$8C35		Assigns value in D register to a variable, and returns to basic
VarAssign8Bit		EQU	$8C36		Assigns value in B register to a variable, and returns to basic
VarAssign16Bit2		EQU	$8C37		Assigns value in D register to a variable, and returns to basic (1 less instruction!).
CmdSTRS			EQU	$8C40		Basic Command
BasResStr		EQU	$8C52		Reserve B bytes of string space return start in X, setup low mem vars
BasResStr2		EQU	$8CB3		Reserve B bytes of string space return start in X
VarGarbageCollect	EQU	$8CD7		Forces garbage collection in string space
BasGetStrLenAddr	EQU	$8D9A		Get string len in B and address in X of string desc in FPA2
VarDelVar		EQU	$8D9F		Frees up storage used by a variable
CmdLEN			EQU	$8DC7		Basic Command
CmdCHRS			EQU	$8DD2		Basic Command
CmdASC			EQU	$8DE6		Basic Command
BasGetStrFirst		EQU	$8DEA		Get first character of string into B
CmdLeftS		EQU	$8DF1		Basic Command
CmdRightS		EQU	$8E0E		Basic Command
CmdMidS			EQU	$8E15		Basic Command
VarGet8Bit		EQU	$8E51		Returns value of variable in B,FCError if more than 8 bits
CmdVAL			EQU	$8E5C		Basic Command
VarGetComma8		EQU	$8E7E		Checks for comman then gets 8 bit.
VarGet16Bit		EQU	$8E83		Returns value of variable in D,FCError if more than 16 bits
CmdPeek			EQU	$8E96		Basic Command
CmdPoke			EQU	$8E9D		Basic Command
CmdLList		EQU	$8EA4		Basic Command
CmdList			EQU	$8EAA		Basic Command
BasList			EQU	$8EAA		List basic program to SysDevN A must be 0 on entry
CmdPrint		EQU	$903D		Basic Command
TextOutCRLF		EQU	$90A1		Outputs an EOL sequence to the screen
TextOutString		EQU	$90E5		Outputs string pointed to by X to screen, X should point to byte before first byte of string
TextOutSpace		EQU	$90F5		Outputs a space to screen
TextOutQuestion		EQU	$90F8		Outputs a question mark to screen
CmdMinus		EQU	$9105		Basic Command
CmdPlus			EQU	$910E		Basic Command
VarNormFPA0		EQU	$9165		Normalize FPA0
CmdLOG			EQU	$923C		Basic Command
CmdMultiply		EQU	$9275		Basic Command
CmdDivide		EQU	$933C		Basic Command
CmdSGN			EQU	$9425		Basic Command
CmdABS			EQU	$943E		Basic Command
CmdINT			EQU	$9499		Basic Command
TextOutNum16		EQU	$957A		Outputs unsigned integer in D to the TextDevN device
TextOutNumFPA0		EQU	$9582		Outputs number in FPA0 to screen
CmdSQR			EQU	$9697		Basic Command
CmdExponet		EQU	$96A0		Basic Command
CmdEXP			EQU	$9713		Basic Command
CmdRND			EQU	$9772		Basic Command
BasRandom8		EQU	$978E		Generate an 8 bit random number and place in BasRandomSeed+1
CmdCOS			EQU	$97CB		Basic Command
CmdSIN			EQU	$97D1		Basic Command
CmdTAN			EQU	$9816		Basic Command
CmdATN			EQU	$9877		Basic Command
CasWriteBin		EQU	$991B		Write a binary file out push return address, then start,end and entry addresses and then JMP to this
CmdFIX			EQU	$9956		Basic Command
CmdEdit			EQU	$9965		Basic Command
CmdTron			EQU	$9AD9		Basic Command
CmdTroff		EQU	$9ADA		Basic Command
CmdPOS			EQU	$9ADE		Basic Command
CmdVarptr		EQU	$9AF4		Basic Command
CmdStringS		EQU	$9B84		Basic Command
CmdInstr		EQU	$9BB4		Basic Command
VarAssign16BitB		EQU	$9C3E		Assigns value in BasVarAssign16 to a variable, and returns to basic
BasChkDirect		EQU	$9C76		Check for direct mode, ID Error if so
CmdDef			EQU	$9C81		Basic Command
CmdUSR			EQU	$9D1D		Basic Command
BasIRQVec		EQU	$9D3D		Basic IRQ routine, increments timer
CmdTimer		EQU	$9D59		Basic Command
CmdDelete		EQU	$9D61		Basic Command
CmdLineInput		EQU	$9DB1		Line input command
BasLineInputEntry	EQU	$9DD9		Entry into LINE INPUT routine, used by DOS
CmdRenum		EQU	$9DFA		Basic Command
IndKeyInput		EQU	$A000		Indirect keyboard input jsr()
IndCharOutput		EQU	$A002		Indirect Character output
IndCasOnRead		EQU	$A004		Indirect prepare cassette for read
IndCasBlockIn		EQU	$A006		Indirect Read cassette block
IndCasBlockOut		EQU	$A008		Indirect Write cassete block
IndJoystickIn		EQU	$A00A		Indirect joystick in
IndCasWriteLead		EQU	$A00C		Indirect Write cassette leader
CmdHexS			EQU	$A00E		Basic Command
CmdDload		EQU	$A049		Basic Command
TextWaitKeyCurs		EQU	$A0EA		Same as TextWaitKey, but with cursor
PixMaskTable2Col	EQU	$A66B		Pixel mask table 2 colour mode
PixMaskTable4Col	EQU	$A673		Pixel mask table 4 colour mode
CmdPPoint		EQU	$A6C7		Basic Command
CmdPset			EQU	$A6EF		Basic Command
CmdPReset		EQU	$A6F3		Basic Command
CmdLine			EQU	$A749		Basic Command
CmdPCls			EQU	$A8C0		Basic Command
GrClearGrScreen		EQU	$A8C7		Clears grapics screen to value in B
CmdColor		EQU	$A8D4		Basic Command
GrSetColours		EQU	$A928		Sets up colours in low memory
GrSelectDisplay		EQU	$A938		Sets Text or Graphics screen, if Z=1 then text
TextResetVDU		EQU	$A93A		Resets to text mode and screen base address of $400
GrSetVDGMode		EQU	$A989		Set VDG to mode in A register
GrSetVDGOffset		EQU	$A99D		Set VDG offset to page in A
GrSelectVDGColSet	EQU	$A9A4		Select colour set from data in GrColourSet
CmdPmode		EQU	$A9AF		Basic Command
GrSelectPage		EQU	$A9E1		On entry B contains Pmode page to be used
CmdScreen		EQU	$A9FE		Basic Command
GrSelectColourSet	EQU	$AA10		Selects colour set dependent on B
CmdPClear		EQU	$AA19		Basic Command
GrReserveGrRam		EQU	$AA23		Reserves memory for graphics, no graphics pages in B
BasLocateScreen		EQU	$AA87		Initialise beginning of basic after graphics screen, no of pages in A
CmdPcopy		EQU	$AABE		Basic Command
CmdGet			EQU	$AAF0		Basic Command
CmdPut			EQU	$AAF3		Basic Command
CmdPaint		EQU	$AC87		Basic Command
CmdPlay			EQU	$ADBD		Basic Command
SndPlayNote		EQU	$AE9A		Plays a note from the A register (ASCII)
GrDraw			EQU	$B051		Draw on pmode screen as in DRAW command
CmdDraw			EQU	$B051		Basic Command
CmdCircle		EQU	$B238		Basic Command
SysReset		EQU	$B3B4		Perform soft reset, as if reset button pressed
BasBootBasic		EQU	$B400		Restart basic, as if power on, also deletes current program
WarmStart		EQU	$B44F		Warm start routine
BasSignonMess		EQU	$B4B2		Signon message address, for CoCo this is for Extended basic.
TextWaitKeyCurs2	EQU	$B505		Same as TextWaitKey, but with cursor
TextOutChar		EQU	$B54A		Outputs character in A to screen
BasInBuffFromX		EQU	$B5D3		Read input buffer at X as basic input
CmdClose		EQU	$B64D		Basic Command
CasClosFiles		EQU	$B65F		Close any open cassete file
CmdCsave		EQU	$B683		Basic Command
CasWriteBasic		EQU	$B6A5		Write tokenized basic program out, similar to CSAVE
CmdCload		EQU	$B6D5		Basic Command
CasReadBin		EQU	$B748		Read in a binary file, similar to CLOADM
CmdExec			EQU	$B771		Basic Command
CmdInkeyS		EQU	$B797		Basic Command
UtilCopyBXtoU		EQU	$B7CC		Copy B bytes from X to U
BasGetDevNo		EQU	$B7D4		Get dev no from line & validate
CmdEOF			EQU	$B801		Basic Command
CmdSkipf		EQU	$B81F		Basic Command
CmdOpen			EQU	$B829		Basic Command
CmdOpenEntry		EQU	$B835		Entry into Basic open command used by Dragon/SuperDos
BasFMError		EQU	$B848		Print ?FM Error and return to basic
CasFindFile		EQU	$B8B3		Searches a tape for specified filename
CasReadBlock1		EQU	$B933		Turns on motor, reads header and then first block into CasIOBufAddr
CasBlockIn		EQU	$B93E		Reads a block into the cassete buffer pointed to by CasIOBuffAddr
CmdMotor		EQU	$B982		Basic Command
CasWriteBlock1		EQU	$B991		Turn on motor, write leader and then first block
CasBlockOut		EQU	$B999		Write a block to cassete pointed to by CasIOBuffAddr
CmdSet			EQU	$B9D3		Basic Command
GrSetLRGPixel		EQU	$B9DF		Sets lo res pixel
CmdReset		EQU	$BA04		Basic Command
GrResetLRGPixel		EQU	$BA07		ReSets lo res pixel
GrCalcPixelPos		EQU	$BA28		Calculates Lo-res pixel pos from data on stack
CmdPoint		EQU	$BA45		Basic Command
CmdCLS			EQU	$BA60		Basic Command
TextCls			EQU	$BA77		Clear text mode screen, resets cursor to top left
TextClsChar		EQU	$BA79		Clears srcrren to character in B register & resets cursor
CmdSound		EQU	$BA9B		Basic Command
SndBeep			EQU	$BAA0		Play a beep duration in B, frequency in SndPitch
SndDisable		EQU	$BAC3		Disables D/A sound output
CasAudioOff		EQU	$BAC3		Turn off audio from cassette
SndEnable		EQU	$BAC5		Enables D/A sound output
SysResetDA		EQU	$BAD4		Reset D/A converter to $7E
SysWriteDA		EQU	$BAD6		Write value in A to D/A, bits 0 &1 should be 0
CmdAudio		EQU	$BADF		Basic Command
CasAudioOn		EQU	$BAEC		Turn on Audio from cassete to speaker
SndDTOAOn		EQU	$BAED		Turn on audio to D/A converter
CmdJoystk		EQU	$BB0D		Basic Command
SysBoot64		EQU	$BB80		Dragon 64 only, boots basic into all ram mode, with 48K available to basic.
TextUpdateCurs		EQU	$BBB5		Decrements TextCursFlashCnt, if zero resets and flashes cursor
TextScanKbd		EQU	$BBE5		Scan keyboard, return Char in A, Zero flag set if no key
TextClearLine		EQU	$BCA0		Clears a VDU line from current cursor pos to EOL
TextVDUOut		EQU	$BCAB		Outputs Char in A to VDU, does not reset screen.
PrinterDirOut		EQU	$BCF5		Sends character in A register to printer (uncooked)
PrinterCRLF		EQU	$BD0A		Moves printer head to next line.
PrinterOut		EQU	$BD1A		Sends character in A register to printer
SysSelJoystick		EQU	$BD41		Select joystick alue to read from A
SysReadJoystick		EQU	$BD52		Read hardware joystick values & update BasJoyVal0..3
CasBitIn		EQU	$BDA5		Reads a bity into the 'Z' flag
CasByteIn		EQU	$BDAD		Reads a single byte into the A register
CasMotorOn		EQU	$BDCF		Turn on motor, and wait for delay in CasMotorDelay
CasMotorOff		EQU	$BDDC		Turn off cassette motor
CasReadLeader		EQU	$BDE7		Turn on motor and read past leader
CasByteOut		EQU	$BE12		Write byte in A register to cassete
			ENDC

DBZero		EQU	Misc16BitScratch ; this is always set to Zero.
			
TextSerBaudRate	EQU	$FF07		Serial baud rate, note on Dragon 64, this is the actual hardware baud rate reg.

StackBuf	equ	$3A		; stack buffer size
RelPTR		equ	$03D		; Tempory arithmetic/logical table ptr
LineBufMax	equ	250		; maximum line buffer length

; For compatibility with CoCo ROM listings
; first floating point accumulator
FP0EXP		equ	BasVarFPAcc1	; exponent
FPA0		equ	BasVarFPAcc1+1	; mantissa
FP0SGN		equ	BasVarFPAcc1+5	; sign
COEFCT		equ	BasVarFPAcc1+6	; polynomial coeficient counter
StrDesc		equ	BasVarFPAcc1+7	; tempory string descriptor (5 bytes)

; second floating point accumulator
FP1EXP		equ	BasVarFPAcc2	; exponent
FPA1		equ	BasVarFPAcc2+1	; mantissa
FP1SGN		equ	BasVarFPAcc2+5	; sign
ResSGN		equ	BasVarFPAcc2+6	; sign of result of FP operation
FPSByte		equ	BasVarFPAcc2+7	; floating point sub byte
CoefPTR		equ	BasVarFPAcc2+8	; Coeficient pointer

FPCARY		equ	$005B	

FPA2		equ	BasTempFPA2	; FPA2

VarAB		equ	$00AB		; temp vars / RND
VarAC		equ	$00AC		; temp vars / RND
VarAD		equ	$00AD		; temp vars / RND
VarAE		equ	$00AE		; temp vars / RND

;
; Device numbers
;

DevConsole	equ 	0		; console
DevCasette	equ 	-1		; cassette
DevPrinter	equ 	-2		; printer
	
;
; Cassette file types
;
SyncByte	equ 	$55 		; Sync byte in cassete files
BlockBegin	equ 	$3C		; Begining of block marker

;Block Types
BtFileName	equ 	$00		; File name block
BtData		equ 	$01		; Data block
BtEOF		equ 	$FF		; End of file block

FNameBlockLen   equ 	15   		; 15 bytes in header block
CasFilenameLen	equ 	8		; Cassette filename length
DefBlockSize    equ 	250  		; Default block size

;File Types, as stored in filename block
FtBasic		equ 	$00		; Basic program
FtDataFile	equ 	$01		; Data file
FtMachineCode	equ 	$02		; Machine code program
FtBinary	equ 	$03		; Binary file
FtDream         equ	$88	  	; Dream Assembler source file
FtHeaderless    equ 	$FF	  	; Headerless

;Ascii/Binary flag from filename block
AsAscii		equ 	$FF		; ASCII file
AsBinary	equ 	$00		; Binary file (tokenised basic)

; Gap Flag from filename block
GfUngapped	equ 	$00		; No gaps
GfGapped	equ 	$FF		; Gaps between blocks

; Cassette file IO types
CasInputFile	equ 	1		; input file
CasOutputFile	equ 	2		; output file

;
; Screen metrics.
;
GrMaxX		equ 	255		; Maximum X co-ordinate
GrMaxY		equ 	191		; Maximum Y co-ordinate
GrMaxColour	equ 	8		; maximum colour number
GrMaxPmode	equ 	4		; maximum pmode	
GrMaxColourSet	equ 	1		; maximum colour set	
GrMaxPages	equ 	8		; maximum PCLEAR pages
GrPageSize	equ 	$600		; Graphic (pclear) page size in bytes

GrStartPage	equ 	$06		; default start page for graphics memory $0600
BasStartPage	equ 	$1E		; default start page for basic program $1E00

TextScreenBase	equ 	$0400		; base of text screen
GrScreenBase	equ 	$0600		; base of graphics pages
TextScreenLen	equ 	$0200		; Length of text screen
TextScreenLast	equ 	(TextScreenBase+TextScreenLen)-1	; last character of text screen

;
; Lo-res colour masks
;

MaskGreen	equ 	$00		; Green
MaskYellow	equ	$10		; Yellow
MaskBlue	equ	$20		; Blue
MaskRed		equ 	$30		; red
MaskBuff	equ	$40		; buff / white
MaskCyan	equ	$50		; cyan
MaskMagenta	equ	$60		; magenta
MaskOrange	equ	$70		; orange

LRGColourDiff	equ	$10		; difference between colours	

;
; Lo-res pixel masks
;
MaskUpR		equ	$08		; upper right
MaskUpL		equ	$04		; upper left
MaskLowR	equ	$02		; lower right
MaskLowL	equ	$01		; lower left
MaskAllOn	equ	(MaskUpR+MaskUpL+MaskLowR+MaskLowL)	; all on
MaskAllOff	equ	$00		; all off

MaskLRG		equ	$80		; low res graphics

; 
; Lo-res (semigraphics) and text screen metrics.
;

LoMaxX		equ 	63		; Maximum lo-res X co-ordinate
LoMaxY		equ 	31		; Maximum lo-res Y co-ordinate
LoMaxColour	equ 	8		; max lo-res colour
TextCharsLine	equ 	32		; Text/Lo-res character cells per line

;
; Cartridge entry points
;

CartBase	equ	$c000		; cartridge area base
CartEntryFIRQ	equ	CartBase	; entry point when FIRQ generated
CartEntryDOS	equ	CartBase+2	; entry point when dos flag found 'DK' at $C000
CartDOSFlag	equ	$444B		; Dos flag word : 'DK' 

;
; Dragon 64 RAM basic stuff
;

D64RAMBase	equ	$C000		; Dragon 64 RAM basic starts at $c000
D64RAMTop	equ	$FEFF		; Dragon 64 RAM ends here 

;
; Firmware flag used by warm start and RAM basic boot
;
FFlagTrue	equ	$55		; flag initialised / true
NOPFlag		equ	$12		; op-code fro NOP, used to mark start of reset vector