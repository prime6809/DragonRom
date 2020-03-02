*
* Deinitions for ports on Dragon 32/64/Alpha.
*
* 2004-11-16. P.Harvey-Smith.
* 	Fixed the stupid error I made in the defines below
*	that made all the non DPxxxxx defines equal to FF00 !!!
*
* 2004-10-10. P.Harvey-Smith.
*	Tidyed up a little, moved romdefs into their own file.
*

IO		equ		$ff00		; IO page on Dragon

*
* Most of these symbols will be defined twice, as some 
* of the Dragon code, sets DP=$FF, and uses direct page
* addressing to access the io ports, whilst some of it
* uses absolute addressing.
* The versions starting DP must be used with DP=$FF.
*

*Pia 0 and 1 standard on all Dragons.

DPPIA0DA	EQU		$00		; Side A Data/DDR
DPPIA0CRA	EQU		$01		; Side A Control.
DPPIA0DB	EQU		$02		; Side B Data/DDR
DPPIA0CRB	EQU		$03		; Side B Control.

PIA0DA		EQU		DPPIA0DA+IO	; Side A Data/DDR
PIA0CRA		EQU		DPPIA0CRA+IO	; Side A Control.
PIA0DB		EQU		DPPIA0DB+IO	; Side A Data/DDR
PIA0CRB		EQU		DPPIA0CRB+IO	; Side A Control.

DPPIA1DA	EQU		$20		; Side A Data/DDR
DPPIA1CRA	EQU		$21		; Side A Control.
DPPIA1DB	EQU		$22		; Side B Data/DDR
DPPIA1CRB	EQU		$23		; Side B Control.

PIA1DA		EQU		DPPIA1DA+IO	; Side A Data/DDR
PIA1CRA		EQU		DPPIA1CRA+IO	; Side A Control.
PIA1DB		EQU		DPPIA1DB+IO	; Side A Data/DDR
PIA1CRB		EQU		DPPIA1CRB+IO	; Side A Control.

* Dragon Alpha has a third PIA at FF24.

DPPIA2DA	EQU		$24		; Side A Data/DDR
DPPIA2CRA	EQU		$25		; Side A Control.
DPPIA2DB	EQU		$26		; Side B Data/DDR
DPPIA2CRB	EQU		$27		; Side B Control.

PIA2DA		EQU		DPPIA2DA+IO	; Side A Data/DDR
PIA2CRA		EQU		DPPIA2CRA+IO	; Side A Control.
PIA2DB		EQU		DPPIA2DB+IO	; Side A Data/DDR
PIA2CRB		EQU		DPPIA2CRB+IO	; Side A Control.

;WD2797 Floppy disk controler, used in Alpha Note registers in reverse order !
DPCmdRegA	EQU		$2F		; command/status			
DPTrkRegA	EQU		$2E		; Track register
DPSecRegA	EQU		$2D		; Sector register
DPDataRegA	EQU		$2C		; Data register

CmdRegA		EQU		DPCMDREGA+IO	; command/status			
TrkRegA		EQU		DPTRKREGA+IO	; Track register
SecRegA		EQU		DPSECREGA+IO	; Sector register
DataRegA	EQU		DPDATAREGA+IO	; Data register

; Constants for Alpha AY-8912 sound chip, which is used to control
; Drive select and motor on the Alpha

AYIOREG		EQU		$0E		; AY-8912, IO Register number.
AYIdle		EQU		$00		; Make AY Idle.
AYWriteReg	EQU		$01		; Write AY Register
AYReadReg	EQU		$02		; Read AY Register
AYREGLatch	EQU		$03		; Latch register into AY

DSMask		EQU		$03		; Drive select mask.
MotorMask	EQU		$04		; Motor enable mask
DDENMask	EQU		$08		; DDEN Mask
ENPMask		EQU		$10		; Enable Precomp mask
NMIMask		EQU		$20		; NMI enable Mask

; Dragon 64/Alpha Serial port.
DPAciaData	EQU		$04		; Acia Rx/Tx Register
DPAciaStat	EQU		$05		; Acia status register
DPAciaCmd	EQU		$06		; Acia command register
DPAciaCtrl	EQU		$07		; Acia control register

AciaData	EQU		DPAciaData+IO	; Acia Rx/Tx Register
AciaStat	EQU		DPAciaStat+IO	; Acia status register
AciaCmd		EQU		DPAciaCmd+IO	; Acia command register
AciaCtrl	EQU		DPAciaCtrl+IO	; Acia control register

; constants for the ACIA status register

AciaSParityErr	EQU		$01		; parity error
AciaSFrameErr	EQU		$02		; framing error
AciaSOverrun	EQU		$04		; data overrun
AciaSRxFull	EQU		$08		; receiver register full
AciaSTxEmpty	EQU		$10		; transmit register empty
AciaSDCD	EQU		$20		; data carrier detect =0detect, 1=not detect
AciaSDSR	EQU		$40		; data set ready 0=ready, 1=not ready
AciaSIRQ	EQU		$80		; interrupt, 0=no int, 1=int

; constants for ACIA command register
AciaPMCMask	EQU		$C0		; mask for PMC bits
AciaTICMask	EQU		$0C		; transmitter interrupt control mask

AciaPMCPOdd	EQU		$00		; odd parity tx/rx
AciaPMCPEven	EQU		$40		; even parity tx/rx
AciaPMCPMark	EQU		$80		; mark parity tx, rx parity disabled
AciaPMCPSpace	EQU		$C0		; space parity tx, rx parity disabled
AciaPME		EQU		$20		; parity mode enable = 1, disable = 0
AciaREM		EQU		$10		; receiver echo mode, 0=no echo, 1=echo
AcidTICRHDis	EQU		$00		; RTS high, interrupt disabled
AcidTICRLEna	EQU		$04		; RTS low, interrupt enabled
AcidTICRLDis	EQU		$08		; RTS low, interrupt disabled
AcidTICRLBrk	EQU		$0C		; RTS low, interrupt disabled, break on txd
AciaIRD		EQU		$02		; receiver interrupt disable, 0=int enabled, 1=int disabled
AciaDTR		EQU		$01		; data terminal ready 0=DTR high, 1=DTR low

; constants for the ACIA control register 
; bits 0..3 specify the baud rate

AciaBrdMask	EQU		$0F		; baud rate mask
AciaCBrdExt	EQU		$00		; 1/16th external clock
AciaCBrd50	EQU		$01		; 50 baud
AciaCBrd75	EQU		$02		; 75 baud
AciaCBrd110	EQU		$03		; 110 baud
AciaCBrd135	EQU		$04		; 135 baud
AciaCBrd150	EQU		$05		; 150 baud
AciaCBrd300	EQU		$06		; 300 baud
AciaCBrd600	EQU		$07		; 600 baud
AciaCBrd1200	EQU		$08		; 1200 baud
AciaCBrd1800	EQU		$09		; 1800 baud
AciaCBrd2400	EQU		$0A		; 2400 baud
AciaCBrd3600	EQU		$0B		; 3600 baud
AciaCBrd4800	EQU		$0C		; 4800 baud
AciaCBrd7200	EQU		$0D		; 7200 baud
AciaCBrd9600	EQU		$0E		; 9600 baud
AciaCBrd19200	EQU		$0E		; 19200 baud

AciaCRxClk	EQU		$10		; receiver clock source, 0= external, 1=baud rate
AciaWrdMask	EQU		$60		; word length bits mask
AciaCWrd8	EQU		$00		; 8 bit word
AciaCWrd7	EQU		$20		; 7 bit word
AciaCWrd6	EQU		$40		; 6 bit word
AciaCWrd5	EQU		$60		; 5 bit word3
AciaCSBN	EQU		$80		; stop bit number, 0=1 bit, 1=2 bits

; Dragon Alpha Modem port (6850)

DPModemCtrl	EQU		$28		; Modem Control/Status
DPModemData	EQU		$29		; Modem Rx/Tx Data

ModemCtrl	EQU		DPModemCtrl+IO	; Modem Control/Status
ModemData	EQU		DPModemData+IO	; Modem Rx/Tx Data

;DragonDos Cartrage IO for WD2797

;WD2797 Floppy disk controler, used in DragonDos.
DPCmdRegD	EQU		$40		; command/status			
DPTrkRegD	EQU		$41		; Track register
DPSecRegD	EQU		$42		; Sector register
DPDataRegD	EQU		$43		; Data register

CmdRegD		EQU		DPCMDREGD+IO	; command/status	4		
TrkRegD		EQU		DPTRKREGD+IO	; Track register
SecRegD		EQU		DPSECREGD+IO	; Sector register
DataRegD	EQU		DPDATAREGD+IO	; Data register

DPDSKCTLD	EQU		$48		; Disk DS/motor control reg
DSKCTLD		EQU		DPDSKCTLD+IO		

; Disk IO bitmasks (DragonDos).

NMIEnD    	EQU		%00100000 
WPCEnD    	EQU   		%00010000 
SDensEnD  	EQU   		%00001000 
MotorOnD  	EQU   		%00000100 
Drive0D		EQU		%00000000
Drive1D		EQU		%00000001
Drive2D		EQU		%00000010
Drive3D		EQU		%00000011
DriveMaskD	EQU		%00000011	; Mask to extract drives

; Disk IO bitmasks (Dragon Alpha).

;NMIEnA    	EQU		%10000000	; This is just a guess, but in current code just used as a flag 
Drive5or8	EQU		%10000000	; is drive in 5" or 8" mode Acording to circuit trace on R.Harding's machine
WPCEnA    	EQU	   	%01000000 	; Acording to circuit trace by R.Harding.
SDensEnA  	EQU   		%00100000 	; DDen Acording to circuit trace on R.Harding's machine
MotorOnA  	EQU   		%00010000 	
Drive0A		EQU		%00000001	; Drive selects acording to OS9 headers
Drive1A		EQU		%00000010
Drive2A		EQU		%00000100
Drive3A		EQU		%00001000
DriveMaskA	EQU		%00001111	; Mask to extract drives

; On the Alpha, NMI is enabled/disabled by setting CA2 of the third PIA, High=enabled.

;WD1793/1772 Floppy disk controler, used in RS-DOS.
DPCmdRegT	EQU		$48		; command/status			
DPTrkRegT	EQU		$49		; Track register
DPSecRegT	EQU		$4A		; Sector register
DPDataRegT	EQU		$4B		; Data registerT

CmdRegT		EQU		DPCMDREGT+IO	; command/status	4		
TrkRegT		EQU		DPTRKREGT+IO	; Track register
SecRegT		EQU		DPSECREGT+IO	; Sector register
DataRegT	EQU		DPDATAREGT+IO	; Data register

DPDSKCTLT	EQU		$40		; Disk DS/motor control reg
DSKCTLT		EQU		DPDSKCTLT+IO		

; Disk IO bitmasks (RSDos FD-500).

HaltEn    	EQU		%10000000	; Halt enable
SS0    		EQU	   	%01000000 	; Side select
SDensEnT	EQU   		%00100000 	; Double density enable 
NMIEnT		EQU		%00100000 	; Enable NMI, always enabled when in DD mode
WPCEnT  	EQU   		%00010000 	
MotorOnT	EQU		%00001000	; Drive selects only 3
Drive0T		EQU		%00000001
Drive1T		EQU		%00000010
Drive2T		EQU		%00000100
Drive3T		EQU		%00000100	; Drive 3 same as drive 2 !
DriveMaskT	EQU		%00000111	; Mask to extract drives
DriveOffMaskT	EQU		MotorOnT+DriveMaskT	

; PIA control register bitmasks
CRIRQ1		EQU		%10000000	; goes to 1 when an inerrupt is triggered on CA1/CB1
CRIRQ2		EQU		%01000000	; goes to 1 when an inerrupt is triggered on CA2/CB2
CRC2IO		EQU		%00100000	; CA2/CB2 output when 1, input when 0
CRDDRDATA	EQU		%00000100	; DDR register accessed if 1, data if 0
CRIRQDir	EQU		%00000010	; CA1/CB1 IRQ edge : 0 = falling, 1=rising
CRIRQ		EQU		%00000001	; CA1/CB1 IRQ enable

; PIA1 bismasks

; Port B

MaskAG		equ		%10000000	; VDG A/G
MaskGM2		equ		%01000000	; VDG GM2
MaskGM1		equ		%00100000	; VDG GM1
MaskGM0		equ		%00010000	; VDG GM0
MaskCSS		equ		%00001000	; VDG CSS
MaskROMSEL	equ		%00000100	; ROM select output on Dragon 64
MaskRAMType	equ		%00000100	; RAM type input on Dragon 32
MaskSBS		equ		%00000010	; Single bit sound sense
MaskBUSY	equ		%00000001	; Printer busy 

; following 2 for convenience
MaskVideo	equ		(MaskAG+MaskGM2+MaskGM1+MaskGM0+MaskCSS)
MaskNONVideo	equ		(MaskRAMType+MaskSBS+MaskBUSY)