;
; SAM Defs.
;

SAMVectors	equ		$FFE0			; SAM vector block at top of memory map
SAMVecSize	equ		$20				; SAM vector block size

SAMBase		equ		$FFC0			; Base of SAM bits

; V2 V1 V0	Mode(s)
;  0  0  0	AL, AE, S4, S6
;  0  0  1	G1C, G1R
;  0  1  0	G2C
;  0  1  1	G2R
;  1  0  0	G3C
;  1  0  1	G3R
;  1  1  0	G6C, G6R
;  1  1  1	DMA

SAMCV0		equ		$FFC0			; Video mode bits
SAMSV0		equ		$FFC1
SAMCV1		equ		$FFC2
SAMSV1		equ		$FFC3
SAMCV2		equ		$FFC4
SAMSV2		equ		$FFC5

;
; Binary offset from $0000, in 512 byte pages.
;

SAMCF0		equ		$FFC6			; Display offset
SAMSF0		equ		$FFC7
SAMCF1		equ		$FFC8
SAMSF1		equ		$FFC9
SAMCF2		equ		$FFCA
SAMSF2		equ		$FFCB
SAMCF3		equ		$FFCC
SAMSF3		equ		$FFCD
SAMCF4		equ		$FFCE
SAMSF4		equ		$FFCF
SAMCF5		equ		$FFD0
SAMSF5		equ		$FFD1
SAMCF6		equ		$FFD2
SAMSF6		equ		$FFD3

; Maps 2 pages of 32K into $0000-$7FFF, requires 64K RAM.

SAMCP1		equ		$FFD4			; Page #1
SAMSP1		equ		$FFD5

; R1 R0
;  0  0		Slow, 0.89MHz 
;  0  1 	Address dependent 1.7MHz / 0.89MHz
;  1  0 	Fast
;  1  1 	Fast

SAMCR0		equ		$FFD6			; CPU Rate
SAMSR0		equ		$FFD7
SAMCR1		equ		$FFD8
SAMSR2		equ		$FFD9

; M1 M0
;  0  0		 4K dynamic
;  0  1		16K dynamic
;  1  0		64K dynamic
;  1  1		64K static

SAMCM0		equ		$FFDA			; Memory type
SAMSM0		equ		$FFDB
SAMCM1		equ		$FFDC
SAMSM1		equ		$FFDD

; 0= RAM below 32K, ROM above, 1=RAM for entire map 
; In both map types top 256 bytes reserved for I/O space

SAMCTY		equ		$FFDE			; Map type
SAMSTY		equ		$FFDF




