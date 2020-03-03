#
# Makefile for Dragon ROMS rom.
#
# 2006, 2020-02-19, P.Harvey-Smith.
#

AS=lwasm
ASFILES=DragonROM.asm 
ASFLAGS=-9 -r -I defs

all: d32 d64_1 d64_2 
#d32_test

d32:	DragonROM.asm
	$(AS) $(ASFLAGS) -oroms/dragon32.rom -llist/d32.lst DragonROM.asm
	

d64_1:	DragonROM.asm
	$(AS) $(ASFLAGS) -DDragon64 -oroms/dragon64_1.rom -llist/d64_1.lst DragonROM.asm

d64_2:	DragonROM.asm
	$(AS) $(ASFLAGS) -DDragon64 -DDragon64ram -oroms/dragon64_2.rom -llist/d64_2.lst DragonROM.asm

clean:
	rm roms/*.rom
	rm list/*.lst
	
check:
	cmp -l roms/dragon32.rom ../ROMS/d32.rom
	cmp -l roms/dragon64_1.rom ../ROMS/d64_1.rom
	cmp -l roms/dragon64_2.rom ../ROMS/d64_2.rom
