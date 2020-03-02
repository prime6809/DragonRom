# DragonRom
Assembleable, commented disassembily of the Dragon 32 / Dragon 64 basic ROMS
Included here is the disassembled source for the Dragon 32 and Dragon 64 ROMS.

This was dissassembled and merged in 2006, and had been partly commented over the years.
I have now fully commented it using the Spectral Ascociates "Color basic unreavelled" and 
"Extended color basic unreavelled" as a partial source to work out what the code was doing,
as the Dragon ROMS share a lot of common code with the Tandy ones. 

To build the included source you will need a copy of the lwasm assembler available from :

http://www.lwtools.ca/

You will also need a build environment that has a unix type Make utility, I use cygwin
under MS windows, but it should also be buildable on Linux or MacOS assuming the correct
tools are installed.

The project has the following structure

Root folder :

DragonROM.asm  		main assembler source file.
Makefile			the project build file.

folder: defs

Various small assembler files that define constants that are used in the main assembler file.

folder: roms
Empty by default, but the build rom images will be put in here at build time.

folder: list
Empty by default, but the assembler listing files will be put in here at build time.


I'd like to acknoledge the following sources used to prepare this disassembly :

"Inside the Dragon" by D.N.Smeed and I.Sommerville
"Color Basic Unravelled" and 
"Exteneded Color Basic Unravelled" 
	Published by Spectral Associates and revised by Walter K Zydhek.
	