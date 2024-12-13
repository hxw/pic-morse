# Makefile

#PROCESSOR ?= pic16c71
PROCESSOR ?= pic16f84

TTY ?= /dev/cuaU0

.PHONY: all
all: morse.hex

morse.hex: morse.o

.PHONY: clean
clean:
	rm -f *.hex *.o *.lst *.cod *.map *~


.PHONY: read
read:
	picprog --device="${PROCESSOR}" --ihx16 --slow --reboot --pic-serial-port="${TTY}" --output-hexfile=dump.hex

.PHONY: erase
erase:
	picprog --device="${PROCESSOR}" --ihx16 --slow --reboot --pic-serial-port="${TTY}" --erase --burn

.PHONY: burn
burn: morse.hex
	picprog --device="${PROCESSOR}" --ihx16 --slow --reboot --pic-serial-port="${TTY}" --input-hexfile=morse.hex --burn


.SUFFIXES: .asm .inc .hex

.asm.o:
	gpasm --processor="${PROCESSOR}" --object --strict=2 --output="${.PREFIX}" ${.ALLSRC}

.o.hex:
	gplink --map --hex-format=inhx16 --output="${.TARGET}" ${.ALLSRC}
