#!/bin/bash

FILE=$1
shift

if [ "$FILE.asm" -nt "$FILE.hex" ]; then
	echo 'Compiling assembly to hex'

	avra "$FILE.asm"

	if [ $? -ne 0 ]; then
		exit 1
	fi
fi

if [ "$FILE.hex" -nt "$FILE.bin" ]; then
	echo 'Converting hex to bin'

	hex2bin "$FILE.hex"

	if [ $? -ne 0 ]; then
		exit 1
	fi
fi

simulavr -d atmega128 -g "$FILE.bin" "$@" &

SIMAVRPID=$!

avr-gdb -ex 'target remote localhost:1212' -ex 'layout asm'

kill $SIMAVRPID
wait $SIMAVRPID
