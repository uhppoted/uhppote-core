CMD=./example

build: 
	cp ../../../bin/libuhppote.so .
	clang -o example example.c ../src/uhppote.c -L. -luhppote

get-devices: build
	GODEBUG=cgocheck=2 && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && $(CMD) get-device

