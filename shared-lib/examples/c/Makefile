LIB=../../lib
BINDINGS=../../bindings/c
CMD=./example

lib: 
	cd ../../go && make all

format:
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format
	clang -o example example.c device.c $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

invalid: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) mystery-meat

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-status

get-time: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-time

set-time: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-time

get-listener: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-listener

set-listener: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-listener

get-door-control: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-door-control

all: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD)

debug: format
	clang -o example example.c device.c $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-door-control

debug-all: format
	clang -o example example.c device.c $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) all
