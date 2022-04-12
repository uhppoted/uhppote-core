LIB=../../lib
BINDINGS=../../bindings/c
CMD=./example
SRC=example.c device.c cards.c

lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format 
	clang -o example $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

usage: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD)

help: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) help

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

set-door-control: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-door-control

get-cards: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-cards

get-card: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-card

get-card-by-index: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-card-by-index

put-card: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) put-card

delete-card: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) delete-card

delete-cards: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) delete-cards

debug: format
	clang -o example $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) delete-cards

