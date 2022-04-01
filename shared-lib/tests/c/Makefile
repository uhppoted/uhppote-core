LIB=../../lib/tests
BINDINGS=../../bindings/c
CMD=GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && ./test

lib: 
	cd ../../go && make all

format:
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format
	clang -o test test.c device.c $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

invalid: build
	$(CMD) mystery-meat

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device

set-address: build
	$(CMD) set-address

get-status: build
	$(CMD) get-status

get-time: build
	$(CMD) get-time

set-time: build
	$(CMD) set-time

get-listener: build
	$(CMD) get-listener

set-listener: build
	$(CMD) set-listener

get-door-control: build
	$(CMD) get-door-control

all: build
	$(CMD) all

test: build
	$(CMD)

debug: build
	$(CMD) get-listener
