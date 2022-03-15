LIB=../../lib
CMD=./example

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...

format:
	clang-format -i *.c
	clang-format -i ../src/*.c
	clang-format -i ../include/*.h

build: format
	clang -o example example.c device.c ../src/uhppoted.c -I$(LIB)/lib -L$(LIB)/lib -luhppoted

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-status

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) all

debug: format
	clang -o example example.c device.c ../src/uhppoted.c -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-devices
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-device
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) set-address
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-status
