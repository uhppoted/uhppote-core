LIB=../../lib
DLL=../../lib/test
BINDINGS=../../bindings/c
CMD=./test

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/test/libuhppoted.so ../../go/test/...

format:
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format
	clang -o test test.c device.c $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(DLL) -L$(DLL) -luhppoted

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(DLL) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(DLL) && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(DLL) && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(DLL) && $(CMD) get-status

test: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(DLL) && $(CMD) all

