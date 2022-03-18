LIB=../../lib
BINDINGS=../../bindings/c++
CMD=./example

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/test/libuhppoted.so ../../go/test/...

format:
	clang-format -i *.cpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11  -o example example.cpp device.cpp $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB)/lib -lc++ -L$(LIB)/lib -luhppoted

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-status

get-time: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-time

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) all

debug: format
	clang -std=c++11  -o example example.cpp device.cpp $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB)/debug -lc++ -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-devices
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-device
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) set-address
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-status
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-time
