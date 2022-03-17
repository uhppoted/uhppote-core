LIB=../../lib
DLL=../../lib/test
BINDINGS=../../bindings/c++
CMD=./test

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/test/libuhppoted.so ../../go/test/...

format:
	clang-format -i *.cpp
	clang-format -i *.hpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11  -o test test.cpp device.cpp $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(DLL) -lc++ -L$(DLL) -luhppoted

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
