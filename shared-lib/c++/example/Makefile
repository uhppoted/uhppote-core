LIB=../../lib
CMD=./example

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/main.go
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/main.go

format:
	clang-format -i *.cpp
	clang-format -i ../src/*.cpp
	clang-format -i ../include/*.hpp

build: format
	clang -std=c++11  -o example example.cpp device.cpp ../src/uhppoted.cpp -I$(LIB)/lib -lc++ -L$(LIB)/lib -luhppoted

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-device

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) all

debug: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-device
