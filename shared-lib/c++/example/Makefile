LIB=../../lib
CMD=./example

lib: 
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppoted.so ../../go/main.go

build: 
	clang -std=c++11  -o example example.cpp device.cpp ../src/uhppoted.cpp -I$(LIB) -lc++ -L$(LIB) -luhppoted

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all
