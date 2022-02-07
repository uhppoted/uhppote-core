LIB=../../lib
CMD=./example

lib: 
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppote.so ../../go/main.go

build: 
	clang -std=c++11  -o example example.cpp device.cpp ../src/uhppote.cpp -I$(LIB) -lc++ -L$(LIB) -luhppote

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

all: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all
