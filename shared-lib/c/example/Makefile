LIB=../../lib
CMD=./example

lib: 
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppote.so ../../go/main.go

build: 
	clang -o example example.c ../src/uhppote.c -I$(LIB) -L$(LIB) -luhppote

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

