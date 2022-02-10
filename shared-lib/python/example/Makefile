LIB=../../lib
CMD=python example.py 

lib: 
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppote.so ../../go/main.go

build: 

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all
