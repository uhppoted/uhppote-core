LIB=../../lib
CMD=mono example.exe

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/main.go
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/main.go

build: 
	csc ../uhppoted.cs example.cs

usage: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD)

help: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) help

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) get-device

run: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/lib && $(CMD) all

debug: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-device
