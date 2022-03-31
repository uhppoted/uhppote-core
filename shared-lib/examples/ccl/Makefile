LIB=../../lib
BINDINGS=../../bindings/ccl
CMD=./examples

.PHONY: run
.PHONY: debug

lib: 
	cd ../../go && make all

build: examples
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp --eval '(make-app)' 

usage: build
	$(CMD) 

help: build
	$(CMD) help

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device

set-address: build
	$(CMD) set-address

get-status: build
	$(CMD) get-status

get-time: build
	$(CMD) get-time

set-time: build
	$(CMD) set-time

get-listener: build
	$(CMD) get-listener

set-listener: build
	$(CMD) set-listener

run: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp

