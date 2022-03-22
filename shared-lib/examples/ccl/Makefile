LIB=../../lib
BINDINGS=../../bindings/ccl
CMD=GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp --eval '(main)' 

.PHONY: run
.PHONY: debug

lib: 
	cd ../../go && make all

build:

usage: build
	$(CMD) -- xxx

help: build
	$(CMD) -- help

get-devices: build
	$(CMD) -- get-devices

get-device: build
	$(CMD) -- get-device

set-address: build
	$(CMD) -- set-address

get-status: build
	$(CMD) -- get-status

run: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp

