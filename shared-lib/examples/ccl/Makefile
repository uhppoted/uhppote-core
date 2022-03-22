LIB=../../lib
BINDINGS=../../bindings/ccl
REPL=GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp --eval '(main)'
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
	# $(REPL) -- get-devices

get-device: build
	$(CMD) get-device
	# $(REPL) -- get-device

set-address: build
	$(CMD) set-address
	# $(REPL) -- set-address

get-status: build
	$(CMD) get-status
	# $(REPL) -- get-status

run: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp

