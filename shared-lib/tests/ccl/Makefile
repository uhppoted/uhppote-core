LIB=../../lib/tests
BINDINGS=../../bindings/ccl
CMD=./test

.PHONY: build
.PHONY: debug

lib: 
	cd ../../go && make all

build: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp --eval '(make-app)' 

invalid: build
	$(CMD) mystery-meat

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

all: build
	$(CMD) all

test: build
	$(CMD)

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp 

