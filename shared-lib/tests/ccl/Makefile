LIB=../../lib/tests
BINDINGS=../../bindings/ccl
CMD=./test

.PHONY: build
.PHONY: debug

lib: 
	cd ../../go && make all

build: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp --eval '(make-app)' 

usage: build
	$(CMD) help

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

set-listener: build
	$(CMD) set-listener

get-door-control: build
	$(CMD) get-door-control

set-door-control: build
	$(CMD) set-door-control

get-cards: build
	$(CMD) get-cards

get-card: build
	$(CMD) get-card

get-card-by-index: build
	$(CMD) get-card-by-index

put-card: build
	$(CMD) put-card

delete-card: build
	$(CMD) delete-card

all: build
	$(CMD) all

test: build
	$(CMD)

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp 

