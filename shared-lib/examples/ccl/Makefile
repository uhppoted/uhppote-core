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

open-door: build
	$(CMD) open-door

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

delete-cards: build
	$(CMD) delete-cards

get-event-index: build
	$(CMD) get-event-index

set-event-index: build
	$(CMD) set-event-index

get-event: build
	$(CMD) get-event

record-special-events: build
	$(CMD) record-special-events

get-time-profile: build
	$(CMD) get-time-profile

set-time-profile: build
	$(CMD) set-time-profile

run: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' --load main.lisp

debug: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp

