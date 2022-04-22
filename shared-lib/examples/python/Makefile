LIB=../../lib
GO=GODEBUG=cgocheck=2
DLL=export DYLD_LIBRARY_PATH=$(LIB)
WRAPPER=export PYTHONPATH=$(PYTHONPATH):..
CMD=python examples.py 

lib: 
	cd ../../go && make all

format:
	yapf -ri .
	yapf -ri ../../bindings/python

build: format

usage: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD)

help: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) help

invalid: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) mystery-meat

get-devices: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-devices

get-device: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-device

set-address: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-address

get-status: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-status

get-time: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-time

set-time: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-time

get-listener: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-listener

set-listener: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-listener

get-door-control: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-door-control

set-door-control: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-door-control

open-door: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) open-door

get-cards: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-cards

get-card: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-card

get-card-by-index: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-card-by-index

put-card: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) put-card

delete-card: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) delete-card

delete-cards: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) delete-cards

get-event-index: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-event-index

set-event-index: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-event-index

get-event: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-event

record-special-events: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) record-special-events

get-time-profile: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-time-profile

set-time-profile: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-time-profile

debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) set-time-profile

