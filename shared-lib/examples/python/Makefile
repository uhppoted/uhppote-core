LIB=../../lib
GO=GODEBUG=cgocheck=2
DLL=export DYLD_LIBRARY_PATH=$(LIB)
WRAPPER=export PYTHONPATH=$(PYTHONPATH):..
CMD=python example.py 

lib: 
	cd ../../go && make all

format:
	yapf -ri .
	yapf -ri ../../bindings/python

build: format

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

all: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) all

debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) set-door-control

debug-all: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) all
