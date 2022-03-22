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

run: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) all

debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-devices
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-device
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) set-address
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-status
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-time
