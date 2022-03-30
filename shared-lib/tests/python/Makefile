LIB=../../lib/tests
GO=GODEBUG=cgocheck=2
DLL=export DYLD_LIBRARY_PATH=$(LIB)
WRAPPER=export PYTHONPATH=$(PYTHONPATH):..
CMD=$(GO) && $(DLL) && python test.py 

lib: 
	cd ../../go && make all

format:
	yapf -ri .
	yapf -ri ../../bindings/python

build: format

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

debug: build
	$(CMD) mystery-meat
