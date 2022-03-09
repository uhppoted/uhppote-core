LIB=../../lib
GO=GODEBUG=cgocheck=2
DLL=export DYLD_LIBRARY_PATH=$(LIB)/lib
WRAPPER=export PYTHONPATH=$(PYTHONPATH):..
CMD=python example.py 

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...

format:
	yapf -ri ..

build: format

get-devices: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-devices

get-device: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) get-device

set-address: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) set-address

run: build
	$(GO) && $(DLL) && $(WRAPPER) && $(CMD) all

debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-devices
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) get-device
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(WRAPPER) && $(CMD) set-address
