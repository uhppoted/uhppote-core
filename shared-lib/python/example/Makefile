LIB=../../lib
GO=GODEBUG=cgocheck=2
DLL=export DYLD_LIBRARY_PATH=$(LIB) 
WRAPPER=export PYTHONPATH=$(PYTHONPATH):..
CMD=$(GO) && $(DLL) && $(WRAPPER) && python example.py 

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppoted.so ../../go/main.go

build: 

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device

run: build
	$(CMD) all
