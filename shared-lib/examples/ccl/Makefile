LIB=../../lib
BINDINGS=../../bindings/ccl

.PHONY: run
.PHONY: debug

lib: 
	go fmt ../../go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ../../go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ../../go/debug/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/test/libuhppoted.so ../../go/test/...

build:

run: 
	ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/lib")' \
	      --load $(BINDINGS)/packages.lisp \
	      --load ./packages.lisp  \
	      --load $(BINDINGS)/uhppoted.lisp \
	      --load examples.lisp

debug: 
	ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' \
	      --load $(BINDINGS)/packages.lisp \
	      --load ./packages.lisp \
	      --load $(BINDINGS)/uhppoted.lisp \
	      --load examples.lisp

old: 
	ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load example.ccl

# usage: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD)
# 
# help: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) help
# 
# get-devices: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices
# 
# get-device: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device
# 
# run: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all
