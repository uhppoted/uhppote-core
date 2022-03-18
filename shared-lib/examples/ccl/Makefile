LIB=../../lib
BINDINGS=../../bindings/ccl

.PHONY: run
.PHONY: debug

lib: 
	cd ../../go && make all

build:

run: 
	GODEBUG=cgocheck=2 && \
	rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)")' \
	      --load $(BINDINGS)/packages.lisp \
	      --load ./packages.lisp  \
	      --load $(BINDINGS)/uhppoted.lisp \
	      --load examples.lisp

debug: 
	GODEBUG=cgocheck=2 && \
	rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' \
	      --load $(BINDINGS)/packages.lisp \
	      --load ./packages.lisp \
	      --load $(BINDINGS)/uhppoted.lisp \
	      --load examples.lisp

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
