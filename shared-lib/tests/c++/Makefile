LIB=../../lib/tests
BINDINGS=../../bindings/c++
CMD=GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && ./test

lib: 
	cd ../../go && make all

format:
	clang-format -i *.cpp
	clang-format -i *.hpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11  -o test test.cpp device.cpp $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -lc++ -L$(LIB) -luhppoted

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device

set-address: build
	$(CMD) set-address

get-status: build
	get-status

get-time: build
	$(CMD) get-time

test: build
	$(CMD) all
