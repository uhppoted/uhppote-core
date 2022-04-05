LIB=../../lib
BINDINGS=../../bindings/c++
CMD=./example
SRC=example.cpp device.cpp cards.cpp

lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.cpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11 -o example $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -lc++ -L$(LIB) -luhppoted

usage: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD)

invalid: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) mystery-meat

get-devices: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-status

get-time: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-time

set-time: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-time

get-listener: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-listener

set-listener: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-listener

get-door-control: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-door-control

set-door-control: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) set-door-control

get-cards: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-cards

debug: format
	clang -std=c++11  -o example $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB)/debug -lc++ -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) get-cards
