LIB=../../lib/tests
BINDINGS=../../bindings/c++
CMD=GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && ./test
SRC=test.cpp device.cpp cards.cpp

lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.cpp
	clang-format -i *.hpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11 $(SRC) -o test  $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -lc++ -L$(LIB) -luhppoted

usage: build
	$(CMD) help

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

set-listener: build
	$(CMD) set-listener

get-door-control: build
	$(CMD) get-door-control

set-door-control: build
	$(CMD) set-door-control

get-cards: build
	$(CMD) get-cards

get-card: build
	$(CMD) get-card

get-card-by-index: build
	$(CMD) get-card-by-index

put-card: build
	$(CMD) put-card

delete-card: build
	$(CMD) delete-card

all: build
	$(CMD) all

test: build
	$(CMD)

debug: build
	$(CMD) delete-card
