LIB=../../lib/tests
BINDINGS=../../bindings/c
CMD=GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && ./tests
SRC=tests.c device.c cards.c events.c time_profiles.c
lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format
	clang -o tests $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

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

open-door: build
	$(CMD) open-door

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

delete-cards: build
	$(CMD) delete-cards

get-event-index: build
	$(CMD) get-event-index

set-event-index: build
	$(CMD) set-event-index

get-event: build
	$(CMD) get-event

record-special-events: build
	$(CMD) record-special-events

get-time-profile: build
	$(CMD) get-time-profile

set-time-profile: build
	$(CMD) set-time-profile

all: build
	$(CMD) all

test: build
	$(CMD)

