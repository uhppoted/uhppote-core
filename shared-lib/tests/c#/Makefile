LIB=../../lib/tests
BINDINGS=../../bindings/c\#
CMD=mono test.exe

lib: 
	cd ../../go && make all

format:
	clang-format -i *.cs
	clang-format -i $(BINDINGS)/*.cs

format-dotnet:
	dotnet format --include *.cs --folder
	dotnet format --include $(BINDINGS)/*.cs --folder

build: format
	csc $(BINDINGS)/uhppoted.cs test.cs

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

get-card: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-card

get-card-by-index: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-card-by-index

all: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all

test: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD)

debug: build
	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-card

