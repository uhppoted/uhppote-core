# LIB=../../lib
LIB=.
CMD=python example.py 

lib: 
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppote.so ../../go/main.go

build: lib
#     clang -o example example.c device.c ../src/uhppote.c -I$(LIB) -L$(LIB) -luhppote

# get-devices: build
# 	GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-devices

get-device: build
	$(CMD) get-device
	# GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) get-device

# run: build
#     GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && $(CMD) all
