LIB=./lib

.PHONY: python
.PHONY: ccl

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared             -o $(LIB)/libuhppoted.so       ./...
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/libuhppoted.so ./...
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/libuhppoted.so ./...

c: ./c/example/example
	make -C ./c/example -f Makefile build

cpp: ./c++/example/example
	make -C ./c++/example -f Makefile build

csharp: ./c#/example/example
	make -C ./c#/example -f Makefile build

python: 
	make -C ./python/example -f Makefile build

ccl: 
	make -C ./ccl/example -f Makefile build
