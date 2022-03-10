LIB=./lib

.PHONY: python
.PHONY: ccl

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/lib/libuhppoted.so   ./go/lib/...
	go build -trimpath -buildmode=c-shared -o $(LIB)/debug/libuhppoted.so ./go/debug/...

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
