LIB=./lib

.PHONY: python
.PHONY: ccl
.PHONY: examples
.PHONY: tests

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared             -o $(LIB)/libuhppoted.so       ./...
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/libuhppoted.so ./...
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/libuhppoted.so ./...

c: ./examples/c/example ./tests/c/test
	make -C ./examples/c -f Makefile build
	make -C ./tests/c    -f Makefile build

cpp: ./examples/c++/example ./tests/c++/test
	make -C ./examples/c++ -f Makefile build
	make -C ./tests/c++    -f Makefile build

csharp: ./examples/c#/example.exe ./tests/c#/test.exe
	make -C ./examples/c# -f Makefile build
	make -C ./tests/c#    -f Makefile build

python: 
	make -C ./examples/python -f Makefile build

ccl: 
	make -C ./examples/ccl -f Makefile build

examples:
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/c#     -f Makefile build
	make -C ./examples/python -f Makefile build
	make -C ./examples/ccl    -f Makefile build

tests: 
	make -C ./tests/c      -f Makefile test
	make -C ./tests/c++    -f Makefile test
	make -C ./tests/c#     -f Makefile test
	make -C ./tests/python -f Makefile test
	make -C ./tests/ccl    -f Makefile test
