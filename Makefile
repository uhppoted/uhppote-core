DIST  ?= development
LIB    = shared-lib/lib
LIBC   = shared-lib/c
LIBCPP = shared-lib/c++
DEBUG ?= --debug

.PHONY: bump
.PHONY: lib

all: test      \
	 benchmark \
     coverage

clean:
	go clean
	rm -rf bin

format: 
	go fmt ./...

build: format
	go build -trimpath ./...
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppote.so shared-lib/go/main.go

test: build
	go test ./...

vet: build
	go vet ./...

lint: build
	golint ./...

benchmark: build
	go test -bench ./...

coverage: build
	go test -cover ./...

build-all: test vet
	env GOOS=linux   GOARCH=amd64       go build -trimpath ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build -trimpath ./...
	env GOOS=darwin  GOARCH=amd64       go build -trimpath ./...
	env GOOS=windows GOARCH=amd64       go build -trimpath ./...

release: test vet
	env GOOS=linux   GOARCH=amd64       go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64       go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64       go build -trimpath -o dist/$(DIST)/windows ./...

debug: build
	go test ./... -run TestTSVUnmarshalTasks

godoc:
	godoc -http=:80	-index_interval=60s

example-c:
	clang -o $(LIBC)/example/example $(LIBC)/example/example.c $(LIBC)/example/device.c $(LIBC)/src/uhppote.c -I$(LIB) -L$(LIB) -luhppote
	export DYLD_LIBRARY_PATH=$(LIB) && $(EXAMPLEC)/example get-device

example-c++:
	clang -std=c++11 -lc++ -o $(LIBCPP)/example/example $(LIBCPP)/example/example.cpp $(LIBCPP)/example/device.cpp $(LIBCPP)/src/uhppote.cpp -I$(LIB) -L$(LIB) -luhppote
	export DYLD_LIBRARY_PATH=$(LIB) && ./shared-lib/c/example/example get-device
