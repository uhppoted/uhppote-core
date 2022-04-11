DIST     ?= development
LIB       = shared-lib/lib
LIBC      = shared-lib/c
LIBCPP    = shared-lib/c++
LIBPYTHON = shared-lib/python
DEBUG    ?= --debug

.PHONY: bump
.PHONY: shared-lib

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

build-all: test vet shared-lib
	env GOOS=linux   GOARCH=amd64       go build -trimpath ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build -trimpath ./...
	env GOOS=darwin  GOARCH=amd64       go build -trimpath ./...
	env GOOS=windows GOARCH=amd64       go build -trimpath ./...

release: test vet shared-lib
	env GOOS=linux   GOARCH=amd64       go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64       go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64       go build -trimpath -o dist/$(DIST)/windows ./...

debug: build
	go test ./... -run TestTSVUnmarshalTasks

godoc:
	godoc -http=:80	-index_interval=60s

shared-lib: 
	make -C ./shared-lib -f Makefile build
