DIST     ?= development
LIB       = shared-lib/lib
LIBC      = shared-lib/c
LIBCPP    = shared-lib/c++
LIBPYTHON = shared-lib/python
DEBUG    ?= --debug

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
	# env CC_FOR_TARGET=????? CGO_ENABLED=1 GOOS=linux GOARCH=amd64 go build -trimpath -buildmode=c-shared shared-lib/go/main.go

release: test vet
	env GOOS=linux   GOARCH=amd64       go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64       go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64       go build -trimpath -o dist/$(DIST)/windows ./...

debug: build
	go test ./... -run TestTSVUnmarshalTasks

godoc:
	godoc -http=:80	-index_interval=60s

lib: format
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppoted.so shared-lib/go/main.go

shared-lib-c: lib
	clang -o $(LIBC)/example/example $(LIBC)/example/example.c $(LIBC)/example/device.c $(LIBC)/src/uhppoted.c -I$(LIB) -L$(LIB) -luhppoted
	export DYLD_LIBRARY_PATH=$(LIB) && $(LIBC)/example/example all

shared-lib-c++: lib
	clang -std=c++11 -lc++ -o $(LIBCPP)/example/example $(LIBCPP)/example/example.cpp $(LIBCPP)/example/device.cpp $(LIBCPP)/src/uhppoted.cpp -I$(LIB) -L$(LIB) -luhppoted
	export DYLD_LIBRARY_PATH=$(LIB) && $(LIBCPP)/example/example all

shared-lib-python: lib
	export DYLD_LIBRARY_PATH=$(LIB) && export PYTHONPATH=$(PYTHONPATH):$(LIBPYTHON) && python $(LIBPYTHON)/example/example.py all


# xxx:
# 	docker run -it --rm \
#   		-v $GOPATH/src/github.com/uhppoted/uhppote-core:/go/src/github.com/uhppoted/uhppote-core \
#   		-w /go/src/github.com/uhppoted/uhppote-core \
#   		-e CGO_ENABLED=1 \
#   		docker.elastic.co/beats-dev/golang-crossbuild:1.17.6-darwin \
#   		--build-cmd "ls -la /" \
#   		-p "darwin/amd64"
