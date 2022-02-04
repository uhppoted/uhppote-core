DIST  ?= development
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
	go build -trimpath -o bin/libuhppote.so -buildmode=c-shared shared-lib/go/main.go

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

lib: 
	cd lib && go build -trimpath -o libuhppote.so -buildmode=c-shared main.go && clang -o test test.c -L. -luhppote

