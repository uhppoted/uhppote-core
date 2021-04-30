DIST  ?= development
DEBUG ?= --debug
VERSION = v0.7.x
LDFLAGS = -ldflags "-X uhppote.VERSION=$(VERSION)" 

.PHONY: bump

all: test      \
	 benchmark \
     coverage

clean:
	go clean
	rm -rf bin

format: 
	go fmt ./...

build: format
	go build ./...

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

release: test vet
	mkdir -p dist/$(DIST)/windows
	mkdir -p dist/$(DIST)/darwin
	mkdir -p dist/$(DIST)/linux
	mkdir -p dist/$(DIST)/arm7
	env GOOS=linux   GOARCH=amd64       go build ./...
	env GOOS=linux   GOARCH=arm GOARM=7 go build ./...
	env GOOS=darwin  GOARCH=amd64       go build ./...
	env GOOS=windows GOARCH=amd64       go build ./...

bump:

debug: build
	go test ./... -run TestGetTimeProfile

godoc:
	godoc -http=:80	-index_interval=60s
