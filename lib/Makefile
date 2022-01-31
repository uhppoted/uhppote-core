format: 
	go fmt ./...

build: format
	go build -trimpath -o libuhppote.so -buildmode=c-shared main.go
	clang -o test test.c uhppote.c -L. -luhppote

get-device: build
	GODEBUG=cgocheck=2 && ./test get-device

