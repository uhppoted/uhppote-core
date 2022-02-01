format: 
	go fmt ./...

build: format
	go build -trimpath -o libuhppote.so -buildmode=c-shared main.go
	clang -o example example.c uhppote.c -L. -luhppote

get-device: build
	GODEBUG=cgocheck=2 && ./example get-device

