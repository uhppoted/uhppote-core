lib: 
	go fmt ./...
	go build -trimpath -o libuhppote.so -buildmode=c-shared main.go
	clang -o test test.c -L. -luhppote
	GODEBUG=cgocheck=2 && ./test

