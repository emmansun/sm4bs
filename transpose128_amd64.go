// Code generated by command: go run transpose_amd64_asm.go -out ../transpose128_amd64.s -stubs ../transpose128_amd64.go -pkg sm4bs. DO NOT EDIT.

//go:build amd64 && gc && !purego

package sm4bs

// Bit level matrix transpose, 128x128
func transpose128(in *byte, out *byte)
