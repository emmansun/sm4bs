//go:build amd64 && gc && !purego

package sm4bs

import (
	"bytes"
	"testing"
)

func TestBS64TransposeRevASM(t *testing.T) {
	in := make([]byte, 64*16)
	ret := make([]byte, 64*16)
	out := make([]byte, 64*16)

	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	for i := 0; i < 64; i++ {
		copy(in[i*16:], key)
	}

	transpose64(&in[0], &out[0])
	transpose64Rev(&out[0], &ret[0])
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %x", ret)
	}
}

func BenchmarkBS64ASMTranspose(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 64*16)
	for i := 0; i < 64; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 64*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose64(&input[0], &out[0])
	}
}

func TestBS128TransposeRev(t *testing.T) {
	in := make([]byte, 128*16)
	ret := make([]byte, 128*16)
	out := make([]byte, 128*16)

	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	for i := 0; i < 128; i++ {
		copy(in[i*16:], key)
	}

	transpose128(&in[0], &out[0])
	transpose128(&out[0], &ret[0])
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %v", ret)
	}
}

func BenchmarkBS128Transpose(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 128*16)
	for i := 0; i < 128; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 128*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose128(&input[0], &out[0])
	}
}
