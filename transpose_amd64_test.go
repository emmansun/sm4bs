//go:build amd64 && gc && !purego

package sm4bs

import (
	"bytes"
	"testing"
)

func TestBS128TransposeRevAvx(t *testing.T) {
	in := make([]byte, 128*16)
	ret := make([]byte, 128*16)
	out := make([]byte, 128*16)

	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	for i := 0; i < 128; i++ {
		copy(in[i*16:], key)
	}

	transpose128avx(&in[0], &out[0])
	transpose128avx(&out[0], &ret[0])
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %x, out %x", ret, out)
	}
}

func BenchmarkBS128TransposeAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 128*16)
	for i := 0; i < 128; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 128*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose128avx(&input[0], &out[0])
	}
}

func BenchmarkBS128TransposeRevAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 128*16)
	for i := 0; i < 128; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 128*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose128RevAvx(&input[0], &out[0])
	}
}

func TestBS256TransposeRev(t *testing.T) {
	in := make([]byte, 256*16)
	ret := make([]byte, 256*16)
	out := make([]byte, 256*16)

	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	for i := 0; i < 256; i++ {
		copy(in[i*16:], key)
	}

	transpose256avx(&in[0], &out[0])
	transpose128x256avx2(&out[0], &ret[0])
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %x", ret[:16])
	}
}

func BenchmarkBS256TransposeAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 256*16)
	for i := 0; i < 256; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 256*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose256avx(&input[0], &out[0])
	}
}

func BenchmarkBS256TransposeRevAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 256*16)
	for i := 0; i < 256; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 256*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose256RevAvx(&input[0], &out[0])
	}
}

func TestBS64Transpose(t *testing.T) {
	in := make([]byte, 64*16)
	ret := make([]byte, 64*16)
	out := make([]byte, 64*16)

	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	for i := 0; i < 64; i++ {
		copy(in[i*16:], key)
	}

	transpose64avx(&in[0], &out[0])
	transpose64Rev(&out[0], &ret[0])
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %x", ret[:16])
	}
}

func BenchmarkBS64TransposeAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 64*16)
	for i := 0; i < 64; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 64*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose64avx(&input[0], &out[0])
	}
}

func BenchmarkBS64TransposeRevAvx(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, 64*16)
	for i := 0; i < 64; i++ {
		copy(input[i*16:], key)
	}
	out := make([]byte, 64*16)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		transpose64RevAvx(&input[0], &out[0])
	}
}
