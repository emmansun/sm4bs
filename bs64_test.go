package sm4bs

import (
	"bytes"
	"testing"
)

func TestBS64Transpose(t *testing.T) {
	in := make([]byte, BSBlockSize)
	out := make([]uint64, BlockSize)

	in[0] = 1
	BS64.transpose(in, out)
	if out[7] != 0x1 {
		t.Fatalf("not expected %x", out)
	}
	out[0] = 0

	for i := 0; i < 64; i++ {
		in[i*16] = 1
	}
	BS64.transpose(in, out)
	if out[7] != 0xffffffffffffffff {
		t.Fatalf("not expected %x", out)
	}
}

func TestBS64TransposeRev(t *testing.T) {
	in := make([]byte, BSBlockSize)
	ret := make([]byte, BSBlockSize)
	out := make([]uint64, BlockSize)

	in[0] = 1
	BS64.transpose(in, out)
	BS64.transposeRev(out, ret)
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %v", ret)
	}
	for i := 0; i < 64; i++ {
		in[i*16] = 1
	}
	BS64.transpose(in, out)
	BS64.transposeRev(out, ret)
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %v", ret)
	}
}

func BenchmarkBS64Transpose(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	input := make([]byte, BSBlockSize)
	for i := 0; i < WordSize; i++ {
		copy(input[i*16:], key)
	}
	out := make([]uint64, BlockSize)

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BS64.transpose(input, out)
	}
}

func TestBS64EncryptBlocks(t *testing.T) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	expected := []byte{0x68, 0x1e, 0xdf, 0x34, 0xd2, 0x06, 0x96, 0x5e, 0x86, 0xb3, 0xe9, 0x4f, 0x53, 0x6e, 0x42, 0x46}
	enc := make([]uint32, rounds)
	dec := make([]uint32, rounds)
	input := make([]byte, BSBlockSize)
	output := make([]byte, BSBlockSize)
	for i := 0; i < WordSize; i++ {
		copy(input[i*16:], key)
	}

	expandKey(key, enc, dec)
	//Encrypt
	BS64.EncryptBlocks(enc, output, input)
	for i := 0; i < WordSize; i++ {
		if !bytes.Equal(expected, output[i*16:(i+1)*16]) {
			t.Fatalf("unexpected encrypt result i=%v, %x", i, output[i*16:(i+1)*16])
		}
	}
	// Decrypt
	BS64.EncryptBlocks(dec, input, output)
	for i := 0; i < WordSize; i++ {
		if !bytes.Equal(key, input[i*16:(i+1)*16]) {
			t.Fatalf("unexpected decrypt result i=%v, %x", i, input[i*16:(i+1)*16])
		}
	}
}

func BenchmarkEncrypt64Blocks(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	enc := make([]uint32, rounds)
	dec := make([]uint32, rounds)
	input := make([]byte, BSBlockSize)
	output := make([]byte, BSBlockSize)
	for i := 0; i < WordSize; i++ {
		copy(input[i*16:], key)
	}

	expandKey(key, enc, dec)
	b.SetBytes(int64(BSBlockSize))
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS64.EncryptBlocks(enc, output, input)
	}
}
