package sm4bs

import (
	"bytes"
	"encoding/binary"
	"math/bits"
	"testing"
)

func newByte64(b byte) []byte {
	ret := make([]byte, 8*BS64.bytes())
	ret1 := ret
	for i := 0; i < 8; i++ {
		if b&1 == 1 {
			for j := 0; j < BS64.bytes(); j++ {
				ret1[j] = 0xff
			}
		}
		ret1 = ret1[BS64.bytes():]
		b = b >> 1
	}
	return ret
}

func newUint32x64(x uint32) []byte {
	var bytes [4]byte
	binary.BigEndian.PutUint32(bytes[:], x)
	ret := newByte64(bytes[0])
	ret = append(ret, newByte64(bytes[1])...)
	ret = append(ret, newByte64(bytes[2])...)
	return append(ret, newByte64(bytes[3])...)
}

func TestSbox64(t *testing.T) {
	buffer := make([]byte, 64*BS64.bytes())
	for i := 0; i < 256; i++ {
		iBytes := newByte64(byte(i))
		sbox64(&iBytes[0], &buffer[0])
		expected := newByte64(sbox[i])
		if !bytes.Equal(iBytes, expected) {
			t.Fatalf("unexpected result for %v.", i)
		}
	}
}

func BenchmarkSbox64(b *testing.B) {
	buffer := make([]byte, 64*BS64.bytes())
	b.ReportAllocs()
	b.ResetTimer()
	x := newByte64(byte(14))

	for i := 0; i < b.N; i++ {
		sbox64(&x[0], &buffer[0])
	}
}

func TestTao64(t *testing.T) {
	x := make([]byte, 32*BS64.bytes())
	buffer := make([]byte, 64*BS64.bytes())
	expected := make([]byte, 32*BS64.bytes())

	copy(x, newByte64(byte(0)))
	copy(x[8*BS64.bytes():], newByte64(byte(1)))
	copy(x[16*BS64.bytes():], newByte64(byte(2)))
	copy(x[24*BS64.bytes():], newByte64(byte(3)))

	copy(expected, newByte64(sbox[0]))
	copy(expected[8*BS64.bytes():], newByte64(sbox[1]))
	copy(expected[16*BS64.bytes():], newByte64(sbox[2]))
	copy(expected[24*BS64.bytes():], newByte64(sbox[3]))

	ret := BS64.tao(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected tao result %x, %x", ret, expected)
	}
}

func BenchmarkTao64(b *testing.B) {
	x := make([]byte, 32*BS64.bytes())
	buffer := make([]byte, 64*BS64.bytes())
	copy(x, newByte64(byte(0)))
	copy(x[8*BS64.bytes():], newByte64(byte(1)))
	copy(x[16*BS64.bytes():], newByte64(byte(2)))
	copy(x[24*BS64.bytes():], newByte64(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS64.tao(x, buffer)
	}
}

func TestL64(t *testing.T) {
	buffer := newUint32x64(0xa0a7aeb5)
	b := uint32(0xa0a7aeb5) ^ uint32(0xe0e7eef5) ^ bits.RotateLeft32(0xe0e7eef5, 2) ^ bits.RotateLeft32(0xe0e7eef5, 10) ^ bits.RotateLeft32(0xe0e7eef5, 18) ^ bits.RotateLeft32(0xe0e7eef5, 24)
	expected := newUint32x64(b)

	x := newUint32x64(0xe0e7eef5)

	ret := BS64.l(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected l256 result, expected %x, got %x", expected, ret)
	}
}

func BenchmarkL64(b *testing.B) {
	x := make([]byte, 32*BS64.bytes())
	buffer := make([]byte, 64*BS64.bytes())
	copy(x, newByte64(byte(0)))
	copy(x[8*BS64.bytes():], newByte64(byte(1)))
	copy(x[16*BS64.bytes():], newByte64(byte(2)))
	copy(x[24*BS64.bytes():], newByte64(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS64.l(x, buffer)
	}
}

func TestXorRK64(t *testing.T) {
	b0 := newUint32x64(0xe0e7eef5)
	b1 := newUint32x64(0xc0c7ced5)
	b2 := newUint32x64(0xa0a7aeb5)
	rk := make([]byte, 32*BS64.bytes())
	k := uint32(0xa3b1bac6)
	BS64.xorRK(k, rk, b0, b1, b2)
	expected := newUint32x64(k ^ 0xe0e7eef5 ^ 0xc0c7ced5 ^ 0xa0a7aeb5)
	if !bytes.Equal(expected, rk) {
		t.Fatalf("unexpected xorRK result %x, %x", rk, expected)
	}
}

func BenchmarkXorRK64(b *testing.B) {
	b0 := make([]byte, 32*BS64.bytes())
	b1 := make([]byte, 32*BS64.bytes())
	b2 := make([]byte, 32*BS64.bytes())
	rk := make([]byte, 32*BS64.bytes())
	k := uint32(0xa3b1bac6)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BS64.xorRK(k, rk, b0, b1, b2)
	}
}

func TestBS64EncryptBlocks(t *testing.T) {
	bitSize := BS64.bytes()
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	expected := []byte{0x68, 0x1e, 0xdf, 0x34, 0xd2, 0x06, 0x96, 0x5e, 0x86, 0xb3, 0xe9, 0x4f, 0x53, 0x6e, 0x42, 0x46}
	enc := make([]uint32, rounds)
	dec := make([]uint32, rounds)
	input := make([]byte, BlockSize*bitSize)
	output := make([]byte, BlockSize*bitSize)
	for i := 0; i < bitSize*8; i++ {
		copy(input[i*16:], key)
	}

	expandKey(key, enc, dec)
	//Encrypt
	BS64.EncryptBlocks(enc, output, input)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(expected, output[i*16:(i+1)*16]) {
			t.Fatalf("unexpected encrypt result i=%v, %x", i, output[i*16:(i+1)*16])
		}
	}
	// Decrypt
	BS64.EncryptBlocks(dec, input, output)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(key, input[i*16:(i+1)*16]) {
			t.Fatalf("unexpected decrypt result i=%v, %x", i, input[i*16:(i+1)*16])
		}
	}
}

func BenchmarkEncrypt64Blocks(b *testing.B) {
	bitSize := BS64.bytes()
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	enc := make([]uint32, rounds)
	dec := make([]uint32, rounds)
	input := make([]byte, BlockSize*bitSize)
	output := make([]byte, BlockSize*bitSize)
	for i := 0; i < bitSize*8; i++ {
		copy(input[i*16:], key)
	}

	expandKey(key, enc, dec)
	b.SetBytes(int64(bitSize * BlockSize))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS64.EncryptBlocks(enc, output, input)
	}
}
