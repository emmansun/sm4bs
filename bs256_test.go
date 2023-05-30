//go:build amd64 && gc && !purego

package sm4bs

import (
	"bytes"
	"encoding/binary"
	"math/bits"
	"testing"
)

func newByte256(b byte) []byte {
	ret := make([]byte, 8*BS256.bytes())
	ret1 := ret
	for i := 0; i < 8; i++ {
		if b&1 == 1 {
			for j := 0; j < BS256.bytes(); j++ {
				ret1[j] = 0xff
			}
		}
		ret1 = ret1[BS256.bytes():]
		b = b >> 1
	}
	return ret
}

func newUint32x256(x uint32) []byte {
	var bytes [4]byte
	binary.BigEndian.PutUint32(bytes[:], x)
	ret := newByte256(bytes[0])
	ret = append(ret, newByte256(bytes[1])...)
	ret = append(ret, newByte256(bytes[2])...)
	return append(ret, newByte256(bytes[3])...)
}

func TestL256(t *testing.T) {
	buffer := make([]byte, 32*BS256.bytes())
	b := uint32(0xe0e7eef5) ^ bits.RotateLeft32(0xe0e7eef5, 2) ^ bits.RotateLeft32(0xe0e7eef5, 10) ^ bits.RotateLeft32(0xe0e7eef5, 18) ^ bits.RotateLeft32(0xe0e7eef5, 24)
	expected := newUint32x256(b)

	x := newUint32x256(0xe0e7eef5)

	ret := BS256.l(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected l256 result, expected %x, got %x", expected, ret)
	}
}

func BenchmarkL256(b *testing.B) {
	x := make([]byte, 32*BS256.bytes())
	buffer := make([]byte, 64*BS256.bytes())
	copy(x, newByte256(byte(0)))
	copy(x[8*BS256.bytes():], newByte256(byte(1)))
	copy(x[16*BS256.bytes():], newByte256(byte(2)))
	copy(x[24*BS256.bytes():], newByte256(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS256.l(x, buffer)
	}
}

func TestSbox256(t *testing.T) {
	buffer := make([]byte, 64*BS256.bytes())
	for i := 0; i < 256; i++ {
		iBytes := newByte256(byte(i))
		sbox256avx2(&iBytes[0], &buffer[0])
		expected := newByte256(sbox[i])
		if !bytes.Equal(iBytes, expected) {
			t.Fatalf("unexpected result for %v.", i)
		}
	}
}

func BenchmarkSbox256(b *testing.B) {
	buffer := make([]byte, 64*BS256.bytes())
	b.ReportAllocs()
	b.ResetTimer()
	x := newByte256(byte(14))

	for i := 0; i < b.N; i++ {
		sbox256avx2(&x[0], &buffer[0])
	}
}

func TestTao256(t *testing.T) {
	x := make([]byte, 32*BS256.bytes())
	buffer := make([]byte, 64*BS256.bytes())
	expected := make([]byte, 32*BS256.bytes())

	copy(x, newByte256(byte(0)))
	copy(x[8*BS256.bytes():], newByte256(byte(1)))
	copy(x[16*BS256.bytes():], newByte256(byte(2)))
	copy(x[24*BS256.bytes():], newByte256(byte(3)))

	copy(expected, newByte256(sbox[0]))
	copy(expected[8*BS256.bytes():], newByte256(sbox[1]))
	copy(expected[16*BS256.bytes():], newByte256(sbox[2]))
	copy(expected[24*BS256.bytes():], newByte256(sbox[3]))

	ret := BS256.tao(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected tao result %x, %x", ret, expected)
	}
}

func BenchmarkTao256(b *testing.B) {
	x := make([]byte, 32*BS256.bytes())
	buffer := make([]byte, 64*BS256.bytes())
	copy(x, newByte256(byte(0)))
	copy(x[8*BS256.bytes():], newByte256(byte(1)))
	copy(x[16*BS256.bytes():], newByte256(byte(2)))
	copy(x[24*BS256.bytes():], newByte256(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS256.tao(x, buffer)
	}
}

func TestXor32X256(t *testing.T) {
	b0 := newUint32x256(0xe0e7eef5)
	b1 := newUint32x256(0xc0c7ced5)

	BS256.xor32(b0, b1)
	if !bytes.Equal(b0, newUint32x256(0xe0e7eef5^0xc0c7ced5)) {
		t.Fatal("bs256 xor32 fail")
	}
}

func TestXorRK256(t *testing.T) {
	b0 := newUint32x256(0xe0e7eef5)
	b1 := newUint32x256(0xc0c7ced5)
	b2 := newUint32x256(0xa0a7aeb5)
	rk := make([]byte, 32*BS256.bytes())
	k := uint32(0xa3b1bac6)
	BS256.xorRK(k, rk, b0, b1, b2)
	expected := newUint32x256(k ^ 0xe0e7eef5 ^ 0xc0c7ced5 ^ 0xa0a7aeb5)
	if !bytes.Equal(expected, rk) {
		t.Fatalf("unexpected xorRK result %x, %x", rk, expected)
	}
}

func BenchmarkXorRK256(b *testing.B) {
	b0 := make([]byte, 32*BS256.bytes())
	b1 := make([]byte, 32*BS256.bytes())
	b2 := make([]byte, 32*BS256.bytes())
	rk := make([]byte, 32*BS256.bytes())
	k := uint32(0xa3b1bac6)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BS256.xorRK(k, rk, b0, b1, b2)
	}
}

func BenchmarkXor32x256(b *testing.B) {
	b0 := make([]byte, 32*BS256.bytes())
	b1 := make([]byte, 32*BS256.bytes())
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BS256.xor32(b0, b1)
	}
}

func TestBS256EncryptBlocks(t *testing.T) {
	bitSize := BS256.bytes()
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
	BS256.EncryptBlocks(enc, output, input)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(expected, output[i*16:(i+1)*16]) {
			t.Fatalf("unexpected encrypt result i=%v, %x", i, output[i*16:(i+1)*16])
		}
	}
	// Decrypt
	BS256.EncryptBlocks(dec, input, output)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(key, input[i*16:(i+1)*16]) {
			t.Fatalf("unexpected decrypt result i=%v, %x", i, input[i*16:(i+1)*16])
		}
	}
}

func BenchmarkEncrypt256Blocks(b *testing.B) {
	bitSize := BS256.bytes()
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
		BS256.EncryptBlocks(enc, output, input)
	}
}
