//go:build amd64 && gc && !purego

package sm4bs

import (
	"bytes"
	"math/bits"
	"testing"
)

func newByte128(b byte) []byte {
	ret := make([]byte, 8*BS128.bytes())
	ret1 := ret
	for i := 0; i < 8; i++ {
		if b&1 == 1 {
			for j := 0; j < BS128.bytes(); j++ {
				ret1[j] = 0xff
			}
		}
		ret1 = ret1[BS128.bytes():]
		b = b >> 1
	}
	return ret
}

func TestSbox(t *testing.T) {
	buffer := make([]byte, 64*BS128.bytes())
	for i := 0; i < 256; i++ {
		iBytes := newByte128(byte(i))
		BS128.sbox(iBytes, buffer)
		expected := newByte128(sbox[i])
		if !bytes.Equal(iBytes, expected) {
			t.Fatalf("unexpected result for %v.", i)
		}
	}
}

func BenchmarkSbox128(b *testing.B) {
	buffer := make([]byte, 64*BS128.bytes())
	b.ReportAllocs()
	b.ResetTimer()
	x := newByte128(byte(14))

	for i := 0; i < b.N; i++ {
		BS128.sbox(x, buffer)
	}
}

func TestTao(t *testing.T) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 64*BS128.bytes())
	expected := make([]byte, 32*BS128.bytes())

	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	copy(expected, newByte128(sbox[0]))
	copy(expected[8*BS128.bytes():], newByte128(sbox[1]))
	copy(expected[16*BS128.bytes():], newByte128(sbox[2]))
	copy(expected[24*BS128.bytes():], newByte128(sbox[3]))

	ret := BS128.tao(x, buffer)
	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected tao result")
	}
}

func BenchmarkTao128(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 64*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.tao(x, buffer)
	}
}

func TestRotateLeft32_2(t *testing.T) {
	x := make([]byte, 32*BS128.bytes())
	expected := make([]byte, 32*BS128.bytes())
	expected1 := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	b := bits.RotateLeft32(0x00010203, 2)
	BS128.roundKey(b, expected)

	copy(expected1, newByte128(byte(0)))
	copy(expected1[8*BS128.bytes():], newByte128(byte(4)))
	copy(expected1[16*BS128.bytes():], newByte128(byte(8)))
	copy(expected1[24*BS128.bytes():], newByte128(byte(0x0c)))

	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	ret := BS128.rotateLeft32_2(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected rotateLeft32_2 result, expected %x, got %x", expected, ret)
	}
}

func BenchmarkRotateLeft32_2(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.rotateLeft32_2(x, buffer)
	}
}

func TestRotateLeft32_10(t *testing.T) {
	x := make([]byte, 32*BS128.bytes())
	expected := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 64*BS128.bytes())
	b := bits.RotateLeft32(0x00010203, 10)
	BS128.roundKey(b, expected)

	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	BS128.rotateLeft32_2(x, buffer)
	BS128.rotateLeft32_8(buffer, x)

	if !bytes.Equal(x, expected) {
		t.Fatalf("unexpected rotateLeft32_2 result, expected %x, got %x", expected, x)
	}
}

func BenchmarkRotateLeft32_10(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.rotateLeft32_10(x, buffer)
	}
}

func TestRotateLeft32_18(t *testing.T) {
	x := make([]byte, 32*BS128.bytes())
	expected := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	b := bits.RotateLeft32(0x00010203, 18)
	BS128.roundKey(b, expected)

	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	BS128.rotateLeft32_2(x, buffer)
	BS128.rotateLeft32_8(buffer, x)
	BS128.rotateLeft32_8(x, buffer)

	if !bytes.Equal(buffer, expected) {
		t.Fatalf("unexpected rotateLeft32_2 result, expected %x, got %x", expected, buffer)
	}
}

func BenchmarkRotateLeft32_18(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.rotateLeft32_18(x, buffer)
	}
}

func BenchmarkRotateLeft32_24(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 32*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.rotateLeft32_24(x, buffer)
	}
}

func TestL128(t *testing.T) {
	x := make([]byte, 32*BS128.bytes())
	expected := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 64*BS128.bytes())
	b := 0x00010203 ^ bits.RotateLeft32(0x00010203, 2) ^ bits.RotateLeft32(0x00010203, 10) ^ bits.RotateLeft32(0x00010203, 18) ^ bits.RotateLeft32(0x00010203, 24)
	BS128.roundKey(b, expected)

	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	ret := BS128.l(x, buffer)

	if !bytes.Equal(ret, expected) {
		t.Fatalf("unexpected rotateLeft32_2 result, expected %x, got %x", expected, ret)
	}
}

func BenchmarkL128(b *testing.B) {
	x := make([]byte, 32*BS128.bytes())
	buffer := make([]byte, 64*BS128.bytes())
	copy(x, newByte128(byte(0)))
	copy(x[8*BS128.bytes():], newByte128(byte(1)))
	copy(x[16*BS128.bytes():], newByte128(byte(2)))
	copy(x[24*BS128.bytes():], newByte128(byte(3)))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.l(x, buffer)
	}
}

func TestBS128EncryptBlocks(t *testing.T) {
	bitSize := BS128.bytes()
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
	BS128.EncryptBlocks(enc, output, input)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(expected, output[i*16:(i+1)*16]) {
			t.Fatalf("unexpected encrypt result i=%v, %x", i, output[i*16:(i+1)*16])
		}
	}
	// Decrypt
	BS128.EncryptBlocks(dec, input, output)
	for i := 0; i < bitSize*8; i++ {
		if !bytes.Equal(key, input[i*16:(i+1)*16]) {
			t.Fatalf("unexpected decrypt result i=%v, %x", i, input[i*16:(i+1)*16])
		}
	}
}

func BenchmarkEncrypt128Blocks(b *testing.B) {
	bitSize := BS128.bytes()
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	enc := make([]uint32, rounds)
	dec := make([]uint32, rounds)
	input := make([]byte, BlockSize*bitSize)
	output := make([]byte, BlockSize*bitSize)
	for i := 0; i < bitSize*8; i++ {
		copy(input[i*16:], key)
	}

	expandKey(key, enc, dec)
	b.SetBytes(int64(bitSize * 8 * BlockSize))

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		BS128.EncryptBlocks(enc, output, input)
	}
}
