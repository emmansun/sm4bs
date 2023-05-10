package sm4bs

import (
	"encoding/binary"
	"math/bits"
)

func encrypt64Blocks(xk []uint32, dst, src []byte) {
	_ = src[BSBlockSize-1] // early bounds check
	_ = dst[BSBlockSize-1] // early bounds check

	state := make([]uint64, BlockSize)
	bsTranspose(src, state)
	b0 := state[:32]
	b1 := state[32:64]
	b2 := state[64:96]
	b3 := state[96:]

	rk := make([]uint64, 32)

	for i := 0; i < 8; i++ {
		bsRoundKey(xk[i*4], rk)
		b0 = xor(b0, L(tao(xorRK(rk, b1, b2, b3))))
		bsRoundKey(xk[i*4+1], rk)
		b1 = xor(b1, L(tao(xorRK(rk, b2, b3, b0))))
		bsRoundKey(xk[i*4+2], rk)
		b2 = xor(b2, L(tao(xorRK(rk, b3, b0, b1))))
		bsRoundKey(xk[i*4+3], rk)
		b3 = xor(b3, L(tao(xorRK(rk, b0, b1, b2))))
	}
	copy(rk, b0)
	copy(state[:], b3)
	copy(state[96:], rk)
	copy(rk, b1)
	copy(state[32:], b2)
	copy(state[64:], rk)
	bsTransposeRev(state, dst)
}

// Key expansion algorithm.
func expandKey(key []byte, enc, dec []uint32) {
	// Encryption key setup.
	var i int
	var mk []uint32
	var k [rounds + 4]uint32
	nk := len(key) / 4
	mk = make([]uint32, nk)
	for i = 0; i < nk; i++ {
		mk[i] = binary.BigEndian.Uint32(key[4*i:])
		k[i] = mk[i] ^ fk[i]
	}

	for i = 0; i < rounds; i++ {
		k[i+4] = k[i] ^ t2(k[i+1]^k[i+2]^k[i+3]^ck[i])
		enc[i] = k[i+4]
	}

	// Derive decryption key from encryption key.
	if dec == nil {
		return
	}
	for i = 0; i < rounds; i++ {
		dec[i] = enc[rounds-1-i]
	}
}

//L'(B)
func l2(b uint32) uint32 {
	return b ^ bits.RotateLeft32(b, 13) ^ bits.RotateLeft32(b, 23)
}

func t2(in uint32) uint32 {
	var bytes [4]byte
	binary.BigEndian.PutUint32(bytes[:], in)
	for i := 0; i < 4; i++ {
		bytes[i] = sbox[bytes[i]]
	}
	return l2(binary.BigEndian.Uint32(bytes[:]))
}
