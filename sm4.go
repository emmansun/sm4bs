package sm4bs

import (
	"encoding/binary"
	"math/bits"
)

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
