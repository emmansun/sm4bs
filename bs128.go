//go:build amd64 && gc && !purego

package sm4bs

const BS128_BITBYTES = 16
const BS128_BYTEBYTES = 8 * 16

var BS128 bs128

type bs128 struct{}

func (bs128) bytes() int {
	return BS128_BITBYTES
}

func (bs bs128) tao(x, buffer []byte) []byte {
	const total = 4 * BS128_BYTEBYTES
	_ = x[total-1]
	_ = buffer[0]
	sbox128(&x[0], &buffer[0])
	sbox128(&x[BS128_BYTEBYTES], &buffer[0])
	sbox128(&x[2*BS128_BYTEBYTES], &buffer[0])
	sbox128(&x[3*BS128_BYTEBYTES], &buffer[0])
	return x
}

func (bs bs128) xor32(x1, x2 []byte) []byte {
	xor32x128avx(32*bs.bytes(), &x1[0], &x2[0], &x1[0])
	return x1
}

func (bs bs128) xorRK(k uint32, rk, x1, x2, x3 []byte) []byte {
	xorRoundKey128(k, &x1[0], &x2[0], &x3[0], &rk[0])
	return rk
}

func (bs bs128) l(x, buffer []byte) []byte {
	l128(&x[0], &buffer[0])
	return buffer
}

func (bs bs128) EncryptBlocks(xk []uint32, dst, src []byte) {
	bitSize := bs.bytes()
	size := BlockSize * bitSize
	_ = src[size-1] // early bounds check
	_ = dst[size-1] // early bounds check

	state := make([]byte, size)
	transpose128avx(&src[0], &state[0])
	b0 := state[:32*bitSize]
	b1 := state[32*bitSize : 64*bitSize]
	b2 := state[64*bitSize : 96*bitSize]
	b3 := state[96*bitSize:]

	buffer := make([]byte, 64*bitSize)
	rk := buffer[:32*bitSize]
	buffer = buffer[32*bitSize:]
	for i := 0; i < 8; i++ {
		_ = xk[3]

		xorRoundKey128(xk[0], &b1[0], &b2[0], &b3[0], &rk[0])
		sbox128(&rk[0], &buffer[0])
		sbox128(&rk[BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[2*BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[3*BS128_BYTEBYTES], &buffer[0])
		l128(&rk[0], &b0[0])

		xorRoundKey128(xk[1], &b2[0], &b3[0], &b0[0], &rk[0])
		sbox128(&rk[0], &buffer[0])
		sbox128(&rk[BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[2*BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[3*BS128_BYTEBYTES], &buffer[0])
		l128(&rk[0], &b1[0])

		xorRoundKey128(xk[2], &b3[0], &b0[0], &b1[0], &rk[0])
		sbox128(&rk[0], &buffer[0])
		sbox128(&rk[BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[2*BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[3*BS128_BYTEBYTES], &buffer[0])
		l128(&rk[0], &b2[0])

		xorRoundKey128(xk[3], &b0[0], &b1[0], &b2[0], &rk[0])
		sbox128(&rk[0], &buffer[0])
		sbox128(&rk[BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[2*BS128_BYTEBYTES], &buffer[0])
		sbox128(&rk[3*BS128_BYTEBYTES], &buffer[0])
		l128(&rk[0], &b3[0])

		xk = xk[4:]
	}
	transpose128RevAvx(&state[0], &dst[0])
}
