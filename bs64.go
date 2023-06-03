package sm4bs

const BS64_BITBYTES = 8
const BS64_BYTEBYTES = 8 * 8

var BS64 bs64

type bs64 struct{}

func (bs64) bytes() int {
	return BS64_BITBYTES
}

func (bs bs64) tao(x, buffer []byte) []byte {
	const total = 4 * BS64_BYTEBYTES
	_ = x[total-1]
	_ = buffer[0]
	sbox64(&x[0], &buffer[0])
	sbox64(&x[BS64_BYTEBYTES], &buffer[0])
	sbox64(&x[2*BS64_BYTEBYTES], &buffer[0])
	sbox64(&x[3*BS64_BYTEBYTES], &buffer[0])
	return x
}

func (bs bs64) l(x, buffer []byte) []byte {
	l64(&x[0], &buffer[0])
	return buffer
}

func (bs bs64) xorRK(k uint32, rk, x1, x2, x3 []byte) []byte {
	xorRoundKey64(k, &x1[0], &x2[0], &x3[0], &rk[0])
	return rk
}

func (bs bs64) EncryptBlocks(xk []uint32, dst, src []byte) {
	bitSize := bs.bytes()
	size := BlockSize * bitSize
	_ = src[size-1] // early bounds check
	_ = dst[size-1] // early bounds check

	state := make([]byte, size)
	transpose64avx(&src[0], &state[0])
	b0 := state[:32*bitSize]
	b1 := state[32*bitSize : 64*bitSize]
	b2 := state[64*bitSize : 96*bitSize]
	b3 := state[96*bitSize:]

	buffer := make([]byte, 64*bitSize)
	rk := buffer[:32*bitSize]
	buffer = buffer[32*bitSize:]
	for i := 0; i < 8; i++ {
		_ = xk[3]

		xorRoundKey64(xk[0], &b1[0], &b2[0], &b3[0], &rk[0])
		sbox64(&rk[0], &buffer[0])
		sbox64(&rk[BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[2*BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[3*BS64_BYTEBYTES], &buffer[0])
		l64(&rk[0], &b0[0])

		xorRoundKey64(xk[1], &b2[0], &b3[0], &b0[0], &rk[0])
		sbox64(&rk[0], &buffer[0])
		sbox64(&rk[BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[2*BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[3*BS64_BYTEBYTES], &buffer[0])
		l64(&rk[0], &b1[0])

		xorRoundKey64(xk[2], &b3[0], &b0[0], &b1[0], &rk[0])
		sbox64(&rk[0], &buffer[0])
		sbox64(&rk[BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[2*BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[3*BS64_BYTEBYTES], &buffer[0])
		l64(&rk[0], &b2[0])

		xorRoundKey64(xk[3], &b0[0], &b1[0], &b2[0], &rk[0])
		sbox64(&rk[0], &buffer[0])
		sbox64(&rk[BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[2*BS64_BYTEBYTES], &buffer[0])
		sbox64(&rk[3*BS64_BYTEBYTES], &buffer[0])
		l64(&rk[0], &b3[0])

		xk = xk[4:]
	}
	transpose64RevAvx(&state[0], &dst[0])
}
