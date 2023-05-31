package sm4bs

var BS64 bs64

type bs64 struct{}

func (bs64) bytes() int {
	return 8
}

func (bs bs64) tao(x, buffer []byte) []byte {
	size := 8 * bs.bytes()
	for i := 0; i < 4; i++ {
		bytes := x[i*size : (i+1)*size]
		sbox64(&bytes[0], &buffer[0])
	}
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
		b0 = bs.l(bs.tao(bs.xorRK(xk[i*4], rk, b1, b2, b3), buffer), b0)
		b1 = bs.l(bs.tao(bs.xorRK(xk[i*4+1], rk, b2, b3, b0), buffer), b1)
		b2 = bs.l(bs.tao(bs.xorRK(xk[i*4+2], rk, b3, b0, b1), buffer), b2)
		b3 = bs.l(bs.tao(bs.xorRK(xk[i*4+3], rk, b0, b1, b2), buffer), b3)
	}
	transpose64RevAvx(&state[0], &dst[0])
}
