//go:build amd64 && gc && !purego

package sm4bs

var BS256 bs256

type bs256 struct{}

func (bs256) bytes() int {
	return 32
}

func (bs bs256) l(x, buffer []byte) []byte {
	l256(&x[0], &buffer[0])
	return buffer
}

func (bs bs256) tao(x, buffer []byte) []byte {
	size := 8 * bs.bytes()
	for i := 0; i < 4; i++ {
		bytes := x[i*size : (i+1)*size]
		sbox256avx2(&bytes[0], &buffer[0])
	}
	return x
}

func (bs bs256) xor32(x1, x2 []byte) []byte {
	xor32x128avx(32*bs.bytes(), &x1[0], &x2[0], &x1[0])
	return x1
}

func (bs bs256) xorRK(k uint32, rk, x1, x2, x3 []byte) []byte {
	xorRoundKey256avx2(k, &x1[0], &x2[0], &x3[0], &rk[0])
	return rk
}

func (bs bs256) EncryptBlocks(xk []uint32, dst, src []byte) {
	bitSize := bs.bytes()
	size := BlockSize * bitSize
	_ = src[size-1] // early bounds check
	_ = dst[size-1] // early bounds check

	state := make([]byte, size)
	transpose256avx(&src[0], &state[0])
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
	transpose256RevAvx(&state[0], &dst[0])
}
