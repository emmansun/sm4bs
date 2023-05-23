//go:build amd64 && gc && !purego

package sm4bs

var BS128 bs128

type bs128 struct{}

func (bs128) bytes() int {
	return 16
}

func (bs bs128) tao(x, buffer []byte) []byte {
	size := 8 * bs.bytes()
	for i := 0; i < 4; i++ {
		bytes := x[i*size : (i+1)*size]
		sbox128(&bytes[0], &buffer[0])
	}
	return x
}

func (bs bs128) xor32(x1, x2 []byte) []byte {
	xor32x128(&x1[0], &x2[0], &x1[0])
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
	transpose128(&src[0], &state[0])
	b0 := state[:32*bitSize]
	b1 := state[32*bitSize : 64*bitSize]
	b2 := state[64*bitSize : 96*bitSize]
	b3 := state[96*bitSize:]

	buffer := make([]byte, 64*bitSize)
	rk := buffer[:32*bitSize]
	buffer = buffer[32*bitSize:]
	for i := 0; i < 8; i++ {
		b0 = bs.xor32(b0, bs.l(bs.tao(bs.xorRK(xk[i*4], rk, b1, b2, b3), buffer), buffer))
		b1 = bs.xor32(b1, bs.l(bs.tao(bs.xorRK(xk[i*4+1], rk, b2, b3, b0), buffer), buffer))
		b2 = bs.xor32(b2, bs.l(bs.tao(bs.xorRK(xk[i*4+2], rk, b3, b0, b1), buffer), buffer))
		b3 = bs.xor32(b3, bs.l(bs.tao(bs.xorRK(xk[i*4+3], rk, b0, b1, b2), buffer), buffer))
	}
	copy(rk, b0)
	copy(state[:], b3)
	copy(state[96*bitSize:], rk)
	copy(rk, b1)
	copy(state[32*bitSize:], b2)
	copy(state[64*bitSize:], rk)
	transpose128(&state[0], &dst[0])
}
