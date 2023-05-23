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

func (bs bs128) xorRK(rk, x1, x2, x3 []byte) []byte {
	xor32x128(&rk[0], &x1[0], &rk[0])
	xor32x128(&rk[0], &x2[0], &rk[0])
	xor32x128(&rk[0], &x3[0], &rk[0])
	return rk
}

func (bs bs128) roundKey(in uint32, out []byte) {
	expandRoundKey128(in, &out[0])
}

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 |  0  1  2  3  4  5  6  7
// 22 23 24 25 26 27 28 29 | 14 15 16 17 18 19 20 21 |  6  7  8  9 10 11 12 13 | 30 31  0  1  2  3  4  5
func (bs bs128) rotateLeft32_2(x, ret []byte) []byte {
	size := bs.bytes()

	copy(ret[2*size:], x)

	copy(ret, x[(16-2)*size:16*size])
	//copy(ret[2*size:], x[:(8-2)*size])

	copy(ret[8*size:], x[(24-2)*size:24*size])
	//copy(ret[(8+2)*size:], x[8*size:(16-2)*size])

	copy(ret[16*size:], x[(32-2)*size:])
	//copy(ret[(16+2)*size:], x[16*size:(24-2)*size])

	copy(ret[24*size:], x[(8-2)*size:8*size])
	//copy(ret[(24+2)*size:], x[24*size:(32-2)*size])

	return ret
}

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 | 0  1  2  3  4  5  6  7
//  0  1  2  3  4  5  6  7 | 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 | 8  9 10 11 12 13 14 15
func (bs bs128) rotateLeft32_24(x, ret []byte) []byte {
	size := bs.bytes()

	copy(ret, x[24*size:32*size])
	copy(ret[8*size:], x[:24*size])

	return ret
}

func (bs bs128) rotateLeft32_8(x, ret []byte) []byte {
	size := bs.bytes()

	copy(ret, x[8*size:32*size])
	copy(ret[24*size:], x[:8*size])

	return ret
}

func (bs bs128) l(x, buffer []byte) []byte {
	size := bs.bytes()

	// rotateLeft32_24 first
	ret1 := bs.rotateLeft32_24(x, buffer[:32*size])
	bs.xor32(ret1, x)
	// rotateLeft32_2
	ret2 := bs.rotateLeft32_2(x, buffer[32*size:])
	bs.xor32(ret1, ret2)

	// rotateLeft32_8 based one rotateLeft32_2
	bs.rotateLeft32_8(ret2, x)
	bs.xor32(ret1, x)

	// rotateLeft32_8 based one rotateLeft32_10
	bs.rotateLeft32_8(x, ret2)
	return bs.xor32(ret1, ret2)
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

	rk := make([]byte, 32*bitSize)
	buffer := make([]byte, 64*bitSize)
	for i := 0; i < 8; i++ {
		bs.roundKey(xk[i*4], rk)
		b0 = bs.xor32(b0, bs.l(bs.tao(bs.xorRK(rk, b1, b2, b3), buffer), buffer))
		bs.roundKey(xk[i*4+1], rk)
		b1 = bs.xor32(b1, bs.l(bs.tao(bs.xorRK(rk, b2, b3, b0), buffer), buffer))
		bs.roundKey(xk[i*4+2], rk)
		b2 = bs.xor32(b2, bs.l(bs.tao(bs.xorRK(rk, b3, b0, b1), buffer), buffer))
		bs.roundKey(xk[i*4+3], rk)
		b3 = bs.xor32(b3, bs.l(bs.tao(bs.xorRK(rk, b0, b1, b2), buffer), buffer))
	}
	copy(rk, b0)
	copy(state[:], b3)
	copy(state[96*bitSize:], rk)
	copy(rk, b1)
	copy(state[32*bitSize:], b2)
	copy(state[64*bitSize:], rk)
	transpose128(&state[0], &dst[0])
}
