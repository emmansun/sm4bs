//go:build amd64 && gc && !purego

package sm4bs

var BS128 bs128

type bs128 struct{}

func (bs128) bytes() int {
	return 16
}

func (bs bs128) xor(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	xor128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) nxor(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	nxor128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) and(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	and128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) nand(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	nand128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) or(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	or128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) nor(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	nor128(&x[0], &y[0], &ret[0])
	return ret
}

func (bs bs128) not(x []byte) []byte {
	not128(&x[0])
	return x
}

// input bytes = 8 x 16 bytes
// g = 8 x 16 bytes, m = 10 x 16 bytes
func (bs bs128) input(bytes, g, m []byte) {
	size := bs.bytes()

	b0 := bytes[:size]
	b1 := bytes[size : 2*size]
	b2 := bytes[2*size : 3*size]
	b3 := bytes[3*size : 4*size]
	b4 := bytes[4*size : 5*size]
	b5 := bytes[5*size : 6*size]
	b6 := bytes[6*size : 7*size]
	b7 := bytes[7*size:]

	xor128(&b7[0], &b5[0], &m[6*size])         // set m6 = t1 first
	nxor128(&b5[0], &b1[0], &g[4*size])        // g4 = t2
	nxor128(&b0[0], &g[4*size], &m[size])      // m1 = t3
	xor128(&b6[0], &b2[0], &m[4*size])         // m4 = t4
	xor128(&b3[0], &m[size], &g[3*size])       // g3 = t5
	xor128(&b4[0], &m[6*size], &m[0])          // m0 = t6
	xor128(&b1[0], &g[3*size], &g[size])       // g1 = t7
	xor128(&b1[0], &m[4*size], &m[2*size])     // m2 = t8
	xor128(&m[0], &m[2*size], &m[8*size])      // m8 = t9
	xor128(&m[0], &g[size], &g[0])             // g0 = t10
	nxor128(&b3[0], &m[6*size], &m[5*size])    // m5 = t11
	nxor128(&b6[0], &m[8*size], &m[9*size])    // m9 = t12
	xor128(&m[4*size], &g[0], &g[2*size])      // g2 = t13
	xor128(&g[4*size], &m[5*size], &g[6*size]) // g6 = t14
	xor128(&m[9*size], &g[6*size], &g[7*size]) // g7 = t15
	xor128(&m[size], &m[9*size], &m[3*size])   // m3 = t16
	xor128(&m[5*size], &m[3*size], &m[7*size]) // m7 = t17

	copy(g[5*size:], b0)
	bs.not(g[5*size:])

	copy(m[6*size:], b1)
}

func (bs bs128) top(g, m, p, buffer []byte) {
	size := bs.bytes()

	t13 := buffer
	t15 := buffer[size:]
	t14 := buffer[2*size:]
	t6 := buffer[3*size:]
	t8 := buffer[4*size:]
	nand128(&g[5*size], &g[size], &t13[0])
	nand128(&m[size], &m[0], &t15[0])
	nand128(&g[4*size], &g[0], &t14[0])
	nand128(&g[7*size], &g[3*size], &p[0])    // p0
	nand128(&m[9*size], &m[8*size], &p[size]) // p1
	nor128(&g[6*size], &g[2*size], &t6[0])
	nor128(&m[9*size], &m[8*size], &t8[0])

	xor128(&t13[0], &t15[0], &t13[0])
	xor128(&t14[0], &t15[0], &t14[0])
	xor128(&p[0], &t13[0], &t15[0]) // use p0
	xor128(&p[1*size], &t14[0], &t14[0])

	nor128(&m[7*size], &m[6*size], &p[2*size])  // p2
	nand128(&m[3*size], &m[2*size], &p[3*size]) // p3
	xor128(&p[2*size], &p[3*size], &p[3*size])  // use p3
	nor128(&g[7*size], &g[3*size], &p[2*size])
	xor128(&p[2*size], &t14[0], &p[1*size])

	nand128(&m[5*size], &m[4*size], &p[2*size])
	nor128(&m[3*size], &m[2*size], &p[0])
	xor128(&p[2*size], &p[0], &p[2*size])
	xor128(&t6[0], &t15[0], &p[0])

	xor128(&p[0], &p[3*size], &p[3*size])
	xor128(&p[1*size], &p[2*size], &p[2*size])
	xor128(&t8[0], &t15[0], &p[1*size])
	xor128(&t6[0], &t14[0], &p[0])
}

func (bs bs128) middle(p, l, buffer []byte) {
	size := bs.bytes()

	t8 := buffer
	t4 := buffer[size:]
	nand128(&p[3*size], &p[0], &l[1*size])
	nor128(&l[1*size], &p[2*size], &t8[0])
	nand128(&l[1*size], &p[2*size], &l[1*size])

	nand128(&p[2*size], &p[0], &t4[0])
	xor128(&p[1*size], &t4[0], &t4[0])
	nor128(&p[2*size], &t4[0], &l[3*size])

	nand128(&p[1*size], &t4[0], &l[2*size])
	nor128(&p[3*size], &t4[0], &t4[0])

	nor128(&t4[0], &t8[0], &t8[0])
	nxor128(&l[3*size], &t4[0], &t4[0])

	nxor128(&t4[0], &p[3*size], &l[0])
	nand128(&l[2*size], &t8[0], &l[2*size])
	nand128(&t8[0], &p[1*size], &l[3*size])

	nxor128(&l[3*size], &p[0], &l[3*size])
	nand128(&t4[0], &l[1*size], &l[1*size])

}

func (bs bs128) bottom(g, m, l, e []byte) {
	size := bs.bytes()

	nand128(&g[5*size], &l[size], &e[size])      // e0
	nand128(&g[4*size], &l[0], &e[2*size])       // e2
	nand128(&g[7*size], &l[3*size], &e[7*size])  // e7
	nand128(&g[6*size], &l[2*size], &e[8*size])  // e8
	nand128(&g[1*size], &l[1*size], &e[10*size]) // e10
	nand128(&g[0], &l[0], &e[11*size])           // e11
	nand128(&g[3*size], &l[3*size], &e[16*size]) // e16
	nand128(&g[2*size], &l[2*size], &e[17*size]) // e17

	// k1 = e14
	// k1 := bs.xor(l[1*size:], l[0*size:]) // k0
	xor128(&l[1*size], &l[0], &e[14*size])
	nand128(&m[1*size], &e[14*size], &e[0]) // e0
	nand128(&m[0], &e[14*size], &e[9*size]) // e9

	xor128(&l[3*size], &l[2*size], &e[14*size])   // k4
	nand128(&m[9*size], &e[14*size], &e[6*size])  // e6
	nand128(&m[8*size], &e[14*size], &e[15*size]) // e15

	// k2 = e5
	// k2 := bs.xor(l[2*size:], l[0*size:])     // k2
	xor128(&l[2*size], &l[0], &e[5*size])
	nand128(&m[5*size], &e[5*size], &e[4*size])  // e4
	nand128(&m[4*size], &e[5*size], &e[13*size]) // e13

	xor128(&l[3*size], &l[1*size], &e[14*size])   // k3
	nand128(&m[7*size], &e[14*size], &e[3*size])  // e3
	nand128(&m[6*size], &e[14*size], &e[12*size]) // e12

	xor128(&e[5*size], &e[14*size], &e[14*size])  // k1
	nand128(&m[3*size], &e[14*size], &e[5*size])  // e5
	nand128(&m[2*size], &e[14*size], &e[14*size]) // e14

}

func (bs bs128) output(e, ret []byte) {
	size := bs.bytes()

	xor128(&e[0], &e[1*size], &ret[0])
	xor128(&e[2*size], &e[1*size], &e[0])
	xor128(&e[3*size], &e[4*size], &e[1*size])
	xor128(&e[5*size], &e[4*size], &e[2*size])

	xor128(&e[6*size], &e[7*size], &e[3*size])
	xor128(&e[8*size], &e[7*size], &e[4*size])

	xor128(&e[9*size], &e[10*size], &e[5*size])
	xor128(&e[11*size], &e[10*size], &e[6*size])

	xor128(&e[12*size], &e[13*size], &e[7*size])
	xor128(&e[14*size], &e[13*size], &e[8*size])

	xor128(&e[15*size], &e[16*size], &e[9*size])
	xor128(&e[17*size], &e[16*size], &e[10*size])

	xor128(&e[8*size], &e[6*size], &e[8*size]) // t1, use r9
	xor128(&e[0], &e[8*size], &e[0])           // t2, use r1
	xor128(&e[4*size], &e[2*size], &e[4*size]) // t4, use r5
	xor128(&e[2*size], &e[0], &e[2*size])      // t3, use r3

	// used r0, r2, r4, r6, r7, r8, r10, r11,
	// r1, r3, r5, r9
	xor128(&e[3*size], &e[4*size], &ret[6*size])  // t5, use ret6 first
	xor128(&e[3*size], &ret[0], &e[3*size])       // t6, use r4
	xor128(&e[10*size], &e[6*size], &ret[4*size]) // t7, use ret4 first
	xor128(&e[8*size], &e[4*size], &ret[5*size])  // t8

	xor128(&e[8*size], &e[3*size], &ret[2*size])    // t9
	xor128(&e[1*size], &ret[6*size], &ret[6*size])  // t0, use ret6 first
	xor128(&e[9*size], &e[7*size], &ret[3*size])    // t11
	nxor128(&e[2*size], &ret[3*size], &ret[1*size]) // t12

	xor128(&ret[6*size], &ret[1*size], &ret[6*size]) // t13
	nxor128(&e[2*size], &ret[4*size], &ret[4*size])  // t14
	nxor128(&e[9*size], &e[5*size], &ret[7*size])    // t15
	xor128(&e[3*size], &ret[4*size], &ret[0])        // t16
}

func (bs bs128) sbox(bytes, buffer []byte) {
	size := bs.bytes()
	bs.input(bytes, buffer, buffer[8*size:])
	bs.top(buffer, buffer[8*size:], buffer[22*size:], buffer[26*size:])
	bs.middle(buffer[22*size:], buffer[18*size:], buffer[26*size:])
	bs.bottom(buffer, buffer[8*size:], buffer[18*size:], buffer[22*size:])
	bs.output(buffer[22*size:], bytes)
}

func (bs bs128) tao(x, buffer []byte) []byte {
	size := 8 * bs.bytes()
	for i := 0; i < 4; i++ {
		bs.sbox(x[i*size:(i+1)*size], buffer)
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

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 |  0  1  2  3  4  5  6  7
// 14 15 16 17 18 19 20 21 |  6  7  8  9 10 11 12 13 | 30 31  0  1  2  3  4  5 | 22 23 24 25 26 27 28 29
func (bs bs128) rotateLeft32_10(x, ret []byte) []byte {
	size := bs.bytes()

	copy(ret, x[(24-2)*size:24*size])
	copy(ret[2*size:], x[8*size:(16-2)*size])

	copy(ret[8*size:], x[(32-2)*size:32*size])
	copy(ret[(8+2)*size:], x[16*size:(24-2)*size])

	copy(ret[16*size:], x[(8-2)*size:8*size])
	copy(ret[(16+2)*size:], x[24*size:(32-2)*size])

	copy(ret[24*size:], x[(16-2)*size:16*size])
	copy(ret[(24+2)*size:], x[:(8-2)*size])

	return ret
}

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 |  0  1  2  3  4  5  6  7
//  6  7  8  9 10 11 12 13 | 30 31  0  1  2  3  4  5 | 22 23 24 25 26 27 28 19 | 14 15 16 17 18 19 20 21
func (bs bs128) rotateLeft32_18(x, ret []byte) []byte {
	size := bs.bytes()

	copy(ret, x[(32-2)*size:32*size])
	copy(ret[2*size:], x[16*size:(24-2)*size])

	copy(ret[8*size:], x[(8-2)*size:8*size])
	copy(ret[(8+2)*size:], x[24*size:(32-2)*size])

	copy(ret[16*size:], x[(16-2)*size:16*size])
	copy(ret[(16+2)*size:], x[:(8-2)*size])

	copy(ret[24*size:], x[(24-2)*size:24*size])
	copy(ret[(24+2)*size:], x[8*size:(16-2)*size])

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
	ret2 := bs.rotateLeft32_2(x, buffer[32*size:])
	bs.xor32(ret1, ret2)

	bs.rotateLeft32_8(ret2, x)
	bs.xor32(ret1, x)

	bs.rotateLeft32_8(x, ret2)
	return bs.xor32(ret1, ret2)

	/*
		ret1 := bs.rotateLeft32_2(x, buffer)
		ret2 := bs.rotateLeft32_10(x, buffer[32*size:])
		ret1 = bs.xor32(ret1, ret2)
		ret2 = bs.rotateLeft32_18(x, buffer[32*size:])
		ret1 = bs.xor32(ret1, ret2)
		ret2 = bs.rotateLeft32_24(x, buffer[32*size:])
		ret1 = bs.xor32(ret1, ret2)
		return bs.xor32(x, ret1)
	*/
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
