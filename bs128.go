//go:build amd64 && gc && !purego

package sm4bs

import "encoding/binary"

var BS128 bs128

type bs128 struct{}

func (bs128) bytes() int {
	return 16
}

func (bs bs128) xor(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	for i := 0; i < bs.bytes(); i++ {
		ret[i] = x[i] ^ y[i]
	}
	return ret
}

func (bs bs128) and(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	for i := 0; i < bs.bytes(); i++ {
		ret[i] = x[i] & y[i]
	}
	return ret
}

func (bs bs128) or(x, y []byte) []byte {
	ret := make([]byte, bs.bytes())
	for i := 0; i < bs.bytes(); i++ {
		ret[i] = x[i] | y[i]
	}
	return ret
}

func (bs bs128) not(x []byte) []byte {
	for i := 0; i < bs.bytes(); i++ {
		x[i] = ^x[i]
	}
	return x
}

// input bytes = 8 x 16 bytes
// g = 8 x 16 bytes, m = 10 x 16 bytes
func (bs bs128) input(bytes []byte) (g []byte, m []byte) {
	size := bs.bytes()
	g = make([]byte, 8*size)
	m = make([]byte, 10*size)

	b0 := bytes[:size]
	b1 := bytes[size : 2*size]
	b2 := bytes[2*size : 3*size]
	b3 := bytes[3*size : 4*size]
	b4 := bytes[4*size : 5*size]
	b5 := bytes[5*size : 6*size]
	b6 := bytes[6*size : 7*size]
	b7 := bytes[7*size:]

	t1 := bs.xor(b7, b5)
	t2 := bs.not(bs.xor(b5, b1))
	t3 := bs.not(bs.xor(b0, t2))
	t4 := bs.xor(b6, b2)
	t5 := bs.xor(b3, t3)
	t6 := bs.xor(b4, t1)
	t7 := bs.xor(b1, t5)
	t8 := bs.xor(b1, t4)
	t9 := bs.xor(t6, t8)
	t10 := bs.xor(t6, t7)
	t11 := bs.not(bs.xor(b3, t1))
	t12 := bs.not(bs.xor(b6, t9))
	t13 := bs.xor(t4, t10)
	t14 := bs.xor(t2, t11)
	t15 := bs.xor(t12, t14)
	t16 := bs.xor(t3, t12)
	t17 := bs.xor(t11, t16)

	copy(g, t10)
	copy(g[size:], t7)
	copy(g[2*size:], t13)
	copy(g[3*size:], t5)
	copy(g[4*size:], t2)
	copy(g[5*size:], b0)
	bs.not(g[5*size:])
	copy(g[6*size:], t14)
	copy(g[7*size:], t15)

	copy(m, t6)
	copy(m[size:], t3)
	copy(m[2*size:], t8)
	copy(m[3*size:], t16)
	copy(m[4*size:], t4)
	copy(m[5*size:], t11)
	copy(m[6*size:], b1)
	copy(m[7*size:], t17)
	copy(m[8*size:], t9)
	copy(m[9*size:], t12)

	return
}

func (bs bs128) top(g, m []byte) (p []byte) {
	size := bs.bytes()
	p = make([]byte, 4*size)

	t1 := bs.not(bs.and(g[5*size:], g[size:]))
	t2 := bs.not(bs.and(m[1*size:], m))
	t3 := bs.not(bs.and(g[4*size:], g))
	t4 := bs.not(bs.and(g[7*size:], g[3*size:]))

	t5 := bs.not(bs.and(m[9*size:], m[8*size:]))
	t6 := bs.not(bs.or(g[6*size:], g[2*size:]))
	t7 := bs.not(bs.or(g[7*size:], g[3*size:]))
	t8 := bs.not(bs.or(m[9*size:], m[8*size:]))

	t9 := bs.not(bs.or(m[7*size:], m[6*size:]))
	t10 := bs.not(bs.and(m[3*size:], m[2*size:]))
	t11 := bs.not(bs.and(m[5*size:], m[4*size:]))
	t12 := bs.not(bs.or(m[3*size:], m[2*size:]))

	t13 := bs.xor(t1, t2)
	t14 := bs.xor(t3, t2)
	t15 := bs.xor(t4, t13)
	t16 := bs.xor(t5, t14)

	t17 := bs.xor(t9, t10)
	t18 := bs.xor(t11, t12)
	t19 := bs.xor(t6, t15)
	t20 := bs.xor(t7, t16)

	copy(p[3*size:], bs.xor(t19, t17))
	copy(p[2*size:], bs.xor(t20, t18))
	copy(p[1*size:], bs.xor(t8, t15))
	copy(p[0*size:], bs.xor(t6, t16))

	return
}

func (bs bs128) middle(p []byte) (l []byte) {
	size := bs.bytes()
	l = make([]byte, 4*size)

	t1 := bs.not(bs.and(p[3*size:], p))
	t2 := bs.not(bs.or(t1, p[2*size:]))
	t3 := bs.not(bs.and(p[2*size:], p))
	t4 := bs.xor(p[1*size:], t3)

	t5 := bs.not(bs.or(p[2*size:], t4))
	t6 := bs.not(bs.and(p[1*size:], t4))
	t7 := bs.not(bs.or(p[3*size:], t4))
	t8 := bs.not(bs.or(t7, t2))

	t9 := bs.not(bs.xor(t5, t7))
	t10 := bs.not(bs.xor(t9, p[3*size:]))
	t11 := bs.not(bs.and(t6, t8))
	t12 := bs.not(bs.and(t8, p[1*size:]))

	t13 := bs.not(bs.xor(t12, p[0*size:]))
	t14 := bs.not(bs.and(t1, p[2*size:]))
	t15 := bs.not(bs.and(t9, t14))

	copy(l, t10)
	copy(l[1*size:], t15)
	copy(l[2*size:], t11)
	copy(l[3*size:], t13)

	return
}

func (bs bs128) bottom(g, m, l []byte) (e []byte) {
	size := bs.bytes()
	e = make([]byte, 18*size)

	k4 := bs.xor(l[3*size:], l[2*size:])
	k3 := bs.xor(l[3*size:], l[1*size:])
	k2 := bs.xor(l[2*size:], l[0*size:])
	k1 := bs.xor(k3, k2)
	k0 := bs.xor(l[1*size:], l[0*size:])

	copy(e[0*size:], bs.not(bs.and(m[1*size:], k0)))
	copy(e[1*size:], bs.not(bs.and(g[5*size:], l[1*size:])))
	copy(e[2*size:], bs.not(bs.and(g[4*size:], l[0*size:])))
	copy(e[3*size:], bs.not(bs.and(m[7*size:], k3)))

	copy(e[4*size:], bs.not(bs.and(m[5*size:], k2)))
	copy(e[5*size:], bs.not(bs.and(m[3*size:], k1)))
	copy(e[6*size:], bs.not(bs.and(m[9*size:], k4)))
	copy(e[7*size:], bs.not(bs.and(g[7*size:], l[3*size:])))

	copy(e[8*size:], bs.not(bs.and(g[6*size:], l[2*size:])))
	copy(e[9*size:], bs.not(bs.and(m[0*size:], k0)))
	copy(e[10*size:], bs.not(bs.and(g[1*size:], l[1*size:])))
	copy(e[11*size:], bs.not(bs.and(g[0*size:], l[0*size:])))

	copy(e[12*size:], bs.not(bs.and(m[6*size:], k3)))
	copy(e[13*size:], bs.not(bs.and(m[4*size:], k2)))
	copy(e[14*size:], bs.not(bs.and(m[2*size:], k1)))
	copy(e[15*size:], bs.not(bs.and(m[8*size:], k4)))

	copy(e[16*size:], bs.not(bs.and(g[3*size:], l[3*size:])))
	copy(e[17*size:], bs.not(bs.and(g[2*size:], l[2*size:])))

	return
}

func (bs bs128) output(e []byte) (ret []byte) {
	size := bs.bytes()
	ret = make([]byte, 8*size)

	r0 := bs.xor(e[0*size:], e[1*size:])
	r1 := bs.xor(e[2*size:], e[1*size:])
	r2 := bs.xor(e[3*size:], e[4*size:])
	r3 := bs.xor(e[5*size:], e[4*size:])

	r4 := bs.xor(e[6*size:], e[7*size:])
	r5 := bs.xor(e[8*size:], e[7*size:])
	r6 := bs.xor(e[9*size:], e[10*size:])
	r7 := bs.xor(e[11*size:], e[10*size:])

	r8 := bs.xor(e[12*size:], e[13*size:])
	r9 := bs.xor(e[14*size:], e[13*size:])
	r10 := bs.xor(e[15*size:], e[16*size:])
	r11 := bs.xor(e[17*size:], e[16*size:])

	t1 := bs.xor(r9, r7)
	t2 := bs.xor(r1, t1)
	t3 := bs.xor(r3, t2)
	t4 := bs.xor(r5, r3)

	t5 := bs.xor(r4, t4)
	t6 := bs.xor(r4, r0)
	t7 := bs.xor(r11, r7)
	t8 := bs.xor(t1, t4)

	t9 := bs.xor(t1, t6)
	t10 := bs.xor(r2, t5)
	t11 := bs.xor(r10, r8)
	t12 := bs.not(bs.xor(t3, t11))

	t13 := bs.xor(t10, t12)
	t14 := bs.not(bs.xor(t3, t7))
	t15 := bs.not(bs.xor(r10, r6))
	t16 := bs.xor(t6, t14)

	copy(ret[7*size:], t15)
	copy(ret[6*size:], t13)
	copy(ret[5*size:], t8)
	copy(ret[4*size:], t14)
	copy(ret[3*size:], t11)
	copy(ret[2*size:], t9)
	copy(ret[1*size:], t12)
	copy(ret[0*size:], t16)

	return
}

func (bs bs128) sbox(bytes []byte) []byte {
	g, m := bs.input(bytes)
	return bs.output(bs.bottom(g, m, bs.middle(bs.top(g, m))))
}

func (bs bs128) tao(x []byte) []byte {
	size := 8 * bs.bytes()
	for i := 0; i < 4; i++ {
		ret := bs.sbox(x[i*size : (i+1)*size])
		copy(x[i*size:(i+1)*size], ret)
	}
	return x
}

func (bs bs128) xor32(x1, x2 []byte) []byte {
	size := bs.bytes()
	for i := 0; i < 32*size; i++ {
		x1[i] ^= x2[i]
	}
	return x1
}

func (bs bs128) xorRK(rk, x1, x2, x3 []byte) []byte {
	size := bs.bytes()
	for i := 0; i < 32*size; i++ {
		rk[i] ^= x1[i] ^ x2[i] ^ x3[i]
	}
	return rk
}

func (bs bs128) roundKey(in uint32, out []byte) {
	var bytes [4]byte
	size := bs.bytes()
	binary.BigEndian.PutUint32(bytes[:], in)
	ret := out
	for i := 0; i < 4; i++ {
		b := bytes[i]
		for j := 0; j < 8; j++ {
			for k := 0; k < size; k++ {
				ret[k] = -uint8(b & 1)
			}
			b = b >> 1
			ret = ret[size:]
		}
	}
}

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 |  0  1  2  3  4  5  6  7
// 22 23 24 25 26 27 28 29 | 14 15 16 17 18 19 20 21 |  6  7  8  9 10 11 12 13 | 30 31  0  1  2  3  4  5
func (bs bs128) rotateLeft32_2(x []byte) []byte {
	size := bs.bytes()
	ret := make([]byte, 32*size)
	copy(ret, x[(16-2)*size:16*size])
	copy(ret[2*size:], x[:(8-2)*size])

	copy(ret[8*size:], x[(24-2)*size:24*size])
	copy(ret[(8+2)*size:], x[8*size:(16-2)*size])

	copy(ret[16*size:], x[(32-2)*size:])
	copy(ret[(16+2)*size:], x[16*size:(24-2)*size])

	copy(ret[24*size:], x[(8-2)*size:8*size])
	copy(ret[(24+2)*size:], x[24*size:(32-2)*size])

	return ret
}

// 24 25 26 27 28 29 30 31 | 16 17 18 19 20 21 22 23 |  8  9 10 11 12 13 14 15 |  0  1  2  3  4  5  6  7
// 14 15 16 17 18 19 20 21 |  6  7  8  9 10 11 12 13 | 30 31  0  1  2  3  4  5 | 22 23 24 25 26 27 28 29
func (bs bs128) rotateLeft32_10(x []byte) []byte {
	size := bs.bytes()
	ret := make([]byte, 32*size)

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
func (bs bs128) rotateLeft32_18(x []byte) []byte {
	size := bs.bytes()
	ret := make([]byte, 32*size)

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
func (bs bs128) rotateLeft32_24(x []byte) []byte {
	size := bs.bytes()
	ret := make([]byte, 32*size)

	copy(ret, x[24*size:])
	copy(ret[8*size:], x[:24*size])

	return ret
}

func (bs bs128) l(x []byte) []byte {
	ret1 := bs.rotateLeft32_2(x)
	ret2 := bs.rotateLeft32_10(x)
	ret1 = bs.xor32(ret1, ret2)
	ret2 = bs.rotateLeft32_18(x)
	ret1 = bs.xor32(ret1, ret2)
	ret2 = bs.rotateLeft32_24(x)
	ret1 = bs.xor32(ret1, ret2)
	return bs.xor32(x, ret1)
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

	for i := 0; i < 8; i++ {
		bs.roundKey(xk[i*4], rk)
		b0 = bs.xor32(b0, bs.l(bs.tao(bs.xorRK(rk, b1, b2, b3))))
		bs.roundKey(xk[i*4+1], rk)
		b1 = bs.xor32(b1, bs.l(bs.tao(bs.xorRK(rk, b2, b3, b0))))
		bs.roundKey(xk[i*4+2], rk)
		b2 = bs.xor32(b2, bs.l(bs.tao(bs.xorRK(rk, b3, b0, b1))))
		bs.roundKey(xk[i*4+3], rk)
		b3 = bs.xor32(b3, bs.l(bs.tao(bs.xorRK(rk, b0, b1, b2))))
	}
	copy(rk, b0)
	copy(state[:], b3)
	copy(state[96*bitSize:], rk)
	copy(rk, b1)
	copy(state[32*bitSize:], b2)
	copy(state[64*bitSize:], rk)
	transpose128(&state[0], &dst[0])
}
