package sm4bs

func sboxInput(bytes []uint64) (g []uint64, m []uint64) {
	g = make([]uint64, 8)
	m = make([]uint64, 10)

	t1 := bytes[0] ^ bytes[2]
	t2 := ^(bytes[2] ^ bytes[6])
	t3 := ^(bytes[7] ^ t2)
	t4 := bytes[1] ^ bytes[5]
	t5 := bytes[4] ^ t3
	t6 := bytes[3] ^ t1
	t7 := bytes[6] ^ t5
	t8 := bytes[6] ^ t4
	t9 := t6 ^ t8
	t10 := t6 ^ t7
	t11 := ^(bytes[4] ^ t1)
	t12 := ^(bytes[1] ^ t9)
	t13 := t4 ^ t10
	t14 := t2 ^ t11
	t15 := t12 ^ t14
	t16 := t3 ^ t12
	t17 := t11 ^ t16

	g[0] = t10
	g[1] = t7
	g[2] = t13
	g[3] = t5
	g[4] = t2
	g[5] = ^bytes[7]
	g[6] = t14
	g[7] = t15

	m[0] = t6
	m[1] = t3
	m[2] = t8
	m[3] = t16
	m[4] = t4
	m[5] = t11
	m[6] = bytes[6]
	m[7] = t17
	m[8] = t9
	m[9] = t12
	return
}

func sboxTop(g, m []uint64) (p []uint64) {
	p = make([]uint64, 4)

	t1 := ^(g[5] & g[1])
	t2 := ^(m[1] & m[0])
	t3 := ^(g[4] & g[0])
	t4 := ^(g[7] & g[3])

	t5 := ^(m[9] & m[8])
	t6 := ^(g[6] | g[2])
	t7 := ^(g[7] | g[3])
	t8 := ^(m[9] | m[8])

	t9 := ^(m[7] | m[6])
	t10 := ^(m[3] & m[2])
	t11 := ^(m[5] & m[4])
	t12 := ^(m[3] | m[2])

	t13 := t1 ^ t2
	t14 := t3 ^ t2
	t15 := t4 ^ t13
	t16 := t5 ^ t14

	t17 := t9 ^ t10
	t18 := t11 ^ t12
	t19 := t6 ^ t15
	t20 := t7 ^ t16

	p[3] = t19 ^ t17
	p[2] = t20 ^ t18
	p[1] = t8 ^ t15
	p[0] = t6 ^ t16

	return
}

func sboxMiddle(p []uint64) (l []uint64) {
	l = make([]uint64, 4)

	t1 := ^(p[3] & p[0])
	t2 := ^(t1 | p[2])
	t3 := ^(p[2] & p[0])
	t4 := p[1] ^ t3
	t5 := ^(p[2] | t4)
	t6 := ^(p[1] & t4)
	t7 := ^(p[3] | t4)
	t8 := ^(t7 | t2)
	t9 := ^(t5 ^ t7)
	t10 := ^(t9 ^ p[3])
	t11 := ^(t6 & t8)
	t12 := ^(t8 & p[1])
	t13 := ^(p[0] ^ t12)
	t14 := ^(t1 & p[2])
	t15 := ^(t9 & t14)

	l[0] = t10
	l[1] = t15
	l[2] = t11
	l[3] = t13

	return
}

func sboxBottom(g, m, l []uint64) (e []uint64) {
	e = make([]uint64, 18)

	k4 := l[3] ^ l[2]
	k3 := l[3] ^ l[1]
	k2 := l[2] ^ l[0]
	k1 := k3 ^ k2
	k0 := l[1] ^ l[0]

	e[0] = ^(m[1] & k0)
	e[1] = ^(g[5] & l[1])
	e[2] = ^(g[4] & l[0])
	e[3] = ^(m[7] & k3)

	e[4] = ^(m[5] & k2)
	e[5] = ^(m[3] & k1)
	e[6] = ^(m[9] & k4)
	e[7] = ^(g[7] & l[3])

	e[8] = ^(g[6] & l[2])
	e[9] = ^(m[0] & k0)
	e[10] = ^(g[1] & l[1])
	e[11] = ^(g[0] & l[0])

	e[12] = ^(m[6] & k3)
	e[13] = ^(m[4] & k2)
	e[14] = ^(m[2] & k1)
	e[15] = ^(m[8] & k4)

	e[16] = ^(g[3] & l[3])
	e[17] = ^(g[2] & l[2])

	return
}

func sboxOutput(e []uint64) []uint64 {
	r0 := e[0] ^ e[1]
	r1 := e[2] ^ e[1]
	r2 := e[3] ^ e[4]
	r3 := e[5] ^ e[4]

	r4 := e[6] ^ e[7]
	r5 := e[8] ^ e[7]
	r6 := e[9] ^ e[10]
	r7 := e[11] ^ e[10]

	r8 := e[12] ^ e[13]
	r9 := e[14] ^ e[13]
	r10 := e[15] ^ e[16]
	r11 := e[17] ^ e[16]

	t1 := r9 ^ r7
	t2 := r1 ^ t1
	t3 := r3 ^ t2
	t4 := r5 ^ r3

	t5 := r4 ^ t4
	t6 := r4 ^ r0
	t7 := r11 ^ r7
	t8 := t1 ^ t4

	t9 := t1 ^ t6
	t10 := r2 ^ t5
	t11 := r10 ^ r8
	t12 := ^(t3 ^ t11)

	t13 := t10 ^ t12
	t14 := ^(t3 ^ t7)
	t15 := ^(r10 ^ r6)
	t16 := t6 ^ t14

	return []uint64{t15, t13, t8, t14, t11, t9, t12, t16}
}

func sboxBitsliced(bytes []uint64) []uint64 {
	g, m := sboxInput(bytes)
	return sboxOutput(sboxBottom(g, m, sboxMiddle(sboxTop(g, m))))
}

func bsTranspose(in []byte, out []uint64) {
	for i := 0; i < BlockSize; i++ {
		for j := 0; j < WordSize; j++ {
			k := (j*BlockSize + i) / 8 // byte order
			b := i % 8
			out[i] |= uint64((in[k]>>(7-b))&1) << j
		}
	}
}

func bsTransposeRev(state []uint64, dst []byte) {
	for i := 0; i < BlockSize; i++ {
		for j := 0; j < WordSize; j++ {
			k := (j*BlockSize + i) / 8 // byte order
			b := i % 8
			dst[k] |= byte((state[i]>>j)&1) << (7 - b)
		}
	}
}

func bsRoundKey(in uint32, out []uint64) {
	for i := 0; i < 32; i++ {
		if (in>>(31-i))&1 == 1 {
			out[i] = 0xffffffffffffffff
		} else {
			out[i] = 0
		}
	}
}

func xorRK(rk, x1, x2, x3 []uint64) []uint64 {
	for i := 0; i < 32; i++ {
		rk[i] ^= x1[i] ^ x2[i] ^ x3[i]
	}
	return rk
}

func xor(x1, x2 []uint64) []uint64 {
	for i := 0; i < 32; i++ {
		x1[i] ^= x2[i]
	}
	return x1
}

func tao(x []uint64) []uint64 {
	for i := 0; i < 4; i++ {
		ret := sboxBitsliced(x[i*8 : (i+1)*8])
		copy(x[i*8:(i+1)*8], ret)
	}
	return x
}

func rotateLeft32(x []uint64, k int) []uint64 {
	ret := make([]uint64, 32)
	copy(ret[:], x[k:])
	copy(ret[32-k:], x[:k])

	return ret
}

func L(x []uint64) []uint64 {
	ret1 := rotateLeft32(x, 2)
	ret2 := rotateLeft32(x, 10)
	ret1 = xor(ret1, ret2)
	ret2 = rotateLeft32(x, 18)
	ret1 = xor(ret1, ret2)
	ret2 = rotateLeft32(x, 24)
	ret1 = xor(ret1, ret2)
	return xor(x, ret1)
}
