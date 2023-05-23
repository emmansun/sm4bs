package main

import (
	. "github.com/mmcloughlin/avo/build"
	. "github.com/mmcloughlin/avo/operand"
	. "github.com/mmcloughlin/avo/reg"
)

//go:generate go run . -out ../transpose128_amd64.s -stubs ../transpose128_amd64.go -pkg sm4bs

func transpose64() {
	// transpose64 function
	TEXT("transpose64", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 64x128")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	tmp := XMM()
	b := GP8()
	o := GP32()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop_64")
	Comment("Initialize cc, current col")
	cc := zero()
	Label("col_loop_64")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(7), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct one XMM with first byte of first 16 rows")
	for i := 0; i < 16; i++ {
		MOVB(in.Idx(addr, 1), b)
		PINSRB(Imm(uint64(i)), b.As32(), tmp)
		Comment("Add ncols / 8")
		ADDQ(Imm(16), addr)
	}

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 64")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(6), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the XMM, and store the returned 2 bytes")
	for i := 7; i >= 0; i-- {
		PMOVMSKB(tmp, o)
		MOVW(o.As16(), out.Idx(addr, 1))
		PSLLQ(Imm(1), tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(8), addr)
	}

	Comment("Compare cc with ncols, here ncols=128")
	ADDQ(Imm(8), cc)
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop_64"))

	Comment("Compare rr with nrows, here nrows=64")
	ADDQ(Imm(16), rr)
	CMPQ(rr, U8(64))
	JL(LabelRef("row_loop_64"))

	RET()
}

func transpose64Rev() {
	// transpose64Rev function
	TEXT("transpose64Rev", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 128x64")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	tmp := XMM()
	b := GP8()
	o := GP32()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop_rev64")
	Comment("Initialize cc, current col")
	cc := zero()
	Label("col_loop_rev64")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=64")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(6), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct one XMM with first byte of first 16 rows")
	for i := 0; i < 16; i++ {
		MOVB(in.Idx(addr, 1), b)
		PINSRB(Imm(uint64(i)), b.As32(), tmp)
		Comment("Add ncols / 8")
		ADDQ(Imm(8), addr)
	}

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the XMM, and store the returned 2 bytes")
	for i := 7; i >= 0; i-- {
		PMOVMSKB(tmp, o)
		MOVW(o.As16(), out.Idx(addr, 1))
		PSLLQ(Imm(1), tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}

	Comment("Compare cc with ncols, here ncols=64")
	ADDQ(Imm(8), cc)
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_rev64"))

	Comment("Compare rr with nrows, here nrows=128")
	ADDQ(Imm(16), rr)
	CMPQ(rr, U8(128))
	JL(LabelRef("row_loop_rev64"))

	RET()
}

func transpose128() {
	// transpose128 function
	TEXT("transpose128", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 128x128")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	tmp := XMM()
	b := GP8()
	o := GP32()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop")
	Comment("Initialize cc, current col")
	cc := zero()
	Label("col_loop")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(7), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct one XMM with first byte of first 16 rows")
	for i := 0; i < 16; i++ {
		MOVB(in.Idx(addr, 1), b)
		PINSRB(Imm(uint64(i)), b.As32(), tmp)
		Comment("Add ncols / 8")
		ADDQ(Imm(16), addr)
	}

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the XMM, and store the returned 2 bytes")
	for i := 7; i >= 0; i-- {
		PMOVMSKB(tmp, o)
		MOVW(o.As16(), out.Idx(addr, 1))
		PSLLQ(Imm(1), tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}

	Comment("Compare cc with ncols, here ncols=128")
	ADDQ(Imm(8), cc)
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop"))

	Comment("Compare rr with nrows, here nrows=128")
	ADDQ(Imm(16), rr)
	CMPQ(rr, U8(128))
	JL(LabelRef("row_loop"))

	RET()
}

func xor32x128() {
	// xor32x128 function
	TEXT("xor32x128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = x xor y")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	count := zero()
	Label("xor32_loop")
	MOVOU(x.Idx(count, 1), X)
	MOVOU(y.Idx(count, 1), Y)
	PXOR(X, Y)
	MOVOU(Y, out.Idx(count, 1))
	ADDQ(U8(16), count)
	CMPQ(count, U32(512))
	JL(LabelRef("xor32_loop"))

	RET()
}

func xorRoundKey128() {
	// xorRoundKey128 function
	TEXT("xorRoundKey128", NOSPLIT, "func(rk uint32, x1, x2, x3, out *byte)")
	Doc("xor x1, x2, x3 with round key, 16 bytes per bit")

	x := Load(Param("rk"), GP32())
	x1 := Mem{Base: Load(Param("x1"), GP64())}
	x2 := Mem{Base: Load(Param("x2"), GP64())}
	x3 := Mem{Base: Load(Param("x3"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	ret := XMM()
	one := XMM()
	PCMPEQB(one, one)

	y := GP32()

	count := GP64()
	XORQ(count, count)
	Comment("Handle first byte")
	MOVL(U32(0x01000000), y)
	Label("rk_loop_1")
	MOVOU(x1.Idx(count, 1), ret)
	PXOR(x2.Idx(count, 1), ret)
	PXOR(x3.Idx(count, 1), ret)
	TESTL(x, y)
	JZ(LabelRef("rk_loop_1_c"))
	PXOR(one, ret)
	Label("rk_loop_1_c")
	MOVOU(ret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(128))
	JL(LabelRef("rk_loop_1"))

	Comment("Handle second byte")
	MOVL(U32(0x00010000), y)
	Label("rk_loop_2")
	MOVOU(x1.Idx(count, 1), ret)
	PXOR(x2.Idx(count, 1), ret)
	PXOR(x3.Idx(count, 1), ret)
	TESTL(x, y)
	JZ(LabelRef("rk_loop_2_c"))
	PXOR(one, ret)
	Label("rk_loop_2_c")
	MOVOU(ret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(256))
	JL(LabelRef("rk_loop_2"))

	Comment("Handle third byte")
	MOVL(U32(0x00000100), y)
	Label("rk_loop_3")
	MOVOU(x1.Idx(count, 1), ret)
	PXOR(x2.Idx(count, 1), ret)
	PXOR(x3.Idx(count, 1), ret)
	TESTL(x, y)
	JZ(LabelRef("rk_loop_3_c"))
	PXOR(one, ret)
	Label("rk_loop_3_c")
	MOVOU(ret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(384))
	JL(LabelRef("rk_loop_3"))

	Comment("Handle last byte")
	MOVL(U32(0x00000001), y)
	Label("rk_loop_4")
	MOVOU(x1.Idx(count, 1), ret)
	PXOR(x2.Idx(count, 1), ret)
	PXOR(x3.Idx(count, 1), ret)
	TESTL(x, y)
	JZ(LabelRef("rk_loop_4_c"))
	PXOR(one, ret)
	Label("rk_loop_4_c")
	MOVOU(ret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(512))
	JL(LabelRef("rk_loop_4"))

	RET()
}

func sbox128() {
	// sbox128 function
	TEXT("sbox128", NOSPLIT, "func(x, buffer *byte)")
	Doc("sbox128, 128 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	Comment("f, for not operation")
	f := XMM()
	PCMPEQB(f, f)

	Comment("Start input function")
	Comment("t1=b7 ^ b5")
	t1 := XMM()
	MOVOU(b.Offset(7*16), t1)
	PXOR(b.Offset(5*16), t1)

	t2, t7, t8 := XMM(), XMM(), XMM()
	MOVOU(b.Offset(1*16), t2)
	MOVOU(t2, t7)
	MOVOU(t2, t8)
	Comment("store m6=b1")
	MOVOU(t2, buffer.Offset((8+6)*16)) // m6
	Comment("t2=b5 ^ b1")
	PXOR(b.Offset(5*16), t2)
	PANDN(f, t2)

	t3, t4 := XMM(), XMM()
	Comment("store g5=^b0")
	MOVOU(b, t3)
	MOVOU(t3, t4)
	PANDN(f, t4)
	MOVOU(t4, buffer.Offset(5*16)) // g5
	Comment("t3=^(b0 ^ t2)")
	PXOR(t2, t3)
	PANDN(f, t3)

	Comment("t4=b6 ^ b2")
	t12 := XMM()
	MOVOU(b.Offset(6*16), t4)
	MOVOU(t4, t12)
	PXOR(b.Offset(2*16), t4)

	Comment("t5=b3 ^ t3")
	t5, t11 := XMM(), XMM()
	MOVOU(b.Offset(3*16), t5)
	MOVOU(t5, t11)
	PXOR(t3, t5)

	Comment("t6=b4 ^ t1")
	t6 := XMM()
	MOVOU(b.Offset(4*16), t6)
	PXOR(t1, t6)

	Comment("t7=b1 ^ t5")
	PXOR(t5, t7)
	Comment("t8=b1 ^ t4")
	PXOR(t4, t8)

	Comment("t9=t6 ^ t8")
	t9 := XMM()
	MOVOU(t6, t9)
	PXOR(t8, t9)
	Comment("store m8")
	MOVOU(t9, buffer.Offset((8+8)*16)) // m8
	Comment("store g1")
	MOVOU(t7, buffer.Offset(1*16)) // g1
	Comment("store g3")
	MOVOU(t5, buffer.Offset(3*16)) // g3
	Comment("store g4")
	MOVOU(t2, buffer.Offset(4*16)) // g4
	Comment("store m0")
	MOVOU(t6, buffer.Offset((8+0)*16)) // m0
	Comment("store m1")
	MOVOU(t3, buffer.Offset((8+1)*16)) // m1
	Comment("store m2")
	MOVOU(t8, buffer.Offset((8+2)*16)) // m2
	Comment("store m4")
	MOVOU(t4, buffer.Offset((8+4)*16)) // m4

	Comment("t11=^(b3 ^ t1)")
	PXOR(t1, t11)
	PANDN(f, t11)
	Comment("store m5, can reuse t1 now")
	MOVOU(t11, buffer.Offset((8+5)*16)) // m5

	Comment("t12=^(b6 ^ t9)")
	PXOR(t9, t12)
	PANDN(f, t12)
	Comment("store m9, can reuse t7 t8 t9 now")
	MOVOU(t12, buffer.Offset((8+9)*16)) // m9

	Comment("t10=t6 ^ t7")
	t10 := t7
	PXOR(t6, t10)
	Comment("store g0, can reuse t6 now")
	MOVOU(t10, buffer) // g0

	Comment("t13=t4 ^ t10")
	t13 := t10
	PXOR(t4, t13)
	Comment("store g2, can reuse t4 now")
	MOVOU(t13, buffer.Offset(2*16)) // g2

	Comment("t14=t2 ^ t11")
	t14 := t1
	MOVOU(t11, t14)
	PXOR(t2, t14)
	Comment("store g6, can reuse t2 now")
	MOVOU(t14, buffer.Offset(6*16)) // g6

	Comment("t15=t12^t14")
	t15 := t14
	PXOR(t12, t15)
	Comment("store g7")
	MOVOU(t15, buffer.Offset(7*16)) // g7

	Comment("t16=t3 ^ t12")
	t16 := t12
	PXOR(t3, t16)
	Comment("store m3")
	MOVOU(t16, buffer.Offset((8+3)*16)) // m3

	Comment("t17=t11 ^ t16")
	t17 := t16
	PXOR(t11, t17)
	Comment("store m7")
	MOVOU(t17, buffer.Offset((8+7)*16)) // m7

	Comment("Start top function")
	Comment("Current register status: t17=t16=t12=m7, t11=m5, t15=t14=t1=g7, t13=t10=t7=g2, t4=m4, t8=m2, t3=m1, t6=m0, t2=g4, t5=g3,t9=m8")
	// t1 = g7
	// t2 = g4
	// t3 = m1
	// t4 = m4
	// t5 = g3
	// t6 = m0
	// t7 = g2
	// t8 = m2
	// t9 = m8
	// t11 = m5
	// t12 = m7
	Comment("t2=^(m0 & m1)")
	PAND(t6, t3)
	PANDN(f, t3) // t2

	Comment("t3=^(g0 & g4)")
	PAND(buffer, t2)
	PANDN(f, t2) // t3

	Comment("t4=^(g3 & g7)")
	MOVOU(t1, t6)
	PAND(t5, t1)
	PANDN(f, t1) // t4

	Comment("t7=^(g3 | g7)")
	POR(t6, t5)
	PANDN(f, t5) // t7

	Comment("t11=^(m4 & m5)")
	PAND(t4, t11)
	PANDN(f, t11) // t11

	MOVOU(buffer.Offset((8+3)*16), t4) // t4 = m3
	MOVOU(t4, t6)
	Comment("t10=^( m3 & m2 )")
	PAND(t8, t6)
	PANDN(f, t6) // t10
	Comment("t12=^( m3 | m2 )")
	POR(t8, t4)
	PANDN(f, t4) // t12

	Comment("t6=^( g6 | g2 )")
	POR(buffer.Offset(6*16), t7)
	PANDN(f, t7) // t6

	Comment("t9=^( m6 | m7 )")
	POR(buffer.Offset((8+6)*16), t12)
	PANDN(f, t12) // t9

	t10 = XMM()
	MOVOU(buffer.Offset((8+9)*16), t8) // t8 = m9
	MOVOU(t8, t10)

	Comment("t5=^( m8 & m9 )")
	PAND(t9, t8)
	PANDN(f, t8) // t5
	Comment("t8=^( m8 | m9 )")
	POR(t9, t10)
	PANDN(f, t10) // t8

	Comment("t14 = t3 ^ t2")
	PXOR(t3, t2) // t14 = t3 ^ t2
	Comment("t16 = t5 ^ t14")
	PXOR(t2, t8) // t16 = t5 ^ t14, can reuse t2 now
	Comment("t20 = t16 ^ t7")
	PXOR(t8, t5) // t20 = t16 ^ t7
	Comment("t17 = t9 ^ t10")
	PXOR(t12, t6) // t17 = t9 ^ t10
	Comment("t18 = t11 ^ t12")
	PXOR(t11, t4) // t18 = t11 ^ t12
	Comment("p2 = t20 ^ t18")
	PXOR(t5, t4) // p2 = t20 ^ t18, can reuse t5 now
	Comment("p0 = t6 ^ t16")
	PXOR(t7, t8) // p0 = t6 ^ t16
	Comment("t1 = ^(g5 & g1)")
	MOVOU(buffer.Offset(1*16), t2)
	MOVOU(buffer.Offset(5*16), t5)
	PAND(t2, t5)
	PANDN(f, t5) // t1
	Comment("t13 = t1 ^ t2")
	PXOR(t5, t3) // t13 = t1 ^ t2
	Comment("t15 = t13 ^ t4")
	PXOR(t1, t3) // t15 = t4 ^ t13
	Comment("t19 = t6 ^ t15")
	PXOR(t3, t7) // t19 = t6 ^ t15
	Comment("p3 = t19 ^ t17")
	PXOR(t6, t7) // p3 = t19 ^ t17
	Comment("p1 = t8 ^ t15")
	PXOR(t10, t3) // p1 = t8 ^ t15

	Comment("start middle function")
	Comment("current register status: t8=p0, t3=p1, t4=p2, t7=p0")

	// t3 = p1
	// t4 = p2
	// t7 = p3
	// t8 = p0
	Comment("t1 = ^(p3 & p0)")
	MOVOU(t8, t1)
	PAND(t7, t1)
	PANDN(f, t1) // t1 = ^(p3 & p0)

	Comment("t2 = ^(t1 | p2)")
	MOVOU(t4, t2)
	POR(t1, t2)
	PANDN(f, t2) // t2 = ^(t1 | p2)

	Comment("t3 = ^(p2 & p0)")
	MOVOU(t4, t5) // p2
	PAND(t8, t4)
	PANDN(f, t4) // t4 = ^(p2 & p0)

	Comment("t4 = p1 ^ t3")
	PXOR(t3, t4) // t4 = p1 ^ t4

	Comment("t5 = ^(p2 | t4)")
	MOVOU(t5, t9) // p2
	POR(t4, t5)
	PANDN(f, t5) // t5 = ^(p2 | t4)

	Comment("t6 = ^(p1 & t4)")
	MOVOU(t3, t6) // p1
	PAND(t4, t6)
	PANDN(f, t6) // t6 = ^(p1 & t4)

	Comment("t7 = ^(p3 | t4)")
	MOVOU(t7, t11) // p3
	POR(t4, t7)
	PANDN(f, t7) // t7 = ^(p3 | t4)

	Comment("t8 = ^(t7 | t2)")
	MOVOU(t8, t12) // p0
	MOVOU(t7, t8)
	POR(t2, t8)
	PANDN(f, t8) // t8 = ^(t7 | t2)

	Comment("t9 = ^(t7 ^ t5)")
	PXOR(t5, t7)
	PANDN(f, t7) // t7 = ^(t5 ^ t7)

	Comment("t10 = ^(t9 ^ p3)")
	PXOR(t7, t11)
	PANDN(f, t11) // l0 = t11 = ^(t7 & p3)

	Comment("t11 = ^(t6 & t8)")
	PAND(t8, t6)
	PANDN(f, t6) // l2 = t6 = ^(t6 & t8)

	Comment("t12 = ^(p1 & t8)")
	PAND(t8, t3)
	PANDN(f, t3) // t3 = ^(t8 & p1)

	Comment("t13 = ^(t12 ^ p0)")
	PXOR(t3, t12)
	PANDN(f, t12) // l3 = t12 = ^(p0 ^ t3)

	Comment("t14 = ^(t1 & p2)")
	PAND(t1, t9)
	PANDN(f, t9) // t14

	Comment("t15 = ^(t14 & t9)")
	PAND(t9, t7)
	PANDN(f, t7) // l1

	Comment("start bottom function")
	Comment("current register status: t11=l0, t7=l1, t6=l2, t12=l3")
	Comment("k4 = l2 ^ l3")
	MOVOU(t12, t5)
	PXOR(t6, t5) // k4 = l2 ^ l3
	Comment("k3 = l1 ^ l3")
	MOVOU(t12, t4)
	PXOR(t7, t4) // k3 = l1 ^ l3
	Comment("k2 = l0 ^ l2")
	MOVOU(t6, t3)
	PXOR(t11, t3) // k2 = l0 ^ l2
	Comment("k0 = l0 ^ l1")
	MOVOU(t7, t1)
	PXOR(t11, t1) // k0 = l0 ^ l1
	Comment("k1 = k2 ^ k3")
	MOVOU(t4, t2)
	PXOR(t3, t2) // k1 = k2 ^ k3

	Comment("e0=^(m1 & k0)")
	MOVOU(buffer.Offset((8+1)*16), t8) // m1
	PAND(t1, t8)
	PANDN(f, t8) // e0

	Comment("e1=^(g5 & l1)")
	MOVOU(buffer.Offset(5*16), t9)
	PAND(t7, t9)
	PANDN(f, t9) // e1

	Comment("r0=e0 ^ e1")
	PXOR(t9, t8) // r0 = e0 ^ e1

	Comment("e2=^(g4 & l0)")
	MOVOU(buffer.Offset(4*16), t10)
	PAND(t11, t10)
	PANDN(f, t10) // e2
	Comment("r1=e2 ^ e1")
	PXOR(t10, t9) // r1 = e2 ^ e1

	Comment("store r0 r1")
	MOVOU(t8, buffer.Offset(22*16)) // in fact, we can start from 18*16
	MOVOU(t9, buffer.Offset(23*16))

	Comment("e3=^(m7 & k3)")
	MOVOU(buffer.Offset((8+7)*16), t8) // m7
	PAND(t4, t8)
	PANDN(f, t8) // e3

	Comment("e4=^(m5 & k2)")
	MOVOU(buffer.Offset((8+5)*16), t9) // m5
	PAND(t3, t9)
	PANDN(f, t9) // e4
	Comment("r2=e3 ^ e4")
	PXOR(t9, t8) // r2 = e3 ^ e4

	Comment("e5=^(m3 & k1)")
	MOVOU(buffer.Offset((8+3)*16), t10) // m3
	PAND(t2, t10)
	PANDN(f, t10) // e5
	Comment("r3=e5 ^ e4")
	PXOR(t10, t9) // r3 = e5 ^ e4

	Comment("store r2 r3")
	MOVOU(t8, buffer.Offset(24*16))
	MOVOU(t9, buffer.Offset(25*16))

	Comment("e6=^(m9 & k4)")
	MOVOU(buffer.Offset((8+9)*16), t8) // m9
	PAND(t5, t8)
	PANDN(f, t8) // e6

	Comment("e7=^(g7 & l3)")
	MOVOU(buffer.Offset(7*16), t9)
	PAND(t12, t9)
	PANDN(f, t9) // e7
	Comment("r4=e7 ^ e6")
	PXOR(t9, t8) // r4 = e6 ^ e7

	Comment("e8=^(g6 & l2)")
	MOVOU(buffer.Offset(6*16), t10)
	PAND(t6, t10)
	PANDN(f, t10) // e8
	Comment("r5=e8 ^ e6")
	PXOR(t10, t9) // r5 = e8 ^ e7

	Comment("store r4 r5")
	MOVOU(t8, buffer.Offset(26*16))
	MOVOU(t9, buffer.Offset(27*16))

	Comment("e9=^(m0 & k0)")
	MOVOU(buffer.Offset((8+0)*16), t8) // m0
	PAND(t1, t8)
	PANDN(f, t8) // e9

	Comment("e10=^(g1 & l1)")
	MOVOU(buffer.Offset(1*16), t9)
	PAND(t7, t9)
	PANDN(f, t9) // e10

	Comment("r6=e9 ^ e10")
	PXOR(t9, t8) // r6 = e9 ^ e10

	Comment("e11=^(g0 & l0)")
	MOVOU(buffer, t10)
	PAND(t11, t10)
	PANDN(f, t10) // e11
	Comment("r7=e11 ^ e10")
	PXOR(t10, t9) // r7 = e11 ^ e10
	Comment("store r6 r7")
	MOVOU(t8, buffer.Offset(28*16))
	MOVOU(t9, buffer.Offset(29*16))

	Comment("e12=^(m6 & k3)")
	MOVOU(buffer.Offset((8+6)*16), t7) // m6
	PAND(t4, t7)
	PANDN(f, t7) // e12

	Comment("e13=^(m4 & k2)")
	MOVOU(buffer.Offset((8+4)*16), t11) // m4
	PAND(t3, t11)
	PANDN(f, t11) // e13

	Comment("r8=e12 ^ e13")
	PXOR(t11, t7) // r8 = e12 ^ e13 = t7

	Comment("e14=^(m2 & k1)")
	MOVOU(buffer.Offset((8+2)*16), t10) // m2
	PAND(t2, t10)
	PANDN(f, t10) // e14

	Comment("r9=e14 ^ e13")
	PXOR(t10, t11) // r9 = e14 ^ e13  = t11

	Comment("e15=^(m8 & k4)")
	MOVOU(buffer.Offset((8+8)*16), t8) // m8
	PAND(t5, t8)
	PANDN(f, t8) // e15

	Comment("e16=^(g3 & l3)")
	MOVOU(buffer.Offset(3*16), t9)
	PAND(t12, t9)
	PANDN(f, t9) // e16
	Comment("r10=e15 ^ e16")
	PXOR(t9, t8) // r10 = e15 ^ e16 = t8

	Comment("e17=^(g2 & l2)")
	MOVOU(buffer.Offset(2*16), t10)
	PAND(t6, t10)
	PANDN(f, t10) // e17

	Comment("r11=e17 ^ e16")
	PXOR(t10, t9) // r11 = e17 ^ e16 = t9

	Comment("start output function")
	// t7 = r8
	// t11 = r9
	// t8 = r10
	// t9 = r11
	Comment("[t1]=r7 ^ r9")
	MOVOU(buffer.Offset((22+7)*16), t1) // r7
	PXOR(t1, t11)                       // t11 = r7 ^ r9
	Comment("t2=t1 ^ r1")
	MOVOU(buffer.Offset((22+1)*16), t2) // r1
	PXOR(t11, t2)                       // t2 = r1 ^ t11
	Comment("t3=t2 ^ r3")
	MOVOU(buffer.Offset((22+3)*16), t3) // r3
	MOVOU(t3, t4)
	PXOR(t2, t3) // t3 = r3 ^ t2
	Comment("t4=r5 ^ r3")
	PXOR(buffer.Offset((22+5)*16), t4) // t4 = r5 ^ r3

	MOVOU(buffer.Offset((22+4)*16), t5) // r4
	MOVOU(t5, t6)
	Comment("t5=r4 ^ t4")
	PXOR(t4, t5) // t5 = r4 ^ t4
	Comment("t6=r0 ^ t4")
	PXOR(buffer.Offset(22*16), t6) // t6 = r4 ^ r0

	Comment("[t7]=r11 ^ r7")
	PXOR(t9, t1) // [t7] t1 = r7 ^ r11
	Comment("[t8]=[t1] ^ t4")
	PXOR(t11, t4) // [t8] t4 = t4 ^ t11
	Comment("store t8")
	MOVOU(t4, b.Offset(5*16))
	Comment("[t9]=[t1] ^ t6")
	PXOR(t6, t11) // [t9] t11 = t11 ^ t6
	Comment("store t9")
	MOVOU(t11, b.Offset(2*16))

	Comment("[t10]=r2 ^ t5")
	PXOR(buffer.Offset((22+2)*16), t5) // [t10] t5 = r2 ^ t5
	Comment("[t11]=r10 ^ r8")
	PXOR(t8, t7) // [t11] t7 = rr8 ^ r10
	Comment("store t11")
	MOVOU(t7, b.Offset(3*16))
	Comment("[t12]=^(t3 ^ [t11])")
	PXOR(t3, t7) // t7 = t3 ^ [t11]
	PANDN(f, t7) // [t12] t7 = ^(t3 ^ [t11])
	Comment("store t12")
	MOVOU(t7, b.Offset(1*16))
	Comment("[t13]=[t10] ^ [t12]")
	PXOR(t7, t5) // [t13] t5 = [t10] ^ [t12]
	Comment("store t13")
	MOVOU(t5, b.Offset(6*16))

	Comment("[t14]=^(t3 ^ [t7])")
	PXOR(t3, t1)
	PANDN(f, t1) // [t14]
	Comment("store t14")
	MOVOU(t1, b.Offset(4*16))
	Comment("[t16]=t6 ^ [t14]")
	PXOR(t6, t1) // [t16]
	Comment("store t16")
	MOVOU(t1, b)

	Comment("[t15]=^(r10 ^ r6)")
	PXOR(buffer.Offset((22+6)*16), t8)
	PANDN(f, t8)
	Comment("store t15")
	MOVOU(t8, b.Offset(7*16))

	RET()
}

// 0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23 | 24 25 26 27 28 29 30 31
//24 25 26 27 28 29 30 31 |  0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23
//14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29
//22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5
//30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13
func l128() {
	// l128 function
	TEXT("l128", NOSPLIT, "func(x, buffer *byte)")
	Doc("l128, 128 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	X0, X1, X2, X3, X4, X5, X6, X7, X8, X9 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()

	MOVOU(b, X0)
	MOVOU(b.Offset(8*16), X1)
	MOVOU(b.Offset(16*16), X2)
	MOVOU(b.Offset(24*16), X3)
	MOVOU(b.Offset(18*16), X5)
	MOVOU(b.Offset(22*16), X6)
	MOVOU(b.Offset(26*16), X7)
	MOVOU(b.Offset(30*16), X8)
	MOVOU(b.Offset(2*16), X9)

	Comment("0=0^24^14^22^30")
	MOVOU(X0, X4)
	PXOR(X3, X4)
	PXOR(b.Offset(14*16), X4)
	PXOR(X6, X4)
	PXOR(X8, X4)
	MOVOU(X4, buffer)

	Comment("2=0^2^26^8^16")
	MOVOU(X0, X4)
	PXOR(X9, X4)
	PXOR(X7, X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	MOVOU(X4, buffer.Offset(2*16))

	Comment("8=0^8^22^30^6")
	MOVOU(X0, X4)
	PXOR(X1, X4)
	PXOR(X6, X4)
	PXOR(X8, X4)
	PXOR(b.Offset(6*16), X4)
	MOVOU(X4, buffer.Offset(8*16))

	Comment("18=0^18^10^16^24")
	MOVOU(X0, X4)
	PXOR(X5, X4)
	PXOR(b.Offset(10*16), X4)
	PXOR(X2, X4)
	PXOR(X3, X4)
	MOVOU(X4, buffer.Offset(18*16))

	Comment("26=0^26^18^24^8")
	PXOR(X1, X0)
	PXOR(X7, X0)
	PXOR(X5, X0)
	PXOR(X3, X0)
	MOVOU(X0, buffer.Offset(26*16))

	Comment("10=10^2^8^16^24")
	MOVOU(X9, X4)
	PXOR(b.Offset(10*16), X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	PXOR(X3, X4)
	MOVOU(X4, buffer.Offset(10*16))

	MOVOU(b.Offset(6*16), X0)
	MOVOU(b.Offset(14*16), X5)
	Comment("16=16^8^30^6^14")
	PXOR(X2, X1)
	PXOR(X8, X1)
	PXOR(X0, X1)
	PXOR(X5, X1)
	MOVOU(X1, buffer.Offset(16*16))

	Comment("24=24^16^6^14^22")
	PXOR(X3, X2)
	PXOR(X0, X2)
	PXOR(X5, X2)
	PXOR(X6, X2)
	MOVOU(X2, buffer.Offset(24*16))

	MOVOU(b.Offset(4*16), X1)
	MOVOU(b.Offset(10*16), X2)
	MOVOU(b.Offset(12*16), X3)
	// X0=6, X1=4, X9=X4=2, X2=10, X3=12, X5=14, X6=22, X7=26, X8=30
	Comment("4=4^28^2^10^18")
	MOVOU(X9, X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	PXOR(b.Offset(18*16), X4)
	PXOR(b.Offset(28*16), X4)
	MOVOU(X4, buffer.Offset(4*16))

	Comment("20=20^12^18^26^2")
	MOVOU(X9, X4)
	PXOR(b.Offset(20*16), X4)
	PXOR(X3, X4)
	PXOR(b.Offset(18*16), X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(20*16))

	Comment("28=28^20^26^2^10")
	PXOR(b.Offset(28*16), X9)
	PXOR(b.Offset(20*16), X9)
	PXOR(X7, X9)
	PXOR(X2, X9)
	MOVOU(X9, buffer.Offset(28*16))

	MOVOU(b.Offset(20*16), X9)
	// X0=6, X1=4, X9=20, X2=10, X3=12, X5=14, X6=22, X7=26, X8=30

	Comment("6=6^30^4^12^20")
	MOVOU(X1, X4)
	PXOR(X0, X4)
	PXOR(X3, X4)
	PXOR(X8, X4)
	PXOR(X9, X4)
	MOVOU(X4, buffer.Offset(6*16))

	Comment("12=12^4^10^18^26")
	MOVOU(X1, X4)
	PXOR(X3, X4)
	PXOR(X2, X4)
	PXOR(b.Offset(18*16), X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(12*16))

	MOVOU(b.Offset(28*16), X7)
	// X0=6, X1=4, X9=20, X2=10, X3=12, X5=14, X6=22, X7=28, X8=30
	Comment("22=22^14^20^28^4")
	MOVOU(X1, X4)
	PXOR(X5, X4)
	PXOR(X6, X4)
	PXOR(X9, X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(22*16))

	Comment("30=30^22^28^4^12")
	PXOR(X8, X1)
	PXOR(X6, X1)
	PXOR(X3, X1)
	PXOR(X7, X1)
	MOVOU(X1, buffer.Offset(30*16))

	Comment("14=14^6^12^20^28")
	PXOR(X3, X0)
	PXOR(X7, X0)
	PXOR(X9, X0)
	PXOR(X5, X0)
	MOVOU(X0, buffer.Offset(14*16))

	MOVOU(b.Offset(1*16), X0)
	MOVOU(b.Offset(9*16), X1)
	MOVOU(b.Offset(17*16), X2)
	MOVOU(b.Offset(25*16), X3)
	MOVOU(b.Offset(19*16), X5)
	MOVOU(b.Offset(23*16), X6)
	MOVOU(b.Offset(27*16), X7)
	MOVOU(b.Offset(31*16), X8)
	MOVOU(b.Offset(3*16), X9)

	Comment("1=1^25^15^23^31")
	MOVOU(X0, X4)
	PXOR(X3, X4)
	PXOR(b.Offset(15*16), X4)
	PXOR(X6, X4)
	PXOR(X8, X4)
	MOVOU(X4, buffer.Offset(1*16))

	Comment("3=3^27^1^9^17")
	MOVOU(X0, X4)
	PXOR(X9, X4)
	PXOR(X7, X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	MOVOU(X4, buffer.Offset(3*16))

	Comment("9=9^1^23^31^7")
	MOVOU(X0, X4)
	PXOR(X1, X4)
	PXOR(X6, X4)
	PXOR(X8, X4)
	PXOR(b.Offset(7*16), X4)
	MOVOU(X4, buffer.Offset(9*16))

	Comment("19=1^19^11^17^25")
	MOVOU(X0, X4)
	PXOR(X5, X4)
	PXOR(b.Offset(11*16), X4)
	PXOR(X2, X4)
	PXOR(X3, X4)
	MOVOU(X4, buffer.Offset(19*16))

	Comment("27=1^27^19^25^9")
	PXOR(X1, X0)
	PXOR(X7, X0)
	PXOR(X5, X0)
	PXOR(X3, X0)
	MOVOU(X0, buffer.Offset(27*16))

	Comment("11=11^3^9^17^25")
	MOVOU(X9, X4)
	PXOR(b.Offset(11*16), X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	PXOR(X3, X4)
	MOVOU(X4, buffer.Offset(11*16))

	MOVOU(b.Offset(7*16), X0)
	MOVOU(b.Offset(15*16), X5)
	Comment("17=17^9^31^7^15")
	PXOR(X2, X1)
	PXOR(X8, X1)
	PXOR(X0, X1)
	PXOR(X5, X1)
	MOVOU(X1, buffer.Offset(17*16))

	Comment("25=25^17^7^15^23")
	PXOR(X3, X2)
	PXOR(X0, X2)
	PXOR(X5, X2)
	PXOR(X6, X2)
	MOVOU(X2, buffer.Offset(25*16))

	MOVOU(b.Offset(5*16), X1)
	MOVOU(b.Offset(11*16), X2)
	MOVOU(b.Offset(13*16), X3)
	// X0=7, X1=5, X9=X4=3, X2=11, X3=13, X5=15, X6=23, X7=27, X8=31
	Comment("5=5^29^3^11^19")
	MOVOU(X9, X4)
	PXOR(X1, X4)
	PXOR(X2, X4)
	PXOR(b.Offset(19*16), X4)
	PXOR(b.Offset(29*16), X4)
	MOVOU(X4, buffer.Offset(5*16))

	Comment("21=21^13^19^27^3")
	MOVOU(X9, X4)
	PXOR(b.Offset(21*16), X4)
	PXOR(X3, X4)
	PXOR(b.Offset(19*16), X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(21*16))

	Comment("29=29^21^27^3^11")
	PXOR(b.Offset(29*16), X9)
	PXOR(b.Offset(21*16), X9)
	PXOR(X7, X9)
	PXOR(X2, X9)
	MOVOU(X9, buffer.Offset(29*16))

	MOVOU(b.Offset(21*16), X9)
	// X0=7, X1=5, X9=21, X2=11, X3=13, X5=15, X6=23, X7=27, X8=31

	Comment("7=7^31^5^13^21")
	MOVOU(X1, X4)
	PXOR(X0, X4)
	PXOR(X3, X4)
	PXOR(X8, X4)
	PXOR(X9, X4)
	MOVOU(X4, buffer.Offset(7*16))

	Comment("13=13^5^11^19^27")
	MOVOU(X1, X4)
	PXOR(X3, X4)
	PXOR(X2, X4)
	PXOR(b.Offset(19*16), X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(13*16))

	MOVOU(b.Offset(29*16), X7)
	// X0=7, X1=5, X9=21, X2=11, X3=13, X5=15, X6=23, X7=29, X8=31
	Comment("23=23^15^21^29^5")
	MOVOU(X1, X4)
	PXOR(X5, X4)
	PXOR(X6, X4)
	PXOR(X9, X4)
	PXOR(X7, X4)
	MOVOU(X4, buffer.Offset(23*16))

	Comment("31=31^23^29^5^13")
	PXOR(X8, X1)
	PXOR(X6, X1)
	PXOR(X3, X1)
	PXOR(X7, X1)
	MOVOU(X1, buffer.Offset(31*16))

	Comment("15=15^7^13^21^29")
	PXOR(X3, X0)
	PXOR(X7, X0)
	PXOR(X9, X0)
	PXOR(X5, X0)
	MOVOU(X0, buffer.Offset(15*16))

	RET()
}

func main() {
	ConstraintExpr("amd64,gc,!purego")
	transpose64()
	transpose64Rev()
	transpose128()
	xor32x128()
	xorRoundKey128()
	sbox128()
	l128()

	Generate()
}

// zero zeroes a new register and returns it.
func zero() Register {
	r := GP64()
	XORQ(r, r)
	return r
}
