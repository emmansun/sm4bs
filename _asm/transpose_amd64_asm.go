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

func xor128() {
	// xor128 function
	TEXT("xor128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = x xor y")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	PXOR(X, Y)
	MOVOU(Y, out)

	RET()
}

func nxor128() {
	// nxor128 function
	TEXT("nxor128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = not(x xor y)")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	PXOR(X, Y)
	PCMPEQB(X, X)
	PANDN(X, Y)
	MOVOU(Y, out)

	RET()
}

func or128() {
	// or128 function
	TEXT("or128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = x or y")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	POR(X, Y)
	MOVOU(Y, out)

	RET()
}

func nor128() {
	// nor128 function
	TEXT("nor128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = not(x or y)")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	POR(X, Y)
	PCMPEQB(X, X)
	PANDN(X, Y)
	MOVOU(Y, out)

	RET()
}

func and128() {
	// and128 function
	TEXT("and128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = x and y")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	PAND(X, Y)
	MOVOU(Y, out)

	RET()
}

func nand128() {
	// nand128 function
	TEXT("nand128", NOSPLIT, "func(x, y, out *byte)")
	Doc("out = not(x and y)")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	MOVOU(y, Y)
	PAND(X, Y)
	PCMPEQB(X, X)
	PANDN(X, Y)
	MOVOU(Y, out)

	RET()
}

func not128() {
	// not128 function
	TEXT("not128", NOSPLIT, "func(x *byte)")
	Doc("not(x)")
	x := Mem{Base: Load(Param("x"), GP64())}

	X := XMM()
	Y := XMM()

	MOVOU(x, X)
	PCMPEQB(Y, Y)
	PANDN(Y, X)
	MOVOU(X, x)

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

func expandRoundKey128() {
	// xor32x128 function
	TEXT("expandRoundKey128", NOSPLIT, "func(x uint32, out *byte)")
	Doc("16 bytes per bit")

	x := Load(Param("x"), GP32())
	out := Mem{Base: Load(Param("out"), GP64())}
	zero := XMM()
	PXOR(zero, zero)
	one := XMM()
	PCMPEQB(one, one)

	y := GP32()

	count := GP64()
	XORQ(count, count)
	Comment("Handle first byte")
	MOVL(U32(0x01000000), y)
	Label("rk_loop_1")
	TESTL(x, y)
	JNZ(LabelRef("rk_loop_1_1"))
	MOVOU(zero, out.Idx(count, 1))
	JMP(LabelRef("rk_loop_1_c"))
	Label("rk_loop_1_1")
	MOVOU(one, out.Idx(count, 1))
	Label("rk_loop_1_c")
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(128))
	JL(LabelRef("rk_loop_1"))

	Comment("Handle second byte")
	MOVL(U32(0x00010000), y)
	Label("rk_loop_2")
	TESTL(x, y)
	JNZ(LabelRef("rk_loop_2_1"))
	MOVOU(zero, out.Idx(count, 1))
	JMP(LabelRef("rk_loop_2_c"))
	Label("rk_loop_2_1")
	MOVOU(one, out.Idx(count, 1))
	Label("rk_loop_2_c")
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(256))
	JL(LabelRef("rk_loop_2"))

	Comment("Handle third byte")
	MOVL(U32(0x00000100), y)
	Label("rk_loop_3")
	TESTL(x, y)
	JNZ(LabelRef("rk_loop_3_1"))
	MOVOU(zero, out.Idx(count, 1))
	JMP(LabelRef("rk_loop_3_c"))
	Label("rk_loop_3_1")
	MOVOU(one, out.Idx(count, 1))
	Label("rk_loop_3_c")
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(384))
	JL(LabelRef("rk_loop_3"))

	Comment("Handle last byte")
	MOVL(U32(0x00000001), y)
	Label("rk_loop_4")
	TESTL(x, y)
	JNZ(LabelRef("rk_loop_4_1"))
	MOVOU(zero, out.Idx(count, 1))
	JMP(LabelRef("rk_loop_4_c"))
	Label("rk_loop_4_1")
	MOVOU(one, out.Idx(count, 1))
	Label("rk_loop_4_c")
	ROLL(U8(1), y)
	ADDQ(U8(16), count)
	CMPQ(count, U32(512))
	JL(LabelRef("rk_loop_4"))

	RET()
}

func main() {
	ConstraintExpr("amd64,gc,!purego")
	transpose64()
	transpose64Rev()
	transpose128()
	xor128()
	nxor128()
	or128()
	nor128()
	and128()
	nand128()
	not128()
	xor32x128()
	expandRoundKey128()

	Generate()
}

// zero zeroes a new register and returns it.
func zero() Register {
	r := GP64()
	XORQ(r, r)
	return r
}
