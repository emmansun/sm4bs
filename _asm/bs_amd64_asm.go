package main

import (
	. "github.com/mmcloughlin/avo/build"
	. "github.com/mmcloughlin/avo/operand"
	. "github.com/mmcloughlin/avo/reg"
)

//go:generate go run . -out ../bs_amd64.s -stubs ../bs_amd64.go -pkg sm4bs

func transpose64Rev() {
	// transpose64Rev function
	TEXT("transpose64Rev", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, just for test, 128x64")

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
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=64")
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_rev64"))
	ADDQ(Imm(16), rr)

	Comment("Compare rr with nrows, here nrows=128")
	CMPQ(rr, U8(128))
	JL(LabelRef("row_loop_rev64"))

	RET()
}

func getFirst4Bytes64(flipMask, in Mem, o, addr, x Register) {
	for i := 0; i < 4; i++ {
		MOVL(in.Idx(addr, 1), o)
		PINSRD(Imm(uint64(i)), o, x)
		ADDQ(Imm(8), addr)
	}
	PSHUFB(flipMask, x)
}

func getFirst4Bytes128(flipMask, in Mem, o, addr, x Register) {
	for i := 0; i < 4; i++ {
		MOVL(in.Idx(addr, 1), o)
		PINSRD(Imm(uint64(i)), o, x)
		ADDQ(Imm(16), addr)
	}
	PSHUFB(flipMask, x)
}

func getFirst4Bytes256(flipMask, in Mem, o, addr, x Register) {
	for i := 0; i < 4; i++ {
		MOVL(in.Idx(addr, 1), o)
		PINSRD(Imm(uint64(i)), o, x)
		ADDQ(Imm(32), addr)
	}
	PSHUFB(flipMask, x)
}

func transpose128avx(flipMask Mem) {
	// transpose128avx function
	TEXT("transpose128avx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 128x128")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	tmp := Y0
	o := GP32()
	cc := GP64()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(7), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop"))
	ADDQ(Imm(32), rr)
	Comment("Compare rr with nrows, here nrows=128")
	CMPQ(rr, U8(128))
	JL(LabelRef("row_loop"))

	VZEROUPPER()
	RET()
}

func transpose256avx(flipMask Mem) {
	// transpose256avx function
	TEXT("transpose256avx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 256x128 => 128x256")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	tmp := Y0
	o := GP32()
	cc := GP64()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(7), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 256")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(8), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(32), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 256")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(8), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(32), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 256")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(8), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(32), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 256")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(8), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(32), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop"))
	ADDQ(Imm(32), rr)
	Comment("Compare rr with nrows, here nrows=256")
	CMPQ(rr, U32(256))
	JL(LabelRef("row_loop"))

	VZEROUPPER()
	RET()
}

func transpose128x256avx2(flipMask Mem) {
	// transpose128x256avx2 function
	TEXT("transpose128x256avx2", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 128x256 => 256x128, just for test here.")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	tmp := Y0
	o := GP32()
	cc := GP64()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=256")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(8), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes256(flipMask, in, o, addr, t1)
	getFirst4Bytes256(flipMask, in, o, addr, t2)
	getFirst4Bytes256(flipMask, in, o, addr, t3)
	getFirst4Bytes256(flipMask, in, o, addr, t4)
	getFirst4Bytes256(flipMask, in, o, addr, t5)
	getFirst4Bytes256(flipMask, in, o, addr, t6)
	getFirst4Bytes256(flipMask, in, o, addr, t7)
	getFirst4Bytes256(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(7), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=256")
	CMPQ(cc, U32(256))
	JL(LabelRef("col_loop"))
	ADDQ(Imm(32), rr)
	Comment("Compare rr with nrows, here nrows=128")
	CMPQ(rr, U32(128))
	JL(LabelRef("row_loop"))

	VZEROUPPER()
	RET()
}

func transpose128RevAvx(flipMask Mem) {
	// transpose128RevAvx function
	TEXT("transpose128RevAvx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, b0-b1-b2-b3, 128x128")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	tmp := Y0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	o := GP32()

	Comment("Initialize rr, current row, 96")
	rr := zero()
	cc := GP64()
	addr := GP64()

	Label("row_loop_b3")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b3")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	MOVQ(U32(12288), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop_b3"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b2")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b2")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	MOVQ(U32(8192), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop_b2"))

	ADDQ(Imm(32), rr)

	Label("row_loop_b1")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b1")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	MOVQ(U32(4096), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop_b1"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b0")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b0")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	MOVQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop_b0"))

	VZEROUPPER()
	RET()
}

func transpose256RevAvx(flipMask Mem) {
	// transpose256RevAvx function
	TEXT("transpose256RevAvx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, b0-b1-b2-b3, 128x256")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	tmp := Y0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	o := GP32()

	Comment("Initialize rr, current row, 96")
	rr := zero()
	cc := GP64()
	addr := GP64()

	Label("row_loop_b3")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b3")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=256")
	MOVQ(U32(24576), addr) // 96 * ncols
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes256(flipMask, in, o, addr, t1)
	getFirst4Bytes256(flipMask, in, o, addr, t2)
	getFirst4Bytes256(flipMask, in, o, addr, t3)
	getFirst4Bytes256(flipMask, in, o, addr, t4)
	getFirst4Bytes256(flipMask, in, o, addr, t5)
	getFirst4Bytes256(flipMask, in, o, addr, t6)
	getFirst4Bytes256(flipMask, in, o, addr, t7)
	getFirst4Bytes256(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=256")
	CMPQ(cc, U32(256))
	JL(LabelRef("col_loop_b3"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b2")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b2")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=256")
	MOVQ(U32(16384), addr) // 64 * ncols
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes256(flipMask, in, o, addr, t1)
	getFirst4Bytes256(flipMask, in, o, addr, t2)
	getFirst4Bytes256(flipMask, in, o, addr, t3)
	getFirst4Bytes256(flipMask, in, o, addr, t4)
	getFirst4Bytes256(flipMask, in, o, addr, t5)
	getFirst4Bytes256(flipMask, in, o, addr, t6)
	getFirst4Bytes256(flipMask, in, o, addr, t7)
	getFirst4Bytes256(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=256")
	CMPQ(cc, U32(256))
	JL(LabelRef("col_loop_b2"))

	ADDQ(Imm(32), rr)

	Label("row_loop_b1")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b1")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=256")
	MOVQ(U32(8192), addr) // 32 * ncols
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes256(flipMask, in, o, addr, t1)
	getFirst4Bytes256(flipMask, in, o, addr, t2)
	getFirst4Bytes256(flipMask, in, o, addr, t3)
	getFirst4Bytes256(flipMask, in, o, addr, t4)
	getFirst4Bytes256(flipMask, in, o, addr, t5)
	getFirst4Bytes256(flipMask, in, o, addr, t6)
	getFirst4Bytes256(flipMask, in, o, addr, t7)
	getFirst4Bytes256(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=256")
	CMPQ(cc, U32(256))
	JL(LabelRef("col_loop_b1"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b0")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b0")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=256")
	MOVQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes256(flipMask, in, o, addr, t1)
	getFirst4Bytes256(flipMask, in, o, addr, t2)
	getFirst4Bytes256(flipMask, in, o, addr, t3)
	getFirst4Bytes256(flipMask, in, o, addr, t4)
	getFirst4Bytes256(flipMask, in, o, addr, t5)
	getFirst4Bytes256(flipMask, in, o, addr, t6)
	getFirst4Bytes256(flipMask, in, o, addr, t7)
	getFirst4Bytes256(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=256")
	CMPQ(cc, U32(256))
	JL(LabelRef("col_loop_b0"))

	VZEROUPPER()
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

func xor32x128avx() {
	// xor32x128 function
	TEXT("xor32x128avx", NOSPLIT, "func(len int, x, y, out *byte)")
	Doc("out = x xor y")
	x := Mem{Base: Load(Param("x"), GP64())}
	y := Mem{Base: Load(Param("y"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}
	len := Load(Param("len"), GP64())

	X := YMM()
	Y := YMM()

	count := zero()
	Label("xor32_loop_avx")
	VMOVDQU(x.Idx(count, 1), X)
	VMOVDQU(y.Idx(count, 1), Y)
	VPXOR(X, Y, Y)
	VMOVDQU(Y, out.Idx(count, 1))
	ADDQ(U8(32), count)
	CMPQ(count, len)
	JL(LabelRef("xor32_loop_avx"))

	VZEROUPPER()
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

	tmp1, tmp2, tmp3, tmp := XMM(), XMM(), XMM(), XMM()
	ret := XMM()
	one := XMM()

	y := GP32()

	count := GP64()
	XORQ(count, count)
	MOVQ(x, tmp2)

	Comment("Handle first byte")
	MOVL(U32(0x01000000), y)
	MOVQ(y, tmp1)
	VMOVDQU(tmp1, tmp3)
	Label("rk_loop_1")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(16), count)
	CMPQ(count, U32(128))
	JL(LabelRef("rk_loop_1"))

	Comment("Handle second byte")
	VPSRLD(Imm(8), tmp3, tmp1)
	Label("rk_loop_2")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(16), count)
	CMPQ(count, U32(256))
	JL(LabelRef("rk_loop_2"))

	Comment("Handle third byte")
	VPSRLD(Imm(16), tmp3, tmp1)
	Label("rk_loop_3")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(16), count)
	CMPQ(count, U32(384))
	JL(LabelRef("rk_loop_3"))

	Comment("Handle last byte")
	VPSRLD(Imm(24), tmp3, tmp1)
	Label("rk_loop_4")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(16), count)
	CMPQ(count, U32(512))
	JL(LabelRef("rk_loop_4"))

	VZEROUPPER()
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
	Comment("t2=m0 & m1")
	PAND(t6, t3) // t2 := t3

	Comment("t3=g0 & g4")
	PAND(buffer, t2) // t3 := t2

	Comment("t4=g3 & g7")
	MOVOU(t1, t6)
	PAND(t5, t1) // t4 := t1

	Comment("t7=g3 | g7")
	POR(t6, t5) // t7 := t5

	Comment("t11=m4 & m5")
	PAND(t4, t11) // t11

	MOVOU(buffer.Offset((8+3)*16), t4) // t4 = m3
	MOVOU(t4, t6)
	Comment("t10=m3 & m2")
	PAND(t8, t6) // t10 := t6
	Comment("t12=m3 | m2")
	POR(t8, t4) // t12 := t4

	Comment("t6=g6 | g2")
	POR(buffer.Offset(6*16), t7) // t6 := t7

	Comment("t9=m6 | m7")
	POR(buffer.Offset((8+6)*16), t12) // t9 := t12

	t10 = XMM()
	MOVOU(buffer.Offset((8+9)*16), t8) // t8 = m9
	MOVOU(t8, t10)

	Comment("t5=m8 & m9")
	PAND(t9, t8) // t5 := t8
	Comment("t8=m8 | m9")
	POR(t9, t10) // t8 := t10

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
	Comment("t1 = g5 & g1")
	MOVOU(buffer.Offset(1*16), t2)
	MOVOU(buffer.Offset(5*16), t5)
	PAND(t2, t5) // t1 := t5
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
	Comment("t0 = p1 & p2")
	MOVOU(t3, t1)
	PAND(t4, t1) // t0 := t1

	Comment("t1 = p3 & p0")
	MOVOU(t8, t2)
	PAND(t7, t2) // t1 := t2

	Comment("t2 = p0 & p2")
	MOVOU(t4, t5)
	PAND(t8, t5) // t2 := t5

	Comment("t3 = p1 & p3")
	MOVOU(t3, t6)
	PAND(t7, t6) // t3 := t6

	Comment("t4 = t0 & t2")
	MOVOU(t1, t9)
	PAND(t5, t9) // t4 := t9

	Comment("t5 = t1 & t3")
	MOVOU(t2, t10)
	PXOR(t6, t10) // t5 := t10

	Comment("t6 = t5 | p0")
	POR(t10, t8) // t6 := t8

	Comment("t7 = t2 | p3")
	POR(t5, t7) // t7

	Comment("t8 = t4 ^ t6")
	PXOR(t9, t8) // l3 = t8

	Comment("t9 = t7 ^ t3")
	PXOR(t7, t6) // t9 := t6

	Comment("t10 = t0 ^ t9")
	PXOR(t1, t6) // l0 = t10 := t6

	Comment("t11 = p2 | t5")
	POR(t10, t4) // t11 := t4
	Comment("l1 = t11 ^ t1")
	PXOR(t4, t2) // l1 := t2

	Comment("t12 = p1 | t2")
	POR(t5, t3) // t12 := t3
	Comment("l2 = t12 ^ t5")
	PXOR(t10, t3) // l2 := t3

	Comment("start bottom function")
	Comment("current register status: t6=l0, t2=l1, t3=l2, t8=l3")
	Comment("k4 = l2 ^ l3")
	MOVOU(t8, t5)
	PXOR(t3, t5) // k4 := t5

	Comment("k3 = l1 ^ l3")
	MOVOU(t8, t4)
	PXOR(t2, t4) // k3 := t4

	Comment("k2 = l0 ^ l2")
	MOVOU(t6, t7)
	PXOR(t3, t7) // k2 := t7

	Comment("k0 = l0 ^ l1")
	MOVOU(t6, t1)
	PXOR(t2, t1) // k0 := t1

	Comment("k1 = k2 ^ k3")
	MOVOU(t4, t9)
	PXOR(t7, t9) // k1 := t9

	Comment("e0=(m1 & k0)")
	MOVOU(buffer.Offset((8+1)*16), t10) // m1
	PAND(t1, t10)                       // e0 := t10

	Comment("e1=(g5 & l1)")
	MOVOU(buffer.Offset(5*16), t11)
	PAND(t2, t11) // e1 := t11

	Comment("r0=e0 ^ e1")
	PXOR(t11, t10) // r0 = e0 ^ e1

	Comment("e2=(g4 & l0)")
	MOVOU(buffer.Offset(4*16), t12)
	PAND(t6, t12)

	Comment("r1=e2 ^ e1")
	PXOR(t12, t11) // r1 = e2 ^ e1

	Comment("store r0 r1")
	MOVOU(t10, buffer.Offset(22*16)) // in fact, we can start from 18*16
	MOVOU(t11, buffer.Offset(23*16))

	Comment("e3=(m7 & k3)")
	MOVOU(buffer.Offset((8+7)*16), t10) // m7
	PAND(t4, t10)

	Comment("e4=(m5 & k2)")
	MOVOU(buffer.Offset((8+5)*16), t11) // m5
	PAND(t7, t11)
	Comment("r2=e3 ^ e4")
	PXOR(t11, t10) // r2 = e3 ^ e4

	Comment("e5=(m3 & k1)")
	MOVOU(buffer.Offset((8+3)*16), t12) // m3
	PAND(t9, t12)
	Comment("r3=e5 ^ e4")
	PXOR(t12, t11) // r3 = e5 ^ e4

	Comment("store r2 r3")
	MOVOU(t10, buffer.Offset(24*16))
	MOVOU(t11, buffer.Offset(25*16))

	Comment("e6=(m9 & k4)")
	MOVOU(buffer.Offset((8+9)*16), t10) // m9
	PAND(t5, t10)

	Comment("e7=(g7 & l3)")
	MOVOU(buffer.Offset(7*16), t11)
	PAND(t8, t11)
	Comment("r4=e7 ^ e6")
	PXOR(t11, t10) // r4 = e6 ^ e7

	Comment("e8=(g6 & l2)")
	MOVOU(buffer.Offset(6*16), t12)
	PAND(t3, t12)
	Comment("r5=e8 ^ e6")
	PXOR(t11, t12) // r5 = e8 ^ e7

	Comment("store r4")
	MOVOU(t10, buffer.Offset(26*16))

	Comment("e9=(m0 & k0)")
	MOVOU(buffer.Offset((8+0)*16), t10) // m0
	PAND(t1, t10)                       // e9 := t10

	Comment("e10=(g1 & l1)")
	MOVOU(buffer.Offset(1*16), t1)
	PAND(t2, t1) // e10 := t1

	Comment("r6=e9 ^ e10")
	PXOR(t1, t10) // r6 = e9 ^ e10

	Comment("e11=(g0 & l0)")
	MOVOU(buffer, t11)
	PAND(t11, t6) // e11 := t6
	Comment("r7=e11 ^ e10")
	PXOR(t6, t1) // r7 = e11 ^ e10 = t1

	Comment("e12=(m6 & k3)")
	MOVOU(buffer.Offset((8+6)*16), t2) // m6
	PAND(t4, t2)

	Comment("e13=(m4 & k2)")
	MOVOU(buffer.Offset((8+4)*16), t6) // m4
	PAND(t7, t6)

	Comment("r8=e12 ^ e13")
	PXOR(t6, t2) // r8 = e12 ^ e13 = t2

	Comment("e14=(m2 & k1)")
	MOVOU(buffer.Offset((8+2)*16), t4) // m2
	PAND(t9, t4)
	Comment("r9=e14 ^ e13")
	PXOR(t6, t4) // r9 = e14 ^ e13  = t4

	Comment("e15=(m8 & k4)")
	MOVOU(buffer.Offset((8+8)*16), t9) // m8
	PAND(t9, t5)

	Comment("e16=(g3 & l3)")
	MOVOU(buffer.Offset(3*16), t9)
	PAND(t9, t8)
	Comment("r10=e15 ^ e16")
	PXOR(t8, t5) // r10 = e15 ^ e16 = t5

	Comment("e17=(g2 & l2)")
	MOVOU(buffer.Offset(2*16), t11)
	PAND(t11, t3)
	Comment("r11=e17 ^ e16")
	PXOR(t8, t3) // r11 = e17 ^ e16 = t3

	Comment("start output function")
	// t12 = r5
	// t10 = r6
	// t1 = r7
	// t2 = r8
	// t4 = r9
	// t5 = r10
	// t3 = r11
	Comment("[t1]=r7 ^ r9")
	PXOR(t1, t4) // [t1] = t4 = r7 ^ r9

	Comment("[t2]=t1 ^ r1")
	MOVOU(buffer.Offset((22+1)*16), t6) // r1
	PXOR(t4, t6)                        // [t2] = t6 = r1 ^ [t1]

	Comment("[t3]=t2 ^ r3")
	MOVOU(buffer.Offset((22+3)*16), t7) // r3
	MOVOU(t7, t8)
	PXOR(t6, t7) // [t3] = t7 = r3 ^ [t2]
	Comment("[t4]=r5 ^ r3")
	PXOR(t12, t8) // [t4] = t8 = r5 ^ r3

	Comment("[t5]=r4 ^ [t4]")
	MOVOU(buffer.Offset((22+4)*16), t9) // r4
	MOVOU(t9, t11)
	PXOR(t8, t9) // [t5] = t9 = r4 ^ t4
	Comment("[t6]=r0 ^ r4")
	PXOR(buffer.Offset(22*16), t11) // [t6] = t11 = r4 ^ r0

	Comment("[t7]=r11 ^ r7")
	PXOR(t3, t1) // [t7] t1 = r7 ^ r11

	Comment("[t8]=[t1] ^ [t4]")
	PXOR(t4, t8) // t8 = t4 ^ t11
	Comment("store t8")
	MOVOU(t8, b.Offset(5*16))

	Comment("[t9]=[t1] ^ [t6]")
	PXOR(t11, t4) // [t9] = t4
	Comment("store t9")
	MOVOU(t4, b.Offset(2*16))

	Comment("[t10]=r2 ^ t5")
	PXOR(buffer.Offset((22+2)*16), t9) // [t10] t9 = r2 ^ [t5]
	Comment("[t11]=r10 ^ r8")
	PXOR(t5, t2) // [t11] = t2
	Comment("store t11")
	MOVOU(t2, b.Offset(3*16))
	Comment("[t12]=^([t3] ^ [t11])")
	PXOR(t7, t2)
	PANDN(f, t2) // [t12] = t2
	Comment("store t12")
	MOVOU(t2, b.Offset(1*16))
	Comment("[t13]=[t10] ^ [t12]")
	PXOR(t2, t9) // [t13] = t9
	Comment("store t13")
	MOVOU(t9, b.Offset(6*16))

	Comment("[t14]=^([t3] ^ [t7])")
	PXOR(t7, t1)
	PANDN(f, t1) // [t14]
	Comment("store t14")
	MOVOU(t1, b.Offset(4*16))
	Comment("[t16]=[t6] ^ [t14]")
	PXOR(t11, t1) // [t16]
	Comment("store t16")
	MOVOU(t1, b)

	Comment("[t15]=^(r10 ^ r6)")
	PXOR(t10, t5)
	PANDN(f, t5)
	Comment("store t15")
	MOVOU(t5, b.Offset(7*16))

	RET()
}

// 0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23 | 24 25 26 27 28 29 30 31
// 24 25 26 27 28 29 30 31 |  0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23
// 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29
// 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5
// 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13
func l128() {
	// l128 function
	TEXT("l128", NOSPLIT, "func(x, buffer *byte)")
	Doc("l128 and xor buffer, 128 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	y0, y1, y2, y3, y4, y5, y6, y7, y8, y9 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()

	MOVOU(b, y0)
	MOVOU(b.Offset(8*16), y1)
	MOVOU(b.Offset(16*16), y2)
	MOVOU(b.Offset(24*16), y3)
	MOVOU(b.Offset(18*16), y5)
	MOVOU(b.Offset(22*16), y6)
	MOVOU(b.Offset(26*16), y7)
	MOVOU(b.Offset(30*16), y8)
	MOVOU(b.Offset(2*16), y9)

	Comment("0=0^24^14^22^30")
	MOVOU(y0, y4)
	PXOR(y3, y4)
	PXOR(b.Offset(14*16), y4)
	PXOR(y6, y4)
	PXOR(y8, y4)
	PXOR(buffer, y4)
	MOVOU(y4, buffer)

	Comment("2=0^2^26^8^16")
	MOVOU(y0, y4)
	PXOR(y9, y4)
	PXOR(y7, y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(buffer.Offset(2*16), y4)
	MOVOU(y4, buffer.Offset(2*16))

	Comment("8=0^8^22^30^6")
	MOVOU(y0, y4)
	PXOR(y1, y4)
	PXOR(y6, y4)
	PXOR(y8, y4)
	PXOR(b.Offset(6*16), y4)
	PXOR(buffer.Offset(8*16), y4)
	MOVOU(y4, buffer.Offset(8*16))

	Comment("18=0^18^10^16^24")
	MOVOU(y0, y4)
	PXOR(y5, y4)
	PXOR(b.Offset(10*16), y4)
	PXOR(y2, y4)
	PXOR(y3, y4)
	PXOR(buffer.Offset(18*16), y4)
	MOVOU(y4, buffer.Offset(18*16))

	Comment("26=0^26^18^24^8")
	PXOR(y1, y0)
	PXOR(y7, y0)
	PXOR(y5, y0)
	PXOR(y3, y0)
	PXOR(buffer.Offset(26*16), y0)
	MOVOU(y0, buffer.Offset(26*16))

	Comment("10=10^2^8^16^24")
	MOVOU(y9, y4)
	PXOR(b.Offset(10*16), y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(y3, y4)
	PXOR(buffer.Offset(10*16), y4)
	MOVOU(y4, buffer.Offset(10*16))

	MOVOU(b.Offset(6*16), y0)
	MOVOU(b.Offset(14*16), y5)
	Comment("16=16^8^30^6^14")
	PXOR(y2, y1)
	PXOR(y8, y1)
	PXOR(y0, y1)
	PXOR(y5, y1)
	PXOR(buffer.Offset(16*16), y1)
	MOVOU(y1, buffer.Offset(16*16))

	Comment("24=24^16^6^14^22")
	PXOR(y3, y2)
	PXOR(y0, y2)
	PXOR(y5, y2)
	PXOR(y6, y2)
	PXOR(buffer.Offset(24*16), y2)
	MOVOU(y2, buffer.Offset(24*16))

	MOVOU(b.Offset(4*16), y1)
	MOVOU(b.Offset(10*16), y2)
	MOVOU(b.Offset(12*16), y3)
	// y0=6, y1=4, y9=y4=2, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30
	Comment("4=4^28^2^10^18")
	MOVOU(y9, y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(b.Offset(18*16), y4)
	PXOR(b.Offset(28*16), y4)
	PXOR(buffer.Offset(4*16), y4)
	MOVOU(y4, buffer.Offset(4*16))

	Comment("20=20^12^18^26^2")
	MOVOU(y9, y4)
	PXOR(b.Offset(20*16), y4)
	PXOR(y3, y4)
	PXOR(b.Offset(18*16), y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(20*16), y4)
	MOVOU(y4, buffer.Offset(20*16))

	Comment("28=28^20^26^2^10")
	PXOR(b.Offset(28*16), y9)
	PXOR(b.Offset(20*16), y9)
	PXOR(y7, y9)
	PXOR(y2, y9)
	PXOR(buffer.Offset(28*16), y9)
	MOVOU(y9, buffer.Offset(28*16))

	MOVOU(b.Offset(20*16), y9)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30

	Comment("6=6^30^4^12^20")
	MOVOU(y1, y4)
	PXOR(y0, y4)
	PXOR(y3, y4)
	PXOR(y8, y4)
	PXOR(y9, y4)
	PXOR(buffer.Offset(6*16), y4)
	MOVOU(y4, buffer.Offset(6*16))

	Comment("12=12^4^10^18^26")
	MOVOU(y1, y4)
	PXOR(y3, y4)
	PXOR(y2, y4)
	PXOR(b.Offset(18*16), y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(12*16), y4)
	MOVOU(y4, buffer.Offset(12*16))

	MOVOU(b.Offset(28*16), y7)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=28, y8=30
	Comment("22=22^14^20^28^4")
	MOVOU(y1, y4)
	PXOR(y5, y4)
	PXOR(y6, y4)
	PXOR(y9, y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(22*16), y4)
	MOVOU(y4, buffer.Offset(22*16))

	Comment("30=30^22^28^4^12")
	PXOR(y8, y1)
	PXOR(y6, y1)
	PXOR(y3, y1)
	PXOR(y7, y1)
	PXOR(buffer.Offset(30*16), y1)
	MOVOU(y1, buffer.Offset(30*16))

	Comment("14=14^6^12^20^28")
	PXOR(y3, y0)
	PXOR(y7, y0)
	PXOR(y9, y0)
	PXOR(y5, y0)
	PXOR(buffer.Offset(14*16), y0)
	MOVOU(y0, buffer.Offset(14*16))

	MOVOU(b.Offset(1*16), y0)
	MOVOU(b.Offset(9*16), y1)
	MOVOU(b.Offset(17*16), y2)
	MOVOU(b.Offset(25*16), y3)
	MOVOU(b.Offset(19*16), y5)
	MOVOU(b.Offset(23*16), y6)
	MOVOU(b.Offset(27*16), y7)
	MOVOU(b.Offset(31*16), y8)
	MOVOU(b.Offset(3*16), y9)

	Comment("1=1^25^15^23^31")
	MOVOU(y0, y4)
	PXOR(y3, y4)
	PXOR(b.Offset(15*16), y4)
	PXOR(y6, y4)
	PXOR(y8, y4)
	PXOR(buffer.Offset(1*16), y4)
	MOVOU(y4, buffer.Offset(1*16))

	Comment("3=3^27^1^9^17")
	MOVOU(y0, y4)
	PXOR(y9, y4)
	PXOR(y7, y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(buffer.Offset(3*16), y4)
	MOVOU(y4, buffer.Offset(3*16))

	Comment("9=9^1^23^31^7")
	MOVOU(y0, y4)
	PXOR(y1, y4)
	PXOR(y6, y4)
	PXOR(y8, y4)
	PXOR(b.Offset(7*16), y4)
	PXOR(buffer.Offset(9*16), y4)
	MOVOU(y4, buffer.Offset(9*16))

	Comment("19=1^19^11^17^25")
	MOVOU(y0, y4)
	PXOR(y5, y4)
	PXOR(b.Offset(11*16), y4)
	PXOR(y2, y4)
	PXOR(y3, y4)
	PXOR(buffer.Offset(19*16), y4)
	MOVOU(y4, buffer.Offset(19*16))

	Comment("27=1^27^19^25^9")
	PXOR(y1, y0)
	PXOR(y7, y0)
	PXOR(y5, y0)
	PXOR(y3, y0)
	PXOR(buffer.Offset(27*16), y0)
	MOVOU(y0, buffer.Offset(27*16))

	Comment("11=11^3^9^17^25")
	MOVOU(y9, y4)
	PXOR(b.Offset(11*16), y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(y3, y4)
	PXOR(buffer.Offset(11*16), y4)
	MOVOU(y4, buffer.Offset(11*16))

	MOVOU(b.Offset(7*16), y0)
	MOVOU(b.Offset(15*16), y5)
	Comment("17=17^9^31^7^15")
	PXOR(y2, y1)
	PXOR(y8, y1)
	PXOR(y0, y1)
	PXOR(y5, y1)
	PXOR(buffer.Offset(17*16), y1)
	MOVOU(y1, buffer.Offset(17*16))

	Comment("25=25^17^7^15^23")
	PXOR(y3, y2)
	PXOR(y0, y2)
	PXOR(y5, y2)
	PXOR(y6, y2)
	PXOR(buffer.Offset(25*16), y2)
	MOVOU(y2, buffer.Offset(25*16))

	MOVOU(b.Offset(5*16), y1)
	MOVOU(b.Offset(11*16), y2)
	MOVOU(b.Offset(13*16), y3)
	// y0=7, y1=5, y9=y4=3, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31
	Comment("5=5^29^3^11^19")
	MOVOU(y9, y4)
	PXOR(y1, y4)
	PXOR(y2, y4)
	PXOR(b.Offset(19*16), y4)
	PXOR(b.Offset(29*16), y4)
	PXOR(buffer.Offset(5*16), y4)
	MOVOU(y4, buffer.Offset(5*16))

	Comment("21=21^13^19^27^3")
	MOVOU(y9, y4)
	PXOR(b.Offset(21*16), y4)
	PXOR(y3, y4)
	PXOR(b.Offset(19*16), y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(21*16), y4)
	MOVOU(y4, buffer.Offset(21*16))

	Comment("29=29^21^27^3^11")
	PXOR(b.Offset(29*16), y9)
	PXOR(b.Offset(21*16), y9)
	PXOR(y7, y9)
	PXOR(y2, y9)
	PXOR(buffer.Offset(29*16), y9)
	MOVOU(y9, buffer.Offset(29*16))

	MOVOU(b.Offset(21*16), y9)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31

	Comment("7=7^31^5^13^21")
	MOVOU(y1, y4)
	PXOR(y0, y4)
	PXOR(y3, y4)
	PXOR(y8, y4)
	PXOR(y9, y4)
	PXOR(buffer.Offset(7*16), y4)
	MOVOU(y4, buffer.Offset(7*16))

	Comment("13=13^5^11^19^27")
	MOVOU(y1, y4)
	PXOR(y3, y4)
	PXOR(y2, y4)
	PXOR(b.Offset(19*16), y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(13*16), y4)
	MOVOU(y4, buffer.Offset(13*16))

	MOVOU(b.Offset(29*16), y7)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=29, y8=31
	Comment("23=23^15^21^29^5")
	MOVOU(y1, y4)
	PXOR(y5, y4)
	PXOR(y6, y4)
	PXOR(y9, y4)
	PXOR(y7, y4)
	PXOR(buffer.Offset(23*16), y4)
	MOVOU(y4, buffer.Offset(23*16))

	Comment("31=31^23^29^5^13")
	PXOR(y8, y1)
	PXOR(y6, y1)
	PXOR(y3, y1)
	PXOR(y7, y1)
	PXOR(buffer.Offset(31*16), y1)
	MOVOU(y1, buffer.Offset(31*16))

	Comment("15=15^7^13^21^29")
	PXOR(y3, y0)
	PXOR(y7, y0)
	PXOR(y9, y0)
	PXOR(y5, y0)
	PXOR(buffer.Offset(15*16), y0)
	MOVOU(y0, buffer.Offset(15*16))

	RET()
}

// 0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23 | 24 25 26 27 28 29 30 31
// 24 25 26 27 28 29 30 31 |  0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23
// 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29
// 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5
// 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13
func l256() {
	// l256 function
	TEXT("l256", NOSPLIT, "func(x, buffer *byte)")
	Doc("l256 and xor buffer, 256 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	y0, y1, y2, y3, y4, y5, y6, y7, y8, y9 := YMM(), YMM(), YMM(), YMM(), YMM(), YMM(), YMM(), YMM(), YMM(), YMM()

	VMOVDQU(b, y0)
	VMOVDQU(b.Offset(8*32), y1)
	VMOVDQU(b.Offset(16*32), y2)
	VMOVDQU(b.Offset(24*32), y3)
	VMOVDQU(b.Offset(18*32), y5)
	VMOVDQU(b.Offset(22*32), y6)
	VMOVDQU(b.Offset(26*32), y7)
	VMOVDQU(b.Offset(30*32), y8)
	VMOVDQU(b.Offset(2*32), y9)

	Comment("0=0^24^14^22^30")
	VPXOR(y3, y0, y4)
	VPXOR(b.Offset(14*32), y4, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(buffer, y4, y4)
	VMOVDQU(y4, buffer)

	Comment("2=0^2^26^8^16")
	VPXOR(y9, y0, y4)
	VPXOR(y7, y4, y4)
	VPXOR(y1, y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(buffer.Offset(2*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(2*32))

	Comment("8=0^8^22^30^6")
	VPXOR(y1, y0, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(b.Offset(6*32), y4, y4)
	VPXOR(buffer.Offset(8*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(8*32))

	Comment("18=0^18^10^16^24")
	VPXOR(y5, y0, y4)
	VPXOR(b.Offset(10*32), y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(y3, y4, y4)
	VPXOR(buffer.Offset(18*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(18*32))

	Comment("26=0^26^18^24^8")
	VPXOR(y1, y0, y0)
	VPXOR(y7, y0, y0)
	VPXOR(y5, y0, y0)
	VPXOR(y3, y0, y0)
	VPXOR(buffer.Offset(26*32), y0, y0)
	VMOVDQU(y0, buffer.Offset(26*32))

	Comment("10=10^2^8^16^24")
	VPXOR(b.Offset(10*32), y9, y4)
	VPXOR(y1, y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(y3, y4, y4)
	VPXOR(buffer.Offset(10*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(10*32))

	VMOVDQU(b.Offset(6*32), y0)
	VMOVDQU(b.Offset(14*32), y5)
	Comment("16=16^8^30^6^14")
	VPXOR(y2, y1, y1)
	VPXOR(y8, y1, y1)
	VPXOR(y0, y1, y1)
	VPXOR(y5, y1, y1)
	VPXOR(buffer.Offset(16*32), y1, y1)
	VMOVDQU(y1, buffer.Offset(16*32))

	Comment("24=24^16^6^14^22")
	VPXOR(y3, y2, y2)
	VPXOR(y0, y2, y2)
	VPXOR(y5, y2, y2)
	VPXOR(y6, y2, y2)
	VPXOR(buffer.Offset(24*32), y2, y2)
	VMOVDQU(y2, buffer.Offset(24*32))

	VMOVDQU(b.Offset(4*32), y1)
	VMOVDQU(b.Offset(10*32), y2)
	VMOVDQU(b.Offset(12*32), y3)
	// y0=6, y1=4, y9=y4=2, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30
	Comment("4=4^28^2^10^18")
	VPXOR(y1, y9, y4)
	VPXOR(y2, y4, y4)
	VPXOR(b.Offset(18*32), y4, y4)
	VPXOR(b.Offset(28*32), y4, y4)
	VPXOR(buffer.Offset(4*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(4*32))

	Comment("20=20^12^18^26^2")
	VPXOR(b.Offset(20*32), y9, y4)
	VPXOR(y3, y4, y4)
	VPXOR(b.Offset(18*32), y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(20*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(20*32))

	Comment("28=28^20^26^2^10")
	VPXOR(b.Offset(28*32), y9, y9)
	VPXOR(b.Offset(20*32), y9, y9)
	VPXOR(y7, y9, y9)
	VPXOR(y2, y9, y9)
	VPXOR(buffer.Offset(28*32), y9, y9)
	VMOVDQU(y9, buffer.Offset(28*32))

	VMOVDQU(b.Offset(20*32), y9)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30

	Comment("6=6^30^4^12^20")
	VPXOR(y0, y1, y4)
	VPXOR(y3, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(y9, y4, y4)
	VPXOR(buffer.Offset(6*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(6*32))

	Comment("12=12^4^10^18^26")
	VPXOR(y3, y1, y4)
	VPXOR(y2, y4, y4)
	VPXOR(b.Offset(18*32), y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(12*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(12*32))

	VMOVDQU(b.Offset(28*32), y7)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=28, y8=30
	Comment("22=22^14^20^28^4")
	VPXOR(y5, y1, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y9, y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(22*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(22*32))

	Comment("30=30^22^28^4^12")
	VPXOR(y8, y1, y1)
	VPXOR(y6, y1, y1)
	VPXOR(y3, y1, y1)
	VPXOR(y7, y1, y1)
	VPXOR(buffer.Offset(30*32), y1, y1)
	VMOVDQU(y1, buffer.Offset(30*32))

	Comment("14=14^6^12^20^28")
	VPXOR(y3, y0, y0)
	VPXOR(y7, y0, y0)
	VPXOR(y9, y0, y0)
	VPXOR(y5, y0, y0)
	VPXOR(buffer.Offset(14*32), y0, y0)
	VMOVDQU(y0, buffer.Offset(14*32))

	VMOVDQU(b.Offset(1*32), y0)
	VMOVDQU(b.Offset(9*32), y1)
	VMOVDQU(b.Offset(17*32), y2)
	VMOVDQU(b.Offset(25*32), y3)
	VMOVDQU(b.Offset(19*32), y5)
	VMOVDQU(b.Offset(23*32), y6)
	VMOVDQU(b.Offset(27*32), y7)
	VMOVDQU(b.Offset(31*32), y8)
	VMOVDQU(b.Offset(3*32), y9)

	Comment("1=1^25^15^23^31")
	VPXOR(y3, y0, y4)
	VPXOR(b.Offset(15*32), y4, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(buffer.Offset(1*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(1*32))

	Comment("3=3^27^1^9^17")
	VPXOR(y9, y0, y4)
	VPXOR(y7, y4, y4)
	VPXOR(y1, y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(buffer.Offset(3*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(3*32))

	Comment("9=9^1^23^31^7")
	VPXOR(y1, y0, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(b.Offset(7*32), y4, y4)
	VPXOR(buffer.Offset(9*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(9*32))

	Comment("19=1^19^11^17^25")
	VPXOR(y5, y0, y4)
	VPXOR(b.Offset(11*32), y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(y3, y4, y4)
	VPXOR(buffer.Offset(19*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(19*32))

	Comment("27=1^27^19^25^9")
	VPXOR(y1, y0, y0)
	VPXOR(y7, y0, y0)
	VPXOR(y5, y0, y0)
	VPXOR(y3, y0, y0)
	VPXOR(buffer.Offset(27*32), y0, y0)
	VMOVDQU(y0, buffer.Offset(27*32))

	Comment("11=11^3^9^17^25")
	VPXOR(b.Offset(11*32), y9, y4)
	VPXOR(y1, y4, y4)
	VPXOR(y2, y4, y4)
	VPXOR(y3, y4, y4)
	VPXOR(buffer.Offset(11*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(11*32))

	VMOVDQU(b.Offset(7*32), y0)
	VMOVDQU(b.Offset(15*32), y5)
	Comment("17=17^9^31^7^15")
	VPXOR(y2, y1, y1)
	VPXOR(y8, y1, y1)
	VPXOR(y0, y1, y1)
	VPXOR(y5, y1, y1)
	VPXOR(buffer.Offset(17*32), y1, y1)
	VMOVDQU(y1, buffer.Offset(17*32))

	Comment("25=25^17^7^15^23")
	VPXOR(y3, y2, y2)
	VPXOR(y0, y2, y2)
	VPXOR(y5, y2, y2)
	VPXOR(y6, y2, y2)
	VPXOR(buffer.Offset(25*32), y2, y2)
	VMOVDQU(y2, buffer.Offset(25*32))

	VMOVDQU(b.Offset(5*32), y1)
	VMOVDQU(b.Offset(11*32), y2)
	VMOVDQU(b.Offset(13*32), y3)
	// y0=7, y1=5, y9=y4=3, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31
	Comment("5=5^29^3^11^19")
	VPXOR(y1, y9, y4)
	VPXOR(y2, y4, y4)
	VPXOR(b.Offset(19*32), y4, y4)
	VPXOR(b.Offset(29*32), y4, y4)
	VPXOR(buffer.Offset(5*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(5*32))

	Comment("21=21^13^19^27^3")
	VPXOR(b.Offset(21*32), y9, y4)
	VPXOR(y3, y4, y4)
	VPXOR(b.Offset(19*32), y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(21*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(21*32))

	Comment("29=29^21^27^3^11")
	VPXOR(b.Offset(29*32), y9, y9)
	VPXOR(b.Offset(21*32), y9, y9)
	VPXOR(y7, y9, y9)
	VPXOR(y2, y9, y9)
	VPXOR(buffer.Offset(29*32), y9, y9)
	VMOVDQU(y9, buffer.Offset(29*32))

	VMOVDQU(b.Offset(21*32), y9)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31

	Comment("7=7^31^5^13^21")
	VPXOR(y0, y1, y4)
	VPXOR(y3, y4, y4)
	VPXOR(y8, y4, y4)
	VPXOR(y9, y4, y4)
	VPXOR(buffer.Offset(7*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(7*32))

	Comment("13=13^5^11^19^27")
	VPXOR(y3, y1, y4)
	VPXOR(y2, y4, y4)
	VPXOR(b.Offset(19*32), y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(13*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(13*32))

	VMOVDQU(b.Offset(29*32), y7)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=29, y8=31
	Comment("23=23^15^21^29^5")
	VPXOR(y5, y1, y4)
	VPXOR(y6, y4, y4)
	VPXOR(y9, y4, y4)
	VPXOR(y7, y4, y4)
	VPXOR(buffer.Offset(23*32), y4, y4)
	VMOVDQU(y4, buffer.Offset(23*32))

	Comment("31=31^23^29^5^13")
	VPXOR(y8, y1, y1)
	VPXOR(y6, y1, y1)
	VPXOR(y3, y1, y1)
	VPXOR(y7, y1, y1)
	VPXOR(buffer.Offset(31*32), y1, y1)
	VMOVDQU(y1, buffer.Offset(31*32))

	Comment("15=15^7^13^21^29")
	VPXOR(y3, y0, y0)
	VPXOR(y7, y0, y0)
	VPXOR(y9, y0, y0)
	VPXOR(y5, y0, y0)
	VPXOR(buffer.Offset(15*32), y0, y0)
	VMOVDQU(y0, buffer.Offset(15*32))

	VZEROUPPER()
	RET()
}

func sbox256avx2() {
	// sbox256avx2 function
	TEXT("sbox256avx2", NOSPLIT, "func(x, buffer *byte)")
	Doc("sbox256avx2, 256 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	Comment("f, for not operation")
	f := YMM()
	VPCMPEQB(f, f, f)

	Comment("Start input function")
	Comment("t1=b7 ^ b5")
	t1 := YMM()
	VMOVDQU(b.Offset(7*32), t1)
	VPXOR(b.Offset(5*32), t1, t1)

	t2, t7, t8 := YMM(), YMM(), YMM()
	VMOVDQU(b.Offset(1*32), t8)
	Comment("store m6=b1")
	VMOVDQU(t8, buffer.Offset((8+6)*32)) // m6
	Comment("t2=b5 ^ b1")
	VPXOR(b.Offset(5*32), t8, t2)
	VPANDN(f, t2, t2)

	t3, t4 := YMM(), YMM()

	VMOVDQU(b, t4)
	Comment("t3=^(b0 ^ t2)")
	VPXOR(t2, t4, t3)
	VPANDN(f, t3, t3)
	Comment("store g5=^b0")
	VPANDN(f, t4, t4)
	VMOVDQU(t4, buffer.Offset(5*32)) // g5

	Comment("t4=b6 ^ b2")
	t12 := YMM()
	VMOVDQU(b.Offset(6*32), t12)
	VPXOR(b.Offset(2*32), t12, t4)

	Comment("t5=b3 ^ t3")
	t5, t11 := YMM(), YMM()
	VMOVDQU(b.Offset(3*32), t11)
	VPXOR(t3, t11, t5)

	Comment("t6=b4 ^ t1")
	t6 := YMM()
	VPXOR(b.Offset(4*32), t1, t6)

	Comment("t7=b1 ^ t5")
	VPXOR(t5, t8, t7)
	Comment("t8=b1 ^ t4")
	VPXOR(t4, t8, t8)

	Comment("t9=t6 ^ t8")
	t9 := YMM()
	VPXOR(t8, t6, t9)
	Comment("store m8")
	VMOVDQU(t9, buffer.Offset((8+8)*32)) // m8
	Comment("store g1")
	VMOVDQU(t7, buffer.Offset(1*32)) // g1
	Comment("store g3")
	VMOVDQU(t5, buffer.Offset(3*32)) // g3
	Comment("store g4")
	VMOVDQU(t2, buffer.Offset(4*32)) // g4
	Comment("store m0")
	VMOVDQU(t6, buffer.Offset((8+0)*32)) // m0
	Comment("store m1")
	VMOVDQU(t3, buffer.Offset((8+1)*32)) // m1
	Comment("store m2")
	VMOVDQU(t8, buffer.Offset((8+2)*32)) // m2
	Comment("store m4")
	VMOVDQU(t4, buffer.Offset((8+4)*32)) // m4

	Comment("t11=^(b3 ^ t1)")
	VPXOR(t1, t11, t11)
	VPANDN(f, t11, t11)
	Comment("store m5, can reuse t1 now")
	VMOVDQU(t11, buffer.Offset((8+5)*32)) // m5

	Comment("t12=^(b6 ^ t9)")
	VPXOR(t9, t12, t12)
	VPANDN(f, t12, t12)
	Comment("store m9, can reuse t7 t8 t9 now")
	VMOVDQU(t12, buffer.Offset((8+9)*32)) // m9

	Comment("t10=t6 ^ t7")
	t10 := t7
	VPXOR(t6, t10, t10)
	Comment("store g0, can reuse t6 now")
	VMOVDQU(t10, buffer) // g0

	Comment("t13=t4 ^ t10")
	t13 := t10
	VPXOR(t4, t13, t13)
	Comment("store g2, can reuse t4 now")
	VMOVDQU(t13, buffer.Offset(2*32)) // g2

	Comment("t14=t2 ^ t11")
	t14 := t1
	VPXOR(t2, t11, t14)
	Comment("store g6, can reuse t2 now")
	VMOVDQU(t14, buffer.Offset(6*32)) // g6

	Comment("t15=t12^t14")
	t15 := t14
	VPXOR(t12, t15, t15)
	Comment("store g7")
	VMOVDQU(t15, buffer.Offset(7*32)) // g7

	Comment("t16=t3 ^ t12")
	t16 := t12
	VPXOR(t3, t16, t16)
	Comment("store m3")
	VMOVDQU(t16, buffer.Offset((8+3)*32)) // m3

	Comment("t17=t11 ^ t16")
	t17 := t16
	VPXOR(t11, t17, t17)
	Comment("store m7")
	VMOVDQU(t17, buffer.Offset((8+7)*32)) // m7

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
	Comment("t2= (m0 & m1)")
	VPAND(t6, t3, t3) // t2

	Comment("t3= (g0 & g4)")
	VPAND(buffer, t2, t2) // t3

	Comment("t4= (g3 & g7)")
	VPAND(t5, t1, t6) // [t4] := t6

	Comment("t7= (g3 | g7)")
	VPOR(t1, t5, t5) // [t7] := t5

	Comment("t11= (m4 & m5)")
	VPAND(t4, t11, t11)

	VMOVDQU(buffer.Offset((8+3)*32), t4) // t4 = m3
	Comment("t10= ( m3 & m2 )")
	VPAND(t8, t4, t1) // [t10] := t1
	Comment("t12= ( m3 | m2 )")
	VPOR(t8, t4, t4) //[t12] := t4

	Comment("t6= ( g6 | g2 )")
	VPOR(buffer.Offset(6*32), t7, t7) // [t6] := t7

	Comment("t9= ( m6 | m7 )")
	VPOR(buffer.Offset((8+6)*32), t12, t12) // [t9] := t12

	t10 = YMM()
	VMOVDQU(buffer.Offset((8+9)*32), t10) // t10 = m9

	Comment("t5= ( m8 & m9 )")
	VPAND(t9, t10, t8) // [t5] := t8
	Comment("t8= ( m8 | m9 )")
	VPOR(t9, t10, t10) // [t8] := t10

	Comment("t14 = t3 ^ t2")
	VPXOR(t3, t2, t2) // t14 = t3 ^ t2
	Comment("t16 = t5 ^ t14")
	VPXOR(t2, t8, t8) // t16 = t5 ^ t14, can reuse t2 now
	Comment("t20 = t16 ^ t7")
	VPXOR(t8, t5, t5) // t20 = t16 ^ t7
	Comment("t17 = t9 ^ t10")
	VPXOR(t12, t1, t1) // t17 = t9 ^ t10
	Comment("t18 = t11 ^ t12")
	VPXOR(t11, t4, t4) // t18 = t11 ^ t12
	Comment("p2 = t20 ^ t18")
	VPXOR(t5, t4, t4) // p2 = t20 ^ t18, can reuse t5 now
	Comment("p0 = t6 ^ t16")
	VPXOR(t7, t8, t8) // p0 = t6 ^ t16
	Comment("t1 = (g5 & g1)")
	VMOVDQU(buffer.Offset(1*32), t2)
	VMOVDQU(buffer.Offset(5*32), t5)
	VPAND(t2, t5, t5)
	Comment("t13 = t1 ^ t2")
	VPXOR(t5, t3, t3) // t13 = t1 ^ t2
	Comment("t15 = t13 ^ t4")
	VPXOR(t6, t3, t3) // t15 = t4 ^ t13
	Comment("t19 = t6 ^ t15")
	VPXOR(t3, t7, t7) // t19 = t6 ^ t15
	Comment("p3 = t19 ^ t17")
	VPXOR(t1, t7, t7) // p3 = t19 ^ t17
	Comment("p1 = t8 ^ t15")
	VPXOR(t10, t3, t3) // p1 = t8 ^ t15

	Comment("start middle function")
	Comment("current register status: t8=p0, t3=p1, t4=p2, t7=p0")

	// t3 = p1
	// t4 = p2
	// t7 = p3
	// t8 = p0
	Comment("t0 = (p1 & p2)")
	VPAND(t3, t4, t1)
	Comment("t1 = (p3 & p0)")
	VPAND(t7, t8, t2)
	Comment("t2 = (p0 & p2)")
	VPAND(t4, t8, t5)
	Comment("t3 = (p1 & p3)")
	VPAND(t3, t7, t6)
	Comment("t4 = (t0 & t2)")
	VPAND(t1, t5, t9)
	Comment("t5 = (t1 ^ t3)")
	VPXOR(t2, t6, t10)
	Comment("t6 = (t5 | p0)")
	VPOR(t10, t8, t8)
	Comment("t7 = (t2 | p3)")
	VPOR(t5, t7, t7)
	Comment("t8 = (t4 ^ t6)")
	VPXOR(t9, t8, t8) // l3
	Comment("t9 = (t7 ^ t3)")
	VPXOR(t6, t7, t9)
	Comment("t10 = (t0 ^ t9)")
	VPXOR(t1, t9, t1) // l0
	Comment("t11 = (t5 | p2)")
	VPOR(t4, t10, t4)
	Comment("l1 = t11 ^ t1")
	VPXOR(t2, t4, t2) // l1
	Comment("t12 = (t2 | p1)")
	VPOR(t5, t3, t3)
	Comment("l2 = t12 ^ t5")
	VPXOR(t3, t10, t3) // l2

	Comment("start bottom function")
	Comment("current register status: t1=l0, t2=l1, t3=l2, t8=l3")
	Comment("k4 = l2 ^ l3")
	VPXOR(t3, t8, t7) // k4 = l2 ^ l3
	Comment("k3 = l1 ^ l3")
	VPXOR(t8, t2, t6) // k3 = l1 ^ l3
	Comment("k2 = l0 ^ l2")
	VPXOR(t1, t3, t5) // k2 = l0 ^ l2
	Comment("k0 = l0 ^ l1")
	VPXOR(t1, t2, t4) // k0 = l0 ^ l1
	Comment("k1 = k2 ^ k3")
	VPXOR(t5, t6, t9) // k1 = k2 ^ k3

	Comment("e0= (m1 & k0)")
	VMOVDQU(buffer.Offset((8+1)*32), t10) // m1
	VPAND(t4, t10, t10)                   // e0
	Comment("e1= (g5 & l1)")
	VMOVDQU(buffer.Offset(5*32), t11)
	VPAND(t2, t11, t11) // e1
	Comment("r0=e0 ^ e1")
	VPXOR(t11, t10, t10) // r0 = e0 ^ e1
	Comment("e2=(g4 & l0)")
	VMOVDQU(buffer.Offset(4*32), t12)
	VPAND(t1, t12, t12) // e2
	Comment("r1=e2 ^ e1")
	VPXOR(t12, t11, t11) // r1 = e2 ^ e1
	Comment("store r0 r1")
	VMOVDQU(t10, buffer.Offset(22*32)) // in fact, we can start from 18*32
	VMOVDQU(t11, buffer.Offset(23*32))

	Comment("e3= (m7 & k3)")
	VMOVDQU(buffer.Offset((8+7)*32), t10) // m7
	VPAND(t6, t10, t10)                   // e3
	Comment("e4= (m5 & k2)")
	VMOVDQU(buffer.Offset((8+5)*32), t11) // m5
	VPAND(t5, t11, t11)
	Comment("r2=e3 ^ e4")
	VPXOR(t11, t10, t10) // r2 = e3 ^ e4
	Comment("e5= (m3 & k1)")
	VMOVDQU(buffer.Offset((8+3)*32), t12) // m3
	VPAND(t9, t12, t12)
	Comment("r3=e5 ^ e4")
	VPXOR(t12, t11, t11) // r3 = e5 ^ e4
	Comment("store r2 r3")
	VMOVDQU(t10, buffer.Offset(24*32))
	VMOVDQU(t11, buffer.Offset(25*32))

	Comment("e6=(m9 & k4)")
	VMOVDQU(buffer.Offset((8+9)*32), t10) // m9
	VPAND(t7, t10, t10)
	Comment("e7=(g7 & l3)")
	VMOVDQU(buffer.Offset(7*32), t11)
	VPAND(t8, t11, t11)
	Comment("r4=e7 ^ e6")
	VPXOR(t11, t10, t10) // r4 = e6 ^ e7
	Comment("e8=(g6 & l2)")
	VMOVDQU(buffer.Offset(6*32), t12)
	VPAND(t3, t12, t12) // e8
	Comment("r5=e8 ^ e6")
	VPXOR(t12, t11, t12) // r5 = e8 ^ e7 = t12
	Comment("store r4")
	VMOVDQU(t10, buffer.Offset(26*32))

	Comment("e9=(m0 & k0)")
	VMOVDQU(buffer.Offset((8+0)*32), t10) // m0
	VPAND(t4, t10, t10)
	Comment("e10=(g1 & l1)")
	VMOVDQU(buffer.Offset(1*32), t4)
	VPAND(t4, t2, t2) // e10
	Comment("r6=e9 ^ e10")
	VPXOR(t10, t2, t10) // r6 = e9 ^ e10 = t10
	Comment("e11=(g0 & l0)")
	VMOVDQU(buffer, t11)
	VPAND(t1, t11, t11)
	Comment("r7=e11 ^ e10")
	VPXOR(t11, t2, t1) // r7 = e11 ^ e10 = t1

	Comment("e12=(m6 & k3)")
	VMOVDQU(buffer.Offset((8+6)*32), t2) // m6
	VPAND(t2, t6, t2)                    // e12
	Comment("e13=(m4 & k2)")
	VMOVDQU(buffer.Offset((8+4)*32), t6) // m4
	VPAND(t6, t5, t5)                    // e13
	Comment("r8=e12 ^ e13")
	VPXOR(t2, t5, t2) // r8 = e12 ^ e13 = t2
	Comment("e14=(m2 & k1)")
	VMOVDQU(buffer.Offset((8+2)*32), t6) // m2
	VPAND(t6, t9, t6)
	Comment("r9=e14 ^ e13")
	VPXOR(t5, t6, t4) // r9 = e14 ^ e13  = t4

	Comment("e15=(m8 & k4)")
	VMOVDQU(buffer.Offset((8+8)*32), t9) // m8
	VPAND(t9, t7, t9)                    // e15
	Comment("e16=(g3 & l3)")
	VMOVDQU(buffer.Offset(3*32), t7)
	VPAND(t7, t8, t8) // e16
	Comment("r10=e15 ^ e16")
	VPXOR(t9, t8, t5) // r10 = e15 ^ e16 = t5
	Comment("e17=(g2 & l2)")
	VMOVDQU(buffer.Offset(2*32), t9)
	VPAND(t3, t9, t3) // e17
	Comment("r11=e17 ^ e16")
	VPXOR(t3, t8, t3) // r11 = e17 ^ e16 = t3

	Comment("start output function")
	// t12 = r5
	// t10 = r6
	// t1 = r7
	// t2 = r8
	// t4 = r9
	// t5 = r10
	// t3 = r11
	Comment("[t1]=r7 ^ r9")
	VPXOR(t1, t4, t4) // [t1] = t4 = r7 ^ r9

	Comment("[t2]=t1 ^ r1")
	VMOVDQU(buffer.Offset((22+1)*32), t6) // r1
	VPXOR(t4, t6, t6)                     // [t2] = t6 = r1 ^ [t1]

	Comment("[t3]=t2 ^ r3")
	VMOVDQU(buffer.Offset((22+3)*32), t8) // r3
	VPXOR(t6, t8, t7)                     // [t3] = t7 = r3 ^ [t2]
	Comment("[t4]=r5 ^ r3")
	VPXOR(t12, t8, t8) // [t4] = t8 = r5 ^ r3

	Comment("[t5]=r4 ^ [t4]")
	VMOVDQU(buffer.Offset((22+4)*32), t11) // r4
	VPXOR(t8, t11, t9)                     // [t5] = t9 = r4 ^ t4
	Comment("[t6]=r0 ^ r4")
	VPXOR(buffer.Offset(22*32), t11, t11) // [t6] = t11 = r4 ^ r0

	Comment("[t7]=r11 ^ r7")
	VPXOR(t3, t1, t1) // [t7] t1 = r7 ^ r11

	Comment("[t8]=[t1] ^ [t4]")
	VPXOR(t4, t8, t8) // t8 = t4 ^ t11
	Comment("store t8")
	VMOVDQU(t8, b.Offset(5*32))

	Comment("[t9]=[t1] ^ [t6]")
	VPXOR(t11, t4, t4) // [t9] = t4
	Comment("store t9")
	VMOVDQU(t4, b.Offset(2*32))

	Comment("[t10]=r2 ^ t5")
	VPXOR(buffer.Offset((22+2)*32), t9, t9) // [t10] t9 = r2 ^ [t5]
	Comment("[t11]=r10 ^ r8")
	VPXOR(t5, t2, t2) // [t11] = t2
	Comment("store t11")
	VMOVDQU(t2, b.Offset(3*32))
	Comment("[t12]=^([t3] ^ [t11])")
	VPXOR(t7, t2, t2)
	VPANDN(f, t2, t2) // [t12] = t2
	Comment("store t12")
	VMOVDQU(t2, b.Offset(1*32))
	Comment("[t13]=[t10] ^ [t12]")
	VPXOR(t2, t9, t9) // [t13] = t9
	Comment("store t13")
	VMOVDQU(t9, b.Offset(6*32))

	Comment("[t14]=^([t3] ^ [t7])")
	VPXOR(t7, t1, t1)
	VPANDN(f, t1, t1) // [t14]
	Comment("store t14")
	VMOVDQU(t1, b.Offset(4*32))
	Comment("[t16]=[t6] ^ [t14]")
	VPXOR(t11, t1, t1) // [t16]
	Comment("store t16")
	VMOVDQU(t1, b)

	Comment("[t15]=^(r10 ^ r6)")
	VPXOR(t10, t5, t5)
	VPANDN(f, t5, t5)
	Comment("store t15")
	VMOVDQU(t5, b.Offset(7*32))

	VZEROUPPER()
	RET()
}

func xorRoundKey256avx2() {
	// xorRoundKey256avx2 function
	TEXT("xorRoundKey256avx2", NOSPLIT, "func(rk uint32, x1, x2, x3, out *byte)")
	Doc("xor x1, x2, x3 with round key, 32 bytes per bit")

	x := Load(Param("rk"), GP32())
	x1 := Mem{Base: Load(Param("x1"), GP64())}
	x2 := Mem{Base: Load(Param("x2"), GP64())}
	x3 := Mem{Base: Load(Param("x3"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	tmp1, tmp2, tmp3, tmp := XMM(), XMM(), XMM(), XMM()
	ret := YMM()
	one := YMM()

	y := GP32()

	count := GP64()
	XORQ(count, count)
	MOVQ(x, tmp2)

	Comment("Handle first byte")
	MOVL(U32(0x01000000), y)
	MOVQ(y, tmp1)
	VMOVDQU(tmp1, tmp3)
	Label("rk_loop_1")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(32), count)
	CMPQ(count, U32(256))
	JL(LabelRef("rk_loop_1"))

	Comment("Handle second byte")
	VPSRLD(Imm(8), tmp3, tmp1)
	Label("rk_loop_2")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(32), count)
	CMPQ(count, U32(512))
	JL(LabelRef("rk_loop_2"))

	Comment("Handle third byte")
	VPSRLD(Imm(16), tmp3, tmp1)
	Label("rk_loop_3")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(32), count)
	CMPQ(count, U32(768))
	JL(LabelRef("rk_loop_3"))

	Comment("Handle last byte")
	VPSRLD(Imm(24), tmp3, tmp1)
	Label("rk_loop_4")
	VMOVDQU(x1.Idx(count, 1), ret)
	VPXOR(x2.Idx(count, 1), ret, ret)
	VPXOR(x3.Idx(count, 1), ret, ret)
	VPAND(tmp1, tmp2, tmp)
	VPCMPEQD(tmp1, tmp, tmp)
	VPBROADCASTD(tmp, one)
	VPXOR(one, ret, ret)
	VMOVDQU(ret, out.Idx(count, 1))
	VPSLLD(Imm(1), tmp1, tmp1)
	ADDQ(U8(32), count)
	CMPQ(count, U32(1024))
	JL(LabelRef("rk_loop_4"))

	VZEROUPPER()
	RET()
}

func sbox64() {
	// sbox64 function
	TEXT("sbox64", NOSPLIT, "func(x, buffer *byte)")
	Doc("sbox64, 64 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	Comment("Start input function")
	Comment("t1=b7 ^ b5")
	t1 := GP64()
	MOVQ(b.Offset(7*8), t1)
	XORQ(b.Offset(5*8), t1)

	t2, t7, t8 := GP64(), GP64(), GP64()
	MOVQ(b.Offset(1*8), t2)
	MOVQ(t2, t7)
	MOVQ(t2, t8)
	Comment("store m6=b1")
	MOVQ(t2, buffer.Offset((8+6)*8)) // m6
	Comment("t2=b5 ^ b1")
	XORQ(b.Offset(5*8), t2)
	NOTQ(t2)

	t3, t4 := GP64(), GP64()
	Comment("store g5=^b0")
	MOVQ(b, t3)
	MOVQ(t3, t4)
	NOTQ(t4)
	MOVQ(t4, buffer.Offset(5*8)) // g5
	Comment("t3=^(b0 ^ t2)")
	XORQ(t2, t3)
	NOTQ(t3)

	Comment("t4=b6 ^ b2")
	t12 := GP64()
	MOVQ(b.Offset(6*8), t4)
	MOVQ(t4, t12)
	XORQ(b.Offset(2*8), t4)

	Comment("t5=b3 ^ t3")
	t5, t11 := GP64(), GP64()
	MOVQ(b.Offset(3*8), t5)
	MOVQ(t5, t11)
	XORQ(t3, t5)

	Comment("t6=b4 ^ t1")
	t6 := GP64()
	MOVQ(b.Offset(4*8), t6)
	XORQ(t1, t6)

	Comment("t7=b1 ^ t5")
	XORQ(t5, t7)
	Comment("t8=b1 ^ t4")
	XORQ(t4, t8)

	Comment("t9=t6 ^ t8")
	t9 := GP64()
	MOVQ(t6, t9)
	XORQ(t8, t9)
	Comment("store m8")
	MOVQ(t9, buffer.Offset((8+8)*8)) // m8
	Comment("store g1")
	MOVQ(t7, buffer.Offset(1*8)) // g1
	Comment("store g3")
	MOVQ(t5, buffer.Offset(3*8)) // g3
	Comment("store g4")
	MOVQ(t2, buffer.Offset(4*8)) // g4
	Comment("store m0")
	MOVQ(t6, buffer.Offset((8+0)*8)) // m0
	Comment("store m1")
	MOVQ(t3, buffer.Offset((8+1)*8)) // m1
	Comment("store m2")
	MOVQ(t8, buffer.Offset((8+2)*8)) // m2
	Comment("store m4")
	MOVQ(t4, buffer.Offset((8+4)*8)) // m4

	Comment("t11=^(b3 ^ t1)")
	XORQ(t1, t11)
	NOTQ(t11)
	Comment("store m5, can reuse t1 now")
	MOVQ(t11, buffer.Offset((8+5)*8)) // m5

	Comment("t12=^(b6 ^ t9)")
	XORQ(t9, t12)
	NOTQ(t12)
	Comment("store m9, can reuse t7 t8 t9 now")
	MOVQ(t12, buffer.Offset((8+9)*8)) // m9

	Comment("t10=t6 ^ t7")
	t10 := t7
	XORQ(t6, t10)
	Comment("store g0, can reuse t6 now")
	MOVQ(t10, buffer) // g0

	Comment("t13=t4 ^ t10")
	t13 := t10
	XORQ(t4, t13)
	Comment("store g2, can reuse t4 now")
	MOVQ(t13, buffer.Offset(2*8)) // g2

	Comment("t14=t2 ^ t11")
	t14 := t1
	MOVQ(t11, t14)
	XORQ(t2, t14)
	Comment("store g6, can reuse t2 now")
	MOVQ(t14, buffer.Offset(6*8)) // g6

	Comment("t15=t12^t14")
	t15 := t14
	XORQ(t12, t15)
	Comment("store g7")
	MOVQ(t15, buffer.Offset(7*8)) // g7

	Comment("t16=t3 ^ t12")
	t16 := t12
	XORQ(t3, t16)
	Comment("store m3")
	MOVQ(t16, buffer.Offset((8+3)*8)) // m3

	Comment("t17=t11 ^ t16")
	t17 := t16
	XORQ(t11, t17)
	Comment("store m7")
	MOVQ(t17, buffer.Offset((8+7)*8)) // m7

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
	Comment("t2=m0 & m1")
	ANDQ(t6, t3) // t2 := t3

	Comment("t3=g0 & g4")
	ANDQ(buffer, t2) // t3 := t2

	Comment("t4=g3 & g7")
	MOVQ(t1, t6)
	ANDQ(t5, t1) // t4 := t1

	Comment("t7=g3 | g7")
	ORQ(t6, t5) // t7 := t5

	Comment("t11=m4 & m5")
	ANDQ(t4, t11) // t11

	MOVQ(buffer.Offset((8+3)*8), t4) // t4 = m3
	MOVQ(t4, t6)
	Comment("t10=m3 & m2")
	ANDQ(t8, t6) // t10 := t6
	Comment("t12=m3 | m2")
	ORQ(t8, t4) // t12 := t4

	Comment("t6=g6 | g2")
	ORQ(buffer.Offset(6*8), t7) // t6 := t7

	Comment("t9=m6 | m7")
	ORQ(buffer.Offset((8+6)*8), t12) // t9 := t12

	t10 = GP64()
	MOVQ(buffer.Offset((8+9)*8), t8) // t8 = m9
	MOVQ(t8, t10)

	Comment("t5=m8 & m9")
	ANDQ(t9, t8) // t5 := t8
	Comment("t8=m8 | m9")
	ORQ(t9, t10) // t8 := t10

	Comment("t14 = t3 ^ t2")
	XORQ(t3, t2) // t14 = t3 ^ t2
	Comment("t16 = t5 ^ t14")
	XORQ(t2, t8) // t16 = t5 ^ t14, can reuse t2 now
	Comment("t20 = t16 ^ t7")
	XORQ(t8, t5) // t20 = t16 ^ t7
	Comment("t17 = t9 ^ t10")
	XORQ(t12, t6) // t17 = t9 ^ t10
	Comment("t18 = t11 ^ t12")
	XORQ(t11, t4) // t18 = t11 ^ t12
	Comment("p2 = t20 ^ t18")
	XORQ(t5, t4) // p2 = t20 ^ t18, can reuse t5 now
	Comment("p0 = t6 ^ t16")
	XORQ(t7, t8) // p0 = t6 ^ t16
	Comment("t1 = g5 & g1")
	MOVQ(buffer.Offset(1*8), t2)
	MOVQ(buffer.Offset(5*8), t5)
	ANDQ(t2, t5) // t1 := t5
	Comment("t13 = t1 ^ t2")
	XORQ(t5, t3) // t13 = t1 ^ t2
	Comment("t15 = t13 ^ t4")
	XORQ(t1, t3) // t15 = t4 ^ t13
	Comment("t19 = t6 ^ t15")
	XORQ(t3, t7) // t19 = t6 ^ t15
	Comment("p3 = t19 ^ t17")
	XORQ(t6, t7) // p3 = t19 ^ t17
	Comment("p1 = t8 ^ t15")
	XORQ(t10, t3) // p1 = t8 ^ t15

	Comment("start middle function")
	Comment("current register status: t8=p0, t3=p1, t4=p2, t7=p0")

	// t3 = p1
	// t4 = p2
	// t7 = p3
	// t8 = p0
	Comment("t0 = p1 & p2")
	MOVQ(t3, t1)
	ANDQ(t4, t1) // t0 := t1

	Comment("t1 = p3 & p0")
	MOVQ(t8, t2)
	ANDQ(t7, t2) // t1 := t2

	Comment("t2 = p0 & p2")
	MOVQ(t4, t5)
	ANDQ(t8, t5) // t2 := t5

	Comment("t3 = p1 & p3")
	MOVQ(t3, t6)
	ANDQ(t7, t6) // t3 := t6

	Comment("t4 = t0 & t2")
	MOVQ(t1, t9)
	ANDQ(t5, t9) // t4 := t9

	Comment("t5 = t1 & t3")
	MOVQ(t2, t10)
	XORQ(t6, t10) // t5 := t10

	Comment("t6 = t5 | p0")
	ORQ(t10, t8) // t6 := t8

	Comment("t7 = t2 | p3")
	ORQ(t5, t7) // t7

	Comment("t8 = t4 ^ t6")
	XORQ(t9, t8) // l3 = t8

	Comment("t9 = t7 ^ t3")
	XORQ(t7, t6) // t9 := t6

	Comment("t10 = t0 ^ t9")
	XORQ(t1, t6) // l0 = t10 := t6

	Comment("t11 = p2 | t5")
	ORQ(t10, t4) // t11 := t4
	Comment("l1 = t11 ^ t1")
	XORQ(t4, t2) // l1 := t2

	Comment("t12 = p1 | t2")
	ORQ(t5, t3) // t12 := t3
	Comment("l2 = t12 ^ t5")
	XORQ(t10, t3) // l2 := t3

	Comment("start bottom function")
	Comment("current register status: t6=l0, t2=l1, t3=l2, t8=l3")
	Comment("k4 = l2 ^ l3")
	MOVQ(t8, t5)
	XORQ(t3, t5) // k4 := t5

	Comment("k3 = l1 ^ l3")
	MOVQ(t8, t4)
	XORQ(t2, t4) // k3 := t4

	Comment("k2 = l0 ^ l2")
	MOVQ(t6, t7)
	XORQ(t3, t7) // k2 := t7

	Comment("k0 = l0 ^ l1")
	MOVQ(t6, t1)
	XORQ(t2, t1) // k0 := t1

	Comment("k1 = k2 ^ k3")
	MOVQ(t4, t9)
	XORQ(t7, t9) // k1 := t9

	Comment("e0=(m1 & k0)")
	MOVQ(buffer.Offset((8+1)*8), t10) // m1
	ANDQ(t1, t10)                     // e0 := t10

	Comment("e1=(g5 & l1)")
	MOVQ(buffer.Offset(5*8), t11)
	ANDQ(t2, t11) // e1 := t11

	Comment("r0=e0 ^ e1")
	XORQ(t11, t10) // r0 = e0 ^ e1

	Comment("e2=(g4 & l0)")
	MOVQ(buffer.Offset(4*8), t12)
	ANDQ(t6, t12)

	Comment("r1=e2 ^ e1")
	XORQ(t12, t11) // r1 = e2 ^ e1

	Comment("store r0 r1")
	MOVQ(t10, buffer.Offset(22*8)) // in fact, we can start from 18*16
	MOVQ(t11, buffer.Offset(23*8))

	Comment("e3=(m7 & k3)")
	MOVQ(buffer.Offset((8+7)*8), t10) // m7
	ANDQ(t4, t10)

	Comment("e4=(m5 & k2)")
	MOVQ(buffer.Offset((8+5)*8), t11) // m5
	ANDQ(t7, t11)
	Comment("r2=e3 ^ e4")
	XORQ(t11, t10) // r2 = e3 ^ e4

	Comment("e5=(m3 & k1)")
	MOVQ(buffer.Offset((8+3)*8), t12) // m3
	ANDQ(t9, t12)
	Comment("r3=e5 ^ e4")
	XORQ(t12, t11) // r3 = e5 ^ e4

	Comment("store r2 r3")
	MOVQ(t10, buffer.Offset(24*8))
	MOVQ(t11, buffer.Offset(25*8))

	Comment("e6=(m9 & k4)")
	MOVQ(buffer.Offset((8+9)*8), t10) // m9
	ANDQ(t5, t10)

	Comment("e7=(g7 & l3)")
	MOVQ(buffer.Offset(7*8), t11)
	ANDQ(t8, t11)
	Comment("r4=e7 ^ e6")
	XORQ(t11, t10) // r4 = e6 ^ e7

	Comment("e8=(g6 & l2)")
	MOVQ(buffer.Offset(6*8), t12)
	ANDQ(t3, t12)
	Comment("r5=e8 ^ e6")
	XORQ(t11, t12) // r5 = e8 ^ e7

	Comment("store r4")
	MOVQ(t10, buffer.Offset(26*8))

	Comment("e9=(m0 & k0)")
	MOVQ(buffer.Offset((8+0)*8), t10) // m0
	ANDQ(t1, t10)                     // e9 := t10

	Comment("e10=(g1 & l1)")
	MOVQ(buffer.Offset(1*8), t1)
	ANDQ(t2, t1) // e10 := t1

	Comment("r6=e9 ^ e10")
	XORQ(t1, t10) // r6 = e9 ^ e10

	Comment("e11=(g0 & l0)")
	MOVQ(buffer, t11)
	ANDQ(t11, t6) // e11 := t6
	Comment("r7=e11 ^ e10")
	XORQ(t6, t1) // r7 = e11 ^ e10 = t1

	Comment("e12=(m6 & k3)")
	MOVQ(buffer.Offset((8+6)*8), t2) // m6
	ANDQ(t4, t2)

	Comment("e13=(m4 & k2)")
	MOVQ(buffer.Offset((8+4)*8), t6) // m4
	ANDQ(t7, t6)

	Comment("r8=e12 ^ e13")
	XORQ(t6, t2) // r8 = e12 ^ e13 = t2

	Comment("e14=(m2 & k1)")
	MOVQ(buffer.Offset((8+2)*8), t4) // m2
	ANDQ(t9, t4)
	Comment("r9=e14 ^ e13")
	XORQ(t6, t4) // r9 = e14 ^ e13  = t4

	Comment("e15=(m8 & k4)")
	MOVQ(buffer.Offset((8+8)*8), t9) // m8
	ANDQ(t9, t5)

	Comment("e16=(g3 & l3)")
	MOVQ(buffer.Offset(3*8), t9)
	ANDQ(t9, t8)
	Comment("r10=e15 ^ e16")
	XORQ(t8, t5) // r10 = e15 ^ e16 = t5

	Comment("e17=(g2 & l2)")
	MOVQ(buffer.Offset(2*8), t11)
	ANDQ(t11, t3)
	Comment("r11=e17 ^ e16")
	XORQ(t8, t3) // r11 = e17 ^ e16 = t3

	Comment("start output function")
	// t12 = r5
	// t10 = r6
	// t1 = r7
	// t2 = r8
	// t4 = r9
	// t5 = r10
	// t3 = r11
	Comment("[t1]=r7 ^ r9")
	XORQ(t1, t4) // [t1] = t4 = r7 ^ r9

	Comment("[t2]=t1 ^ r1")
	MOVQ(buffer.Offset((22+1)*8), t6) // r1
	XORQ(t4, t6)                      // [t2] = t6 = r1 ^ [t1]

	Comment("[t3]=t2 ^ r3")
	MOVQ(buffer.Offset((22+3)*8), t7) // r3
	MOVQ(t7, t8)
	XORQ(t6, t7) // [t3] = t7 = r3 ^ [t2]
	Comment("[t4]=r5 ^ r3")
	XORQ(t12, t8) // [t4] = t8 = r5 ^ r3

	Comment("[t5]=r4 ^ [t4]")
	MOVQ(buffer.Offset((22+4)*8), t9) // r4
	MOVQ(t9, t11)
	XORQ(t8, t9) // [t5] = t9 = r4 ^ t4
	Comment("[t6]=r0 ^ r4")
	XORQ(buffer.Offset(22*8), t11) // [t6] = t11 = r4 ^ r0

	Comment("[t7]=r11 ^ r7")
	XORQ(t3, t1) // [t7] t1 = r7 ^ r11

	Comment("[t8]=[t1] ^ [t4]")
	XORQ(t4, t8) // t8 = t4 ^ t11
	Comment("store t8")
	MOVQ(t8, b.Offset(5*8))

	Comment("[t9]=[t1] ^ [t6]")
	XORQ(t11, t4) // [t9] = t4
	Comment("store t9")
	MOVQ(t4, b.Offset(2*8))

	Comment("[t10]=r2 ^ t5")
	XORQ(buffer.Offset((22+2)*8), t9) // [t10] t9 = r2 ^ [t5]
	Comment("[t11]=r10 ^ r8")
	XORQ(t5, t2) // [t11] = t2
	Comment("store t11")
	MOVQ(t2, b.Offset(3*8))
	Comment("[t12]=^([t3] ^ [t11])")
	XORQ(t7, t2)
	NOTQ(t2) // [t12] = t2
	Comment("store t12")
	MOVQ(t2, b.Offset(1*8))
	Comment("[t13]=[t10] ^ [t12]")
	XORQ(t2, t9) // [t13] = t9
	Comment("store t13")
	MOVQ(t9, b.Offset(6*8))

	Comment("[t14]=^([t3] ^ [t7])")
	XORQ(t7, t1)
	NOTQ(t1) // [t14]
	Comment("store t14")
	MOVQ(t1, b.Offset(4*8))
	Comment("[t16]=[t6] ^ [t14]")
	XORQ(t11, t1) // [t16]
	Comment("store t16")
	MOVQ(t1, b)

	Comment("[t15]=^(r10 ^ r6)")
	XORQ(t10, t5)
	NOTQ(t5)
	Comment("store t15")
	MOVQ(t5, b.Offset(7*8))

	RET()
}

// 0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23 | 24 25 26 27 28 29 30 31
// 24 25 26 27 28 29 30 31 |  0  1  2  3  4  5  6  7 |  8  9 10 11 12 13 14 15 | 16 17 18 19 20 21 22 23
// 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29
// 22 23  8  9 10 11 12 13 | 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5
// 30 31 16 17 18 19 20 21 |  6  7 24 25 26 27 28 29 | 14 15  0  1  2  3  4  5 | 22 23  8  9 10 11 12 13
func l64() {
	// l64 function
	TEXT("l64", NOSPLIT, "func(x, buffer *byte)")
	Doc("l64 and xor buffer, 64 bits per 'byte'")

	b := Mem{Base: Load(Param("x"), GP64())}
	buffer := Mem{Base: Load(Param("buffer"), GP64())}

	y0, y1, y2, y3, y4, y5, y6, y7, y8, y9 := GP64(), GP64(), GP64(), GP64(), GP64(), GP64(), GP64(), GP64(), GP64(), GP64()

	MOVQ(b, y0)
	MOVQ(b.Offset(8*8), y1)
	MOVQ(b.Offset(16*8), y2)
	MOVQ(b.Offset(24*8), y3)
	MOVQ(b.Offset(18*8), y5)
	MOVQ(b.Offset(22*8), y6)
	MOVQ(b.Offset(26*8), y7)
	MOVQ(b.Offset(30*8), y8)
	MOVQ(b.Offset(2*8), y9)

	Comment("0=0^24^14^22^30")
	MOVQ(y0, y4)
	XORQ(y3, y4)
	XORQ(b.Offset(14*8), y4)
	XORQ(y6, y4)
	XORQ(y8, y4)
	XORQ(buffer, y4)
	MOVQ(y4, buffer)

	Comment("2=0^2^26^8^16")
	MOVQ(y0, y4)
	XORQ(y9, y4)
	XORQ(y7, y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(buffer.Offset(2*8), y4)
	MOVQ(y4, buffer.Offset(2*8))

	Comment("8=0^8^22^30^6")
	MOVQ(y0, y4)
	XORQ(y1, y4)
	XORQ(y6, y4)
	XORQ(y8, y4)
	XORQ(b.Offset(6*8), y4)
	XORQ(buffer.Offset(8*8), y4)
	MOVQ(y4, buffer.Offset(8*8))

	Comment("18=0^18^10^16^24")
	MOVQ(y0, y4)
	XORQ(y5, y4)
	XORQ(b.Offset(10*8), y4)
	XORQ(y2, y4)
	XORQ(y3, y4)
	XORQ(buffer.Offset(18*8), y4)
	MOVQ(y4, buffer.Offset(18*8))

	Comment("26=0^26^18^24^8")
	XORQ(y1, y0)
	XORQ(y7, y0)
	XORQ(y5, y0)
	XORQ(y3, y0)
	XORQ(buffer.Offset(26*8), y0)
	MOVQ(y0, buffer.Offset(26*8))

	Comment("10=10^2^8^16^24")
	MOVQ(y9, y4)
	XORQ(b.Offset(10*8), y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(y3, y4)
	XORQ(buffer.Offset(10*8), y4)
	MOVQ(y4, buffer.Offset(10*8))

	MOVQ(b.Offset(6*8), y0)
	MOVQ(b.Offset(14*8), y5)
	Comment("16=16^8^30^6^14")
	XORQ(y2, y1)
	XORQ(y8, y1)
	XORQ(y0, y1)
	XORQ(y5, y1)
	XORQ(buffer.Offset(16*8), y1)
	MOVQ(y1, buffer.Offset(16*8))

	Comment("24=24^16^6^14^22")
	XORQ(y3, y2)
	XORQ(y0, y2)
	XORQ(y5, y2)
	XORQ(y6, y2)
	XORQ(buffer.Offset(24*8), y2)
	MOVQ(y2, buffer.Offset(24*8))

	MOVQ(b.Offset(4*8), y1)
	MOVQ(b.Offset(10*8), y2)
	MOVQ(b.Offset(12*8), y3)
	// y0=6, y1=4, y9=y4=2, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30
	Comment("4=4^28^2^10^18")
	MOVQ(y9, y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(b.Offset(18*8), y4)
	XORQ(b.Offset(28*8), y4)
	XORQ(buffer.Offset(4*8), y4)
	MOVQ(y4, buffer.Offset(4*8))

	Comment("20=20^12^18^26^2")
	MOVQ(y9, y4)
	XORQ(b.Offset(20*8), y4)
	XORQ(y3, y4)
	XORQ(b.Offset(18*8), y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(20*8), y4)
	MOVQ(y4, buffer.Offset(20*8))

	Comment("28=28^20^26^2^10")
	XORQ(b.Offset(28*8), y9)
	XORQ(b.Offset(20*8), y9)
	XORQ(y7, y9)
	XORQ(y2, y9)
	XORQ(buffer.Offset(28*8), y9)
	MOVQ(y9, buffer.Offset(28*8))

	MOVQ(b.Offset(20*8), y9)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=26, y8=30

	Comment("6=6^30^4^12^20")
	MOVQ(y1, y4)
	XORQ(y0, y4)
	XORQ(y3, y4)
	XORQ(y8, y4)
	XORQ(y9, y4)
	XORQ(buffer.Offset(6*8), y4)
	MOVQ(y4, buffer.Offset(6*8))

	Comment("12=12^4^10^18^26")
	MOVQ(y1, y4)
	XORQ(y3, y4)
	XORQ(y2, y4)
	XORQ(b.Offset(18*8), y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(12*8), y4)
	MOVQ(y4, buffer.Offset(12*8))

	MOVQ(b.Offset(28*8), y7)
	// y0=6, y1=4, y9=20, y2=10, y3=12, y5=14, y6=22, y7=28, y8=30
	Comment("22=22^14^20^28^4")
	MOVQ(y1, y4)
	XORQ(y5, y4)
	XORQ(y6, y4)
	XORQ(y9, y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(22*8), y4)
	MOVQ(y4, buffer.Offset(22*8))

	Comment("30=30^22^28^4^12")
	XORQ(y8, y1)
	XORQ(y6, y1)
	XORQ(y3, y1)
	XORQ(y7, y1)
	XORQ(buffer.Offset(30*8), y1)
	MOVQ(y1, buffer.Offset(30*8))

	Comment("14=14^6^12^20^28")
	XORQ(y3, y0)
	XORQ(y7, y0)
	XORQ(y9, y0)
	XORQ(y5, y0)
	XORQ(buffer.Offset(14*8), y0)
	MOVQ(y0, buffer.Offset(14*8))

	MOVQ(b.Offset(1*8), y0)
	MOVQ(b.Offset(9*8), y1)
	MOVQ(b.Offset(17*8), y2)
	MOVQ(b.Offset(25*8), y3)
	MOVQ(b.Offset(19*8), y5)
	MOVQ(b.Offset(23*8), y6)
	MOVQ(b.Offset(27*8), y7)
	MOVQ(b.Offset(31*8), y8)
	MOVQ(b.Offset(3*8), y9)

	Comment("1=1^25^15^23^31")
	MOVQ(y0, y4)
	XORQ(y3, y4)
	XORQ(b.Offset(15*8), y4)
	XORQ(y6, y4)
	XORQ(y8, y4)
	XORQ(buffer.Offset(1*8), y4)
	MOVQ(y4, buffer.Offset(1*8))

	Comment("3=3^27^1^9^17")
	MOVQ(y0, y4)
	XORQ(y9, y4)
	XORQ(y7, y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(buffer.Offset(3*8), y4)
	MOVQ(y4, buffer.Offset(3*8))

	Comment("9=9^1^23^31^7")
	MOVQ(y0, y4)
	XORQ(y1, y4)
	XORQ(y6, y4)
	XORQ(y8, y4)
	XORQ(b.Offset(7*8), y4)
	XORQ(buffer.Offset(9*8), y4)
	MOVQ(y4, buffer.Offset(9*8))

	Comment("19=1^19^11^17^25")
	MOVQ(y0, y4)
	XORQ(y5, y4)
	XORQ(b.Offset(11*8), y4)
	XORQ(y2, y4)
	XORQ(y3, y4)
	XORQ(buffer.Offset(19*8), y4)
	MOVQ(y4, buffer.Offset(19*8))

	Comment("27=1^27^19^25^9")
	XORQ(y1, y0)
	XORQ(y7, y0)
	XORQ(y5, y0)
	XORQ(y3, y0)
	XORQ(buffer.Offset(27*8), y0)
	MOVQ(y0, buffer.Offset(27*8))

	Comment("11=11^3^9^17^25")
	MOVQ(y9, y4)
	XORQ(b.Offset(11*8), y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(y3, y4)
	XORQ(buffer.Offset(11*8), y4)
	MOVQ(y4, buffer.Offset(11*8))

	MOVQ(b.Offset(7*8), y0)
	MOVQ(b.Offset(15*8), y5)
	Comment("17=17^9^31^7^15")
	XORQ(y2, y1)
	XORQ(y8, y1)
	XORQ(y0, y1)
	XORQ(y5, y1)
	XORQ(buffer.Offset(17*8), y1)
	MOVQ(y1, buffer.Offset(17*8))

	Comment("25=25^17^7^15^23")
	XORQ(y3, y2)
	XORQ(y0, y2)
	XORQ(y5, y2)
	XORQ(y6, y2)
	XORQ(buffer.Offset(25*8), y2)
	MOVQ(y2, buffer.Offset(25*8))

	MOVQ(b.Offset(5*8), y1)
	MOVQ(b.Offset(11*8), y2)
	MOVQ(b.Offset(13*8), y3)
	// y0=7, y1=5, y9=y4=3, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31
	Comment("5=5^29^3^11^19")
	MOVQ(y9, y4)
	XORQ(y1, y4)
	XORQ(y2, y4)
	XORQ(b.Offset(19*8), y4)
	XORQ(b.Offset(29*8), y4)
	XORQ(buffer.Offset(5*8), y4)
	MOVQ(y4, buffer.Offset(5*8))

	Comment("21=21^13^19^27^3")
	MOVQ(y9, y4)
	XORQ(b.Offset(21*8), y4)
	XORQ(y3, y4)
	XORQ(b.Offset(19*8), y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(21*8), y4)
	MOVQ(y4, buffer.Offset(21*8))

	Comment("29=29^21^27^3^11")
	XORQ(b.Offset(29*8), y9)
	XORQ(b.Offset(21*8), y9)
	XORQ(y7, y9)
	XORQ(y2, y9)
	XORQ(buffer.Offset(29*8), y9)
	MOVQ(y9, buffer.Offset(29*8))

	MOVQ(b.Offset(21*8), y9)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=27, y8=31

	Comment("7=7^31^5^13^21")
	MOVQ(y1, y4)
	XORQ(y0, y4)
	XORQ(y3, y4)
	XORQ(y8, y4)
	XORQ(y9, y4)
	XORQ(buffer.Offset(7*8), y4)
	MOVQ(y4, buffer.Offset(7*8))

	Comment("13=13^5^11^19^27")
	MOVQ(y1, y4)
	XORQ(y3, y4)
	XORQ(y2, y4)
	XORQ(b.Offset(19*8), y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(13*8), y4)
	MOVQ(y4, buffer.Offset(13*8))

	MOVQ(b.Offset(29*8), y7)
	// y0=7, y1=5, y9=21, y2=11, y3=13, y5=15, y6=23, y7=29, y8=31
	Comment("23=23^15^21^29^5")
	MOVQ(y1, y4)
	XORQ(y5, y4)
	XORQ(y6, y4)
	XORQ(y9, y4)
	XORQ(y7, y4)
	XORQ(buffer.Offset(23*8), y4)
	MOVQ(y4, buffer.Offset(23*8))

	Comment("31=31^23^29^5^13")
	XORQ(y8, y1)
	XORQ(y6, y1)
	XORQ(y3, y1)
	XORQ(y7, y1)
	XORQ(buffer.Offset(31*8), y1)
	MOVQ(y1, buffer.Offset(31*8))

	Comment("15=15^7^13^21^29")
	XORQ(y3, y0)
	XORQ(y7, y0)
	XORQ(y9, y0)
	XORQ(y5, y0)
	XORQ(buffer.Offset(15*8), y0)
	MOVQ(y0, buffer.Offset(15*8))

	RET()
}

func xorRoundKey64() {
	// xorRoundKey64 function
	TEXT("xorRoundKey64", NOSPLIT, "func(rk uint32, x1, x2, x3, out *byte)")
	Doc("xor x1, x2, x3 with round key, 16 bytes per bit")

	x := Load(Param("rk"), GP32())
	x1 := Mem{Base: Load(Param("x1"), GP64())}
	x2 := Mem{Base: Load(Param("x2"), GP64())}
	x3 := Mem{Base: Load(Param("x3"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	ret, nret := GP64(), GP64()

	y := GP32()

	count := GP64()
	XORQ(count, count)
	Comment("Handle first byte")
	MOVL(U32(0x01000000), y)
	Label("rk_b1")
	MOVQ(x1.Idx(count, 1), ret)
	XORQ(x2.Idx(count, 1), ret)
	XORQ(x3.Idx(count, 1), ret)
	MOVQ(ret, nret)
	NOTQ(nret)
	TESTL(x, y)
	CMOVQEQ(ret, nret)
	MOVQ(nret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(8), count)
	CMPQ(count, U32(64))
	JL(LabelRef("rk_b1"))

	Comment("Handle second byte")
	MOVL(U32(0x00010000), y)
	Label("rk_b2")
	MOVQ(x1.Idx(count, 1), ret)
	XORQ(x2.Idx(count, 1), ret)
	XORQ(x3.Idx(count, 1), ret)
	MOVQ(ret, nret)
	NOTQ(nret)
	TESTL(x, y)
	CMOVQEQ(ret, nret)
	MOVQ(nret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(8), count)
	CMPQ(count, U32(128))
	JL(LabelRef("rk_b2"))

	Comment("Handle third byte")
	MOVL(U32(0x00000100), y)
	Label("rk_b3")
	MOVQ(x1.Idx(count, 1), ret)
	XORQ(x2.Idx(count, 1), ret)
	XORQ(x3.Idx(count, 1), ret)
	MOVQ(ret, nret)
	NOTQ(nret)
	TESTL(x, y)
	CMOVQEQ(ret, nret)
	MOVQ(nret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(8), count)
	CMPQ(count, U32(192))
	JL(LabelRef("rk_b3"))

	Comment("Handle last byte")
	MOVL(U32(0x00000001), y)
	Label("rk_b4")
	MOVQ(x1.Idx(count, 1), ret)
	XORQ(x2.Idx(count, 1), ret)
	XORQ(x3.Idx(count, 1), ret)
	MOVQ(ret, nret)
	NOTQ(nret)
	TESTL(x, y)
	CMOVQEQ(ret, nret)
	MOVQ(nret, out.Idx(count, 1))
	ROLL(U8(1), y)
	ADDQ(U8(8), count)
	CMPQ(count, U32(256))
	JL(LabelRef("rk_b4"))

	RET()
}

func transpose64avx(flipMask Mem) {
	// transpose64avx function
	TEXT("transpose64avx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, 64x128")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	tmp := Y0
	o := GP32()
	cc := GP64()

	Comment("Initialize rr, current row")
	rr := zero()
	Label("row_loop")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop")

	Comment("Initialize (rr * ncols + cc) / 8, here ncols=128")
	addr := GP64()
	MOVQ(rr, addr)
	Comment("Multiple with ncols")
	SHLQ(Imm(7), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes128(flipMask, in, o, addr, t1)
	getFirst4Bytes128(flipMask, in, o, addr, t2)
	getFirst4Bytes128(flipMask, in, o, addr, t3)
	getFirst4Bytes128(flipMask, in, o, addr, t4)
	getFirst4Bytes128(flipMask, in, o, addr, t5)
	getFirst4Bytes128(flipMask, in, o, addr, t6)
	getFirst4Bytes128(flipMask, in, o, addr, t7)
	getFirst4Bytes128(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 64")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(6), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(8), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 64")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(6), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(8), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 64")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(6), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(8), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)

	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 64")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	Comment("Multiple with nrows")
	SHLQ(Imm(6), addr)
	ADDQ(rr, addr)
	SHRQ(Imm(3), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(8), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=128")
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop"))
	ADDQ(Imm(32), rr)
	Comment("Compare rr with nrows, here nrows=64")
	CMPQ(rr, U8(64))
	JL(LabelRef("row_loop"))

	VZEROUPPER()
	RET()
}

func transpose64RevAvx(flipMask Mem) {
	// transpose64RevAvx function
	TEXT("transpose64RevAvx", NOSPLIT, "func(in, out *byte)")
	Doc("Bit level matrix transpose, b0-b1-b2-b3, 128x64")

	in := Mem{Base: Load(Param("in"), GP64())}
	out := Mem{Base: Load(Param("out"), GP64())}

	h, l := X1, X0
	tmp := Y0
	t1, t2, t3, t4, t5, t6, t7, t8 := XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM(), XMM()
	o := GP32()

	Comment("Initialize rr, current row, 96")
	rr := zero()
	cc := GP64()
	addr := GP64()

	Label("row_loop_b3")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b3")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=64")
	MOVQ(U32(6144), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes64(flipMask, in, o, addr, t1)
	getFirst4Bytes64(flipMask, in, o, addr, t2)
	getFirst4Bytes64(flipMask, in, o, addr, t3)
	getFirst4Bytes64(flipMask, in, o, addr, t4)
	getFirst4Bytes64(flipMask, in, o, addr, t5)
	getFirst4Bytes64(flipMask, in, o, addr, t6)
	getFirst4Bytes64(flipMask, in, o, addr, t7)
	getFirst4Bytes64(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=64")
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_b3"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b2")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b2")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=64")
	MOVQ(U32(4096), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes64(flipMask, in, o, addr, t1)
	getFirst4Bytes64(flipMask, in, o, addr, t2)
	getFirst4Bytes64(flipMask, in, o, addr, t3)
	getFirst4Bytes64(flipMask, in, o, addr, t4)
	getFirst4Bytes64(flipMask, in, o, addr, t5)
	getFirst4Bytes64(flipMask, in, o, addr, t6)
	getFirst4Bytes64(flipMask, in, o, addr, t7)
	getFirst4Bytes64(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(4), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=64")
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_b2"))

	ADDQ(Imm(32), rr)

	Label("row_loop_b1")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b1")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=64")
	MOVQ(U32(2048), addr)
	ADDQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of the 32 rows")
	getFirst4Bytes64(flipMask, in, o, addr, t1)
	getFirst4Bytes64(flipMask, in, o, addr, t2)
	getFirst4Bytes64(flipMask, in, o, addr, t3)
	getFirst4Bytes64(flipMask, in, o, addr, t4)
	getFirst4Bytes64(flipMask, in, o, addr, t5)
	getFirst4Bytes64(flipMask, in, o, addr, t6)
	getFirst4Bytes64(flipMask, in, o, addr, t7)
	getFirst4Bytes64(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(8), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=64")
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_b1"))

	ADDQ(Imm(32), rr)
	Label("row_loop_b0")
	Comment("Initialize cc, current col")
	XORQ(cc, cc)
	Label("col_loop_b0")
	Comment("Initialize (rr * ncols + cc) / 8, here ncols=64")
	MOVQ(cc, addr)
	SHRQ(Imm(3), addr)

	Comment("Construct eight XMM with first 4 bytes of first 32 rows")
	getFirst4Bytes64(flipMask, in, o, addr, t1)
	getFirst4Bytes64(flipMask, in, o, addr, t2)
	getFirst4Bytes64(flipMask, in, o, addr, t3)
	getFirst4Bytes64(flipMask, in, o, addr, t4)
	getFirst4Bytes64(flipMask, in, o, addr, t5)
	getFirst4Bytes64(flipMask, in, o, addr, t6)
	getFirst4Bytes64(flipMask, in, o, addr, t7)
	getFirst4Bytes64(flipMask, in, o, addr, t8)

	Comment("Matrix transform 4x4")
	VPUNPCKHDQ(t2, t1, h)
	VPUNPCKLDQ(t2, t1, t1)
	VPUNPCKLDQ(t4, t3, l)
	VPUNPCKHDQ(t4, t3, t3)
	VPUNPCKHQDQ(l, t1, t2)
	VPUNPCKLQDQ(l, t1, t1)
	VPUNPCKHQDQ(t3, h, t4)
	VPUNPCKLQDQ(t3, h, t3)

	VPUNPCKHDQ(t6, t5, h)
	VPUNPCKLDQ(t6, t5, t5)
	VPUNPCKLDQ(t8, t7, l)
	VPUNPCKHDQ(t8, t7, t7)
	VPUNPCKHQDQ(l, t5, t6)
	VPUNPCKLQDQ(l, t5, t5)
	VPUNPCKHQDQ(t7, h, t8)
	VPUNPCKLQDQ(t7, h, t7)

	MOVOU(t1, l)
	VINSERTI128(Imm(1), t5, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t2, l)
	VINSERTI128(Imm(1), t6, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t3, l)
	VINSERTI128(Imm(1), t7, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	MOVOU(t4, l)
	VINSERTI128(Imm(1), t8, tmp, tmp)
	Comment("Initialize ((cc + 7) * nrows + rr) / 8, here nrows = 128")
	MOVQ(cc, addr)
	ADDQ(Imm(7), addr)
	SHLQ(Imm(4), addr)
	ADDQ(Imm(12), addr)

	Comment("Get the most significant bit of each 8-bit element in the YMM, and store the returned 4 bytes")
	for i := 7; i >= 0; i-- {
		VPMOVMSKB(tmp, o)
		MOVL(o, out.Idx(addr, 1))
		VPSLLQ(Imm(1), tmp, tmp)
		Comment("Sub nrows / 8")
		SUBQ(Imm(16), addr)
	}
	ADDQ(Imm(8), cc)

	Comment("Compare cc with ncols, here ncols=64")
	CMPQ(cc, Imm(64))
	JL(LabelRef("col_loop_b0"))

	VZEROUPPER()
	RET()
}

func main() {
	ConstraintExpr("amd64,gc,!purego")
	flipMask := GLOBL("flip_mask", RODATA|NOPTR)
	DATA(0, U64(0x0d0905010c080400))
	DATA(8, U64(0x0f0b07030e0a0602))
	transpose64avx(flipMask)
	transpose64RevAvx(flipMask)
	transpose64Rev()
	transpose128avx(flipMask)
	transpose128RevAvx(flipMask)
	transpose256avx(flipMask)
	transpose128x256avx2(flipMask)
	transpose256RevAvx(flipMask)
	xor32x128()
	xor32x128avx()
	xorRoundKey128()
	sbox128()
	l128()
	l256()
	sbox256avx2()
	xorRoundKey256avx2()
	sbox64()
	l64()
	xorRoundKey64()

	Generate()
}

// zero zeroes a new register and returns it.
func zero() Register {
	r := GP64()
	XORQ(r, r)
	return r
}
