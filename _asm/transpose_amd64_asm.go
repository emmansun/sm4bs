package main

import (
	. "github.com/mmcloughlin/avo/build"
	. "github.com/mmcloughlin/avo/operand"
	. "github.com/mmcloughlin/avo/reg"
)

//go:generate go run . -out ../transpose128_amd64.s -stubs ../transpose128_amd64.go -pkg sm4bs

func main() {
	ConstraintExpr("amd64,gc,!purego")

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
		SUBQ(Imm(16), addr)
	}

	Comment("Compare cc with ncols, here ncols=128")
	ADDQ(Imm(8), cc)
	CMPQ(cc, Imm(128))
	JL(LabelRef("col_loop"))

	Comment("Compare cc with nrows, here nrows=128")
	ADDQ(Imm(16), rr)
	CMPQ(rr, Imm(128))
	JL(LabelRef("row_loop"))

	RET()
	Generate()
}

// zero zeroes a new register and returns it.
func zero() Register {
	r := GP64()
	XORQ(r, r)
	return r
}
