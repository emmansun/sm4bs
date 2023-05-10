package sm4bs

import (
	"bytes"
	"testing"
)

func TestBsTranspose(t *testing.T) {
	in := make([]byte, BSBlockSize)
	out := make([]uint64, BlockSize)

	in[0] = 1
	bsTranspose(in, out)
	if out[7] != 0x1 {
		t.Fatalf("not expected %x", out)
	}
	out[0] = 0

	for i := 0; i < 64; i++ {
		in[i*16] = 1
	}
	bsTranspose(in, out)
	if out[7] != 0xffffffffffffffff {
		t.Fatalf("not expected %x", out)
	}
}

func TestBsTransposeRev(t *testing.T) {
	in := make([]byte, BSBlockSize)
	ret := make([]byte, BSBlockSize)
	out := make([]uint64, BlockSize)

	in[0] = 1
	bsTranspose(in, out)
	bsTransposeRev(out, ret)
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %v", ret)
	}
	for i := 0; i < 64; i++ {
		in[i*16] = 1
	}
	bsTranspose(in, out)
	bsTransposeRev(out, ret)
	if !bytes.Equal(in, ret) {
		t.Fatalf("not expected %v", ret)
	}
}
