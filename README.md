# sm4bs
实验性项目，通过比特切片等技术来实现sm4，验证性能。

第一个版本，纯go语言，64组并发版本，性能很不理想。
```
    goos: windows
    goarch: amd64
    pkg: github.com/emmansun/sm4bs
    cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
    BenchmarkEncrypt64Blocks-6   	    7018	    152759 ns/op	   6.70 MB/s	   86016 B/op	     896 allocs/op
```

作为比较，gmsm的ecb模式，64组加密性能：
```golang
func BenchmarkAESNIEncryptBlocks(b *testing.B) {
	key := []byte{0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10}
	c, err := sm4.NewCipher(key)
	if err != nil {
		b.Fatal(err)
	}
	encrypter := cipher.NewECBEncrypter(c)
	input := make([]byte, BSBlockSize)
	output := make([]byte, BSBlockSize)
	for i := 0; i < WordSize; i++ {
		copy(input[i*16:], key)
	}
	b.SetBytes(int64(BSBlockSize))
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		encrypter.CryptBlocks(output, input)
	}
}
```
```
    goos: windows
    goarch: amd64
    pkg: github.com/emmansun/sm4bitsliced
    cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
    BenchmarkAESNIEncryptBlocks-6   	  561253	      2076 ns/op	 493.20 MB/s	       0 B/op	       0 allocs/op
```

Reference:
- https://github.com/emmansun/gmsm/discussions/116
