# sm4bs
实验性项目，通过比特切片等技术来实现sm4，验证性能。

目前性能如下：

**64组**：
```
goos: windows
goarch: amd64
pkg: github.com/emmansun/sm4bs
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkXorRK64-6   	38663529	        29.36 ns/op	       0 B/op	       0 allocs/op
BenchmarkSbox64-6   	67601062	        17.83 ns/op	       0 B/op	       0 allocs/op
BenchmarkTao64-6   	14010654	        82.90 ns/op	       0 B/op	       0 allocs/op
BenchmarkL64-6   	54585649	        22.99 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS64TransposeAvx-6   	 4376485	       274.2 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS64TransposeRevAvx-6   	 4492495	       267.5 ns/op	       0 B/op	       0 allocs/op
BenchmarkEncrypt64Blocks-6   	  218025	      5303 ns/op	 193.10 MB/s	    1536 B/op	       2 allocs/op
```

**128组**：
```
goos: windows
goarch: amd64
pkg: github.com/emmansun/sm4bs
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkXor32-6   	84093679	        13.37 ns/op
BenchmarkXorRK-6   	37912296	        31.48 ns/op
BenchmarkL128-6   	52159624	        22.68 ns/op	       0 B/op	       0 allocs/op
BenchmarkSbox128-6   	68027982	        17.27 ns/op	       0 B/op	       0 allocs/op
BenchmarkTao128-6   	14089119	        86.37 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS128TransposeAvx-6   	 2279635	       511.7 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS128TransposeRevAvx-6   	 2365981	       511.2 ns/op	       0 B/op	       0 allocs/op
BenchmarkEncrypt128Blocks-6   	  205614	      5557 ns/op	 368.55 MB/s	    3072 B/op	       2 allocs/op
```
**256组**：
```
goos: windows
goarch: amd64
pkg: github.com/emmansun/sm4bs
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkSbox256-6   	68508790	        17.99 ns/op	       0 B/op	       0 allocs/op
BenchmarkTao256-6   	13837366	        88.67 ns/op	       0 B/op	       0 allocs/op
BenchmarkL256-6   	50736309	        22.45 ns/op	       0 B/op	       0 allocs/op
BenchmarkXorRK256-6   	35646387	        34.23 ns/op	       0 B/op	       0 allocs/op
BenchmarkXor32x256-6   	54216703	        21.92 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS256TransposeAvx-6   	 1000000	      1046 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS256TransposeRevAvx-6   	 1000000	      1020 ns/op	       0 B/op	       0 allocs/op
BenchmarkEncrypt256Blocks-6   	  145350	      8104 ns/op	 505.45 MB/s	    6144 B/op	       2 allocs/op
```

**AES-NI + AVX2方案**：
```
goos: windows
goarch: amd64
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkAESNIEncrypt64Blocks-6   	  469024	      2254 ns/op	 454.21 MB/s	       0 B/op	       0 allocs/op
BenchmarkAESNIEncrypt128Blocks-6   	  297854	      3972 ns/op	 515.63 MB/s	       0 B/op	       0 allocs/op
BenchmarkAESNIEncrypt256Blocks-6   	  147344	      8047 ns/op	 508.99 MB/s	       0 B/op	       0 allocs/op
```

Detail：
- 128/256组：https://github.com/emmansun/sm4bs/issues/1

Reference:
- https://github.com/emmansun/gmsm/discussions/116
