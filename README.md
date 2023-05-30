# sm4bs
实验性项目，通过比特切片等技术来实现sm4，验证性能。

目前性能如下：

**128组**：
```
goos: windows
goarch: amd64
pkg: github.com/emmansun/sm4bs
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkXor32-6   	84093679	        13.37 ns/op
BenchmarkXorRK-6   	37912296	        31.48 ns/op
BenchmarkL128-6   	56766843	        20.75 ns/op	       0 B/op	       0 allocs/op
BenchmarkSbox128-6   	68027982	        17.27 ns/op	       0 B/op	       0 allocs/op
BenchmarkTao128-6   	14089119	        86.37 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS128TransposeAvx-6   	 2279635	       511.7 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS128TransposeRevAvx-6   	 2365981	       511.2 ns/op	       0 B/op	       0 allocs/op
BenchmarkEncrypt128Blocks-6   	  170002	      6755 ns/op	 303.18 MB/s	    3072 B/op	       2 allocs/op
```
**256组**：
```
goos: windows
goarch: amd64
pkg: github.com/emmansun/sm4bs
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkSbox256-6   	68508790	        17.99 ns/op	       0 B/op	       0 allocs/op
BenchmarkTao256-6   	13837366	        88.67 ns/op	       0 B/op	       0 allocs/op
BenchmarkL256-6   	60229776	        19.20 ns/op	       0 B/op	       0 allocs/op
BenchmarkXorRK256-6   	35646387	        34.23 ns/op	       0 B/op	       0 allocs/op
BenchmarkXor32x256-6   	54216703	        21.92 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS256TransposeAvx-6   	 1000000	      1046 ns/op	       0 B/op	       0 allocs/op
BenchmarkBS256TransposeRevAvx-6   	 1000000	      1020 ns/op	       0 B/op	       0 allocs/op
BenchmarkEncrypt256Blocks-6   	  132698	      8964 ns/op	 456.93 MB/s	    6144 B/op	       2 allocs/op
```

**AES-NI + AVX2方案**：
```
goos: windows
goarch: amd64
cpu: Intel(R) Core(TM) i5-9500 CPU @ 3.00GHz
BenchmarkAESNIEncrypt128Blocks-6   	  297854	      3972 ns/op	 515.63 MB/s	       0 B/op	       0 allocs/op
BenchmarkAESNIEncrypt256Blocks-6   	  147344	      8047 ns/op	 508.99 MB/s	       0 B/op	       0 allocs/op
```

Detail：
- 128/256组：https://github.com/emmansun/sm4bs/issues/1

Reference:
- https://github.com/emmansun/gmsm/discussions/116
