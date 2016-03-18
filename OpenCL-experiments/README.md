Copy 1D vs. 2D-grid
------------------
In this experiment we want to measure the difference between copying
a 2D-array using two different approaches to indexing:

 * 1D-indexing, launching a 1D-grid
 * 2D-indexing, launching a 2D-grid

We implement two versions of each case, one using shared memory, and
one not using shared memory.

(Maybe it would also be interesting to do 2D-indexing, but launching a
1D-grid and computing the 2D indices ourselves.)

### Conclusion

The goal was to decide whether we should support 2D/3D grids when
generating OpenCL or not. The results indicate that there is an actual
slow-down when using 2D-grids, so they might just emulate them.

It would be interesting to see differences in generated PTX-code.

### Results
 * Copying 4194304 32-bit integers
 * 1000 executions
 * Workgroup size: 256
 * No work-group virtualization.

#### AMD FirePro W8100
2D grid, 2D-indexing

 - **simple_copy**, Throughput =  99.4611 GB/s, Time = 0.00017 s
 - **shared_copy**, Throughput = 100.6432 GB/s, Time = 0.00017 s

1D grid, 1D-indexing

 - **simple_copy1D**, Throughput = 113.5291 GB/s, Time = 0.00015 s
 - **shared_copy1D**, Throughput = 109.5472 GB/s, Time = 0.00015 s

#### NVIDIA Geforce GTX 780 Ti
2D grid, 2D-indexing

 - simple_copy, Throughput = 119.3844 GB/s, Time = 0.00014 s
 - shared_copy, Throughput = 98.6014 GB/s, Time = 0.00017 s

1D grid, 1D-indexing

 - simple_copy1D, Throughput = 127.5270 GB/s, Time = 0.00013 s
 - shared_copy1D, Throughput = 124.4572 GB/s, Time = 0.00013 s


Transpose 1D vs. 2D-grid
-----------------------
In this experiment we want to measure the difference between
transposing a 2D-array using two different approaches to indexing:

 * 2D-indexing, launching a 2D-grid
 * 2D-indexing, launching a 1D-grid and computing indices w. div/modulus

The goal is to decide whether we should support 2D/3D grids when
generating OpenCL or not.

### Results

Work-group virtualization
-------------------------
Experiment with the performance penalty of work-group/block
virtualization (strip-mining?) in OpenCL. 

### Conclusions

Work-group virtualization gives some speed-up on our NVIDIA card, but
the slow-down on AMD card shows that it shouldn't be something we do
always.

The FCL-compiler should have a flag for it!

### Results
 * 1000 executions
 * Work-group size: 256

#### AMD FirePro W8100

##### 2^22 32-bit ints

Without virtualization:

 - Num. work groups: 16384, 1 block per work-group
 - simple_copy1D: Throughput = 114.9707 GB/s, Time = 0.00015 s
 - shared_copy1D: Throughput = 112.1030 GB/s, Time = 0.00015 s

With virtualization

 - Num. work groups: 4096, 4 blocks per work-group
 - simple_copy1D_virt: Throughput = 104.8124 GB/s, Time = 0.00016 s
 - simple_copy1D_virt_strength_reduced: Throughput = 104.5954 GB/s, Time = 0.00016 s

##### 2^24 32-bit ints

Without virtualization:

 - Num. work groups: 65536, 1 block per work-group
 - simple_copy1D: Throughput = 122.8079 GB/s, Time = 0.00055 s
 - shared_copy1D: Throughput = 107.2666 GB/s, Time = 0.00063 s


With virtualization

 - Num. work groups: 4096, 16 blocks per work-group
 - simple_copy1D_virt: Throughput = 107.8420 GB/s, Time = 0.00062 s
 - simple_copy1D_virt_strength_reduced: Throughput = 97.4117 GB/s, Time = 0.00069 s


#### NVIDIA Geforce GTX 780 Ti

##### 2^22 32-bit ints

Without virtualization:

 - Num. work groups: 16384, 1 block per work-group
 - simple_copy1D: Throughput = 126.4077 GB/s, Time = 0.00013 s
 - shared_copy1D: Throughput = 116.9503 GB/s, Time = 0.00014 s

With virtualization

 - Num. work groups: 4096, 4 blocks per work-group
 - simple_copy1D_virt: Throughput = 132.1665 GB/s, Time = 0.00013 s
 - simple_copy1D_virt_strength_reduced: Throughput = 132.1633 GB/s, Time = 0.00013 s


##### 2^24 32-bit ints

Without virtualization:

 - Num. work groups: 65536, 1 block per work-group
 - simple_copy1D: Throughput = 130.8859 GB/s, Time = 0.00051 s
 - shared_copy1D: Throughput = 128.2701 GB/s, Time = 0.00052 s

With virtualization

 - Num. work groups: 4096, 16 blocks per work-group
 - simple_copy1D_virt: Throughput = 135.6162 GB/s, Time = 0.00049 s
 - simple_copy1D_virt_strength_reduced: Throughput = 135.7586 GB/s, Time = 0.00049 s
