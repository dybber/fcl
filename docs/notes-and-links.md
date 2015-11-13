Calculating peak bandwidth for a GPU
------------------------------------
http://stackoverflow.com/questions/15055877/how-to-get-memory-bandwidth-from-memory-clock-memory-speed

 - e.g. 384 bit bus width, 900 Mhz DDR

   - 900 Mhz = 900 Megabit per second
   - DDR = Double Data Rate, thus multiply by two
   - multiply by bus width

   - 900 * 2 * 384 Mbps = 86,4 Gbps

Measuring bandwidth for a GPU
-----------------------------
 * Measure the time it takes to copy data between two large arrays
http://stackoverflow.com/questions/29196123/measuring-opencl-kernels-memory-throughput


Various low-level optimizations
--------------------------------

### Shuffle (SHFL) instruction on Kepler
An instruction in CUDA for exchanging data between warps. We should
not aim to support this, but it would be worth noting in the paper, as
such an instruction makes GPU-programming even more complicated and
could eventually be abstracted away.

http://devblogs.nvidia.com/parallelforall/cuda-pro-tip-kepler-shuffle/
http://www.pixel.io/blog/2013/3/25/fast-matrix-transposition-on-kepler-without-using-shared-mem.html


### Vectorised loads
Using int2/int4 when reading and writing data to memory, generates
different instructions that performs better.

Addresses must be aligned.

http://devblogs.nvidia.com/parallelforall/cuda-pro-tip-increase-performance-with-vectorized-memory-access/


### Strip-mining

Do the same amount of work using fewer threads

 - Wastes fewer registers (Threads eat registers pretty fast)
 - which makes it possible to run concurrent workgroups/blocks
