This directory contains example kernels, that we would like to
support.

Array reversal using shared memory
----------------------------------
Description: http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
   
CUDA version: https://github.com/parallel-forall/code-samples/blob/master/series/cuda-cpp/shared-memory/shared-memory.cu

Array transpose using shared memory
-----------------------------------
Description: http://devblogs.nvidia.com/parallelforall/efficient-matrix-transpose-cuda-cc/

CUDA version: https://github.com/parallel-forall/code-samples/blob/master/series/cuda-cpp/transpose/transpose.cu

MonteCarlo \pi simulation using Sobol-sequences (3 different Sobol-algorithms)
-----------------------------------------------------------------------------
GPU algorithm described here:
https://people.maths.ox.ac.uk/gilesm/files/gems_rng.pdf

Brownian-bridge algorithm
-------------------------
GPU algorithm described here:
http://www.nag.com/doc/techrep/pdf/tr2_12.pdf

This is a very detailed study doing stuff like prefetching from
shared-memory to registers, precomputing optimal execution strategies
etc. The focus here should not be on those aspects, the important
thing here is the memory access pattern which may require mutable
updates in our language.

Other
-----

Longstaff & Schwartz algorithm:
http://devblogs.nvidia.com/parallelforall/american-option-pricing-monte-carlo-simulation/

NVidia provides more OpenCL examples in their GPU Computing SDK. A
copy is available here:
https://github.com/ChiahungTai/OpenCL-playgorund/blob/master/nvidia/nvidia-opencl-examples/
