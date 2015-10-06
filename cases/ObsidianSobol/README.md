Sobol-sequence generation
=========================
This case study has become an important component in the HIPERFIT
research center, and is also used in benchmarks obtained from partner
companies.

Sobol-sequences are quasi-random sequences of uniformly distributed
numbers, useful for e.g. Monte-Carlo integration. A set of precomputed
"direction numbers" are used to define the sobol-sequence,
multi-dimensional sobol-sequences uses different "direction numbers"
for each dimension.

The case is interesting as the most efficient GPU algorithm depends on
the block-size, and is thus not possible to write in most high-level
parallel languages, where the block size is not of the users concern.


There are three algorithms
--------------------------
 * Independent: each number can be computed independently using a simple formula.

 * Recursive: we can calculate the sobol number at index (i+1) from
   (i), with fewer operations.

   Combining the independent formula with the recursive can give a
   good parallel implementation, where each thread fills seperate
   parts of the array.

 * A GPU version of this has been developed by Thomas Bradley, Mike
   Giles et al.
   https://people.maths.ox.ac.uk/gilesm/files/gems_rng.pdf

   This algorithm depends on the choice of Blocksize! It makes it
   possible for the threads to cooperatively fill each block of the
   array, thus obtaining memory coalescing.
