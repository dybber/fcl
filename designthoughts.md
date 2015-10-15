Design thoughts
===============

Language design
---------------
 * Start with language constructs like those in Michel Steuwer et
   al. ICFP'15,

 * Array permutations are not supported in Michel Steuwer et al.'s
   language, some kind of index-space transformation should be
   supported

 * Instead of a single sequential reduction primitive, add sequential
   loops as in Obsidian, to allow different possible iteration
   patterns?

   - Look at Futhark and Rust, and their use of uniqueness types to
     avoid aliasing of mutable arrays in sequential code.

 * Rewrites and automatic fusion should not be part of this at all, I
   believe!

 * Perhaps move toLocal/toGlobal/reorder-annotations to the
   type-level? Maybe on function arrows, an effect-system of some
   kind?

Output of compilation
---------------------
This has to be very practical and useful to succeed!

 * Should be easy to integrate in other compiler projects, that
   generates C-code. Generate OpenCL kernels and C-wrappers for
   invoking them on OpenCL buffers? E.g. like the output of
   CLU-generator.

 * The wrappers should allow to keep

 * Will use CLU from Intel in first iteration to simplify the
   host-side: https://github.com/Computing-Language-Utility/CLU/

 * Should generate C-code, not execute using Haskell OpenCL/CUDA
   primitives.

Implementation language
-----------------------
Haskell2010 with as few dependencies as possible.

I will probably use the language-c-quote package which I have already
extended to support OpenCL though it brings in a lot of dependencies!

Cost semantics
--------------
It would be really nice if programs could be given a cost-semantics
of some sort! Ideally in terms of some parameters regarding the
hardware as well as input size (see Koji Nakano papers)

Limitations
-----------

 * The user should be able to express how everything is fused, so
   fusion should not be automatic.

 * Scheduling kernels is not a part of this project, that should be
   implemented at a higher level.

 * We will only generate individual kernels, which can be called from
   some runtime system

 * Data-movement between host and device is not mentioned in Michel
   Steuwers design. I also think we should avoid doing this at all on
   this level and instead assume that input arrays are already present
   on the device and that the output should stay on the device for the
   next kernel to consume.

   There might thus be need for an additional layer on top of this to
   specify these memory movement and kernel-invocation schedules?


Benchmarks
----------
See the README-file in cases-directory.

Additional features
--------------------

Stuff we might not have time to do for this paper, but would be nice
to add later:

 * Compiling to CUDA

 * Some macro-language on top to make it easier to write big (but
   recursively defined) kernels? Similar to the role Haskell plays in
   Obsidian.

 * Some scheduling and data-movement language on top of this (see
   Limitations section)
