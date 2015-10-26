Related work on low-level intermediate languages
================================================

Additional references can be found here:
<http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/PapersRelatedToFeldsparObsidianIntermediateRepresentationsAndTheLike>



Low-level
---------

 * FSCL - F# to OpenCL
   http://fscl.github.io/FSCL.Compiler/
 * Generating Performance Portable Code using Rewrite Rules (ICFP'15)
   by Michel Steuwer, Christian Fensch, Sam Lindley
 * SkelCL
 * [Insieme / INSPIRE](http://www.dps.uibk.ac.at/insieme/architecture.html)
 * Obsidian
 * [OKL: A Unified Language for Parallel Architectures](http://dsmedina.com/pdf/papers/phd.pdf) by David Medina 
 * PIRE: [A Parallel Intermediate representation for Embedded Languages](http://publications.lib.chalmers.se/records/fulltext/184387/184387.pdf),
  Master thesis by Ivar Lång
 * Delite:
  - [A Heterogeneous Parallel Framework for Domain-Specific Languages](https://ppl.stanford.edu/papers/pact11-brown.pdf)
  - [Additional publications](https://stanford-ppl.github.io/Delite/publications.html)
 * [Intermediate representation for heterogeneous multi-core: A survey](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=7050496&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D7050496)
   - And a lot of papers that it references also seem relevant to look at:
     *  "Automatic extraction of functional parallelism from ordinary programs"
        (also not available from Chalmers)
 * [Kimble: a Hierarchical Intermediate Representation for Multi-Grain Parallelism](http://nbenoit.tuxfamily.org/projects/gomet/wir11-kimble.pdf)
 * [EXOCHI](http://web.cs.ucla.edu/~palsberg/course/cs239/papers/wang.pdf)
   Check also the papers that references this one!
 * Qilin
 * Pencil IR
 * [SPIR](https://www.khronos.org/spir)
 * CUDA PTX. This is what CUDA compiles to, so it might be too
   low-level to be interesting, but maybe there's something to learn from
   it? I have never looked at it in detail.


Parallel cost-model papers
--------------------------
 * "Simple Memory Machine Models for GPUs" by Koji Nakano, IPDPS'12 (IEEE)
 * "The Hierarchical Memory Machine Model for GPUs" by Koji Nakano, IPDPS'13 (IEEE)
 * [A memory access model for highly-threaded many-core architectures](http://www1.cse.wustl.edu/~lin.ma/publications/ICPADS12.pdf)
 * [Provably Efficient GPU Algorithms](http://arxiv.org/pdf/1306.5076.pdf)
 * "A Practical Hierarchical Model of Parallel Computation" by Heywood and Ranka (Hierarchical PRAM)
 * "A detailed GPU cache model based on Reuse Distance Theory" by Nugteren et al.
 * [Modeling parallel computers as memory hierarchies](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=315548&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D315548)
 * [LogP: A practical model of parallel computation](http://rsim.cs.uiuc.edu/arch/qual_papers/systems/5.pdf)

High-level intermediate languages
---------------------------------
 * Futhark
 * Single-assignment C
 * Bohrium
 * VCODE

Intermediate languages for sequential compilers
-----------------------------------------------
 * LLVM
 * C-- (or Cmm, or C minus minus)
   Intermediate language in GHC. "C without type declarations and pointers" + a runtime system for
   - Talk on vimeo: <https://vimeo.com/69025829>


Permutations/Index-transformations
----------------------------------
Might be relevant at some later point: http://dl.acm.org/citation.cfm?id=165248
