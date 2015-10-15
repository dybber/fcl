Related work on low-level intermediate languages
================================================

Additional references can be found here:
http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/PapersRelatedToFeldsparObsidianIntermediateRepresentationsAndTheLike


Low-level
---------

### Generating Performance Portable Code using Rewrite Rules (ICFP'15)
by Michel Steuwer, Christian Fensch, Sam Lindley

They present the fact that OpenCL is not performance-portable between
platforms, and show that different optimizations are necessary for
NVIDIA GPUs, AMD GPUs and Intel CPUs.

They address this issue by a rewrite system, where the high-level
constructs used in describing the program are rewritten automatically
to different low-level representations depending on the hardware
platform.

They claim to support nested arrays, but do not describe how they
handle deeply nested arrays, maybe they only parallelise on certain
dimensions, like Copperhead, sequentialising on the remaining
dimensions? Maybe they only support irregular arrays in the sequential
code, like Futhark?

No type inference, my guess is that it isn't possible.

##### High-level language
 * map, zip
 
 * reduce: like foldAll, reduction to a single scalar

 * split: cut's a vector into a vector of vectors, increasing
   dimension by one. (The first argument denotes the size of subarrays.)

 * join: catenate subarrays, lowering the dimension by one. (Inverse of
   split)

 * iterate: apply some function *n* times. It is allowed to shrink the
   array by some fixed amount in each iteration (given implicitly by
   the type of argument function).

 * reorder: an annotation that does not perform any computation, but
   specifies that the order of elements is not of importance.

<!-- On the high-level the language is much like Accelerate, and the -->
<!-- programmer is programming using well known combinators: map, reduce -->

##### Low-level representation

 * mapWorkgroup, mapLocal, mapGlobal, mapVec: All of these have the
   same type, but generates different OpenCL code

   - mapWorkgroup: each element is given to a workgroup
   - mapLocal: each element is given to one thread in a workgroup
   - mapVec: sequential map in a thread
   - mapGlobal: used if workgroup concept is not important I guess,
     disallowing any synchronization between threads I guess..
     
 * toLocal/toGlobal: annotation on function, saying if results should
   be stored in local (shared) memory or in global memory. Registers
   are not mentioned.

   "the implementation checks that a toLocal primitive is eventually
   followed by a toGlobal primitive to ensure that the final result is
   copied back into global memory"

 * reduceSeq: sequential reduction in a thread

 * reducePart: partial reduction, where *n* elements are still left
   afterwards.

 * reorderStride: ??

 * mapVec, splitVec, joinVec: used to target vector units (e.g. by
   using ```int4``` types in OpenCL), splitVec returns another
   array-type (angle brackets), which mapVec can consume and joinVec
   can convert back to an ordinary array

(Footnote: This is actually much like what I and my fellow student
suggested in a future work section of our Master thesis, though we
being even more imprecise and handwavy)

##### Rewrite rules
Mostly standard rewrite rules:

 * reordering annotations propagates through maps
 * map f --> join o map (map f) o split
 * fusion:
    - map f o map g --> map (f o g)
    - reduceSeq o mapSeq --> reduceSeq
 * join/split --> identity
 * reduce:
   - reduce --> reduce o reducePart
   - reducePart (resulting in scalar) --> reduce
   - reducePart --> reducePart o reorder
   - reducePart --> iterate (reducePart)
   - reducePart --> join o map (reducePart) o split
   - reduce --> reduceSeq

OpenCL specific:

 * map --> mapWorkgroup/mapLocal/mapGlobal/mapSeq
 * reorder --> reorderStride
 * mapLocal f --> toGlobal (mapLocal f)
 * mapLocal f --> toLocal (mapLocal f)
 * map f --> joinVec o map (mapVec f) o splitVec

See "designthoughts.md" for my thoughts on how we can extend on these
ideas for this project.

### SkelCL



### Insieme / INSPIRE
<http://www.dps.uibk.ac.at/insieme/architecture.html>

### Obsidian

### OKL
"OKL: A Unified Language for Parallel Architectures" by David Medina - <http://dsmedina.com/pdf/papers/phd.pdf>

### PIRE
Intermediate representation for Feldspar, Master thesis by Ivar Lång
<http://publications.lib.chalmers.se/records/fulltext/184387/184387.pdf>

### Delite
*A Heterogeneous Parallel Framework for Domain-Specific Languages*
<https://ppl.stanford.edu/papers/pact11-brown.pdf>

*Additional publications:* <https://stanford-ppl.github.io/Delite/publications.html>

Delite uses and extends the *Lightweight Modular Staging* (LMS)
framework for embedding DSLs in Scala (generative programming?).

Embedding a DSL using Delite and LMS. The DSL programs are compiled to
an intermediate machine-agnostic representation, from which the Delite
runtime can generate code that execute on various hardware platforms
(generating Scala, C++ or CUDA code), thus enabling heterogeneous
computing.

They support domain-specific optimizations,e .g. rewriting matrix
multiplications: ```AB + AC --> A(B + C)```

##### Mutable state
Delite provides for a programming model where most operations are
side-effect free, but they are pragmatic and thus still allow
restricted access side-effects. All objects starts out immutable, but
a mutable (in-place updatable) *copy* can be obtained, and this can be
converted back to an immutable object again by *copying*. The copying
makes sure that no aliasing/deep sharing occurs. They mention that in
future versions they hope to remove the actual copying by doing
liveness analysis.

The DSL-developer annotate effectful operations and indicate which
inputs are only read and which inputs can be aliased by the return
value of the operation.

##### Parallel IR
Most operations are specified in terms of a looping construct, that
seem similar to SaCs "with-loops", having both a map and reduction
phase.

##### Code-generation and runtime
In the code-generation stage the DSL-developer is allowed to override
code generation, and provide hand-optimized implementations
(e.g. through CUBLAS).

They compile the same operations to kernels that target multiple
different targets and defer hardware specific decisions to runtime.

The Delite runtime is responsible for scheduling kernels and
specializing to the hardware as necessary (they refer to this paper
about how they perform scheduling: "Walk-time techniques: catalyst for
architectural change" by Fisher). First this specialization is
performed and partial schedules of blocks is then produced, for those
parts where it can be done statically. These partial schedules are
then dispatch dynamically during execution (depending on branches taken).

For example they write: "a Reduce op only has its reduction function
generated by the compiler, and the runtime generates a tree-reduction
implementation with the tree height specialized to the number of
processors"


### Intermediate representation for heterogeneous multi-core: A survey
<http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=7050496&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D7050496>

We don't have access to this paper, but it seems very relevant. From
the abstract:

    "This article studies the most popular IR techniques for
    heterogeneous multi-core, classifies them into three broad
    categories and performs a comparison among them based on the data
    structure used and their importance in academia and research"

And a lot of papers that it references also seem relevant to look at:

 * "Automatic extraction of functional parallelism from ordinary programs"
   (also not available from Chalmers)

 * and more ...

### Kimble: a Hierarchical Intermediate Representation for Multi-Grain Parallelism
<http://nbenoit.tuxfamily.org/projects/gomet/wir11-kimble.pdf>

This is not a great paper. No example code or benchmarking. Not really
sure if this is implemented.

Graph based approach, a collection of DAGs are connected by nesting
relationships, and these nesting relationships can also point back to
form cycles (recursion). This graph is generated from a C-like
language (I think), though they never really mention the source
language explicitly.

Data-parallel loops are annotated by the user if they are maps or
reductions (OpenMP annotations I believe).

##### Node types:
 * Function (containing body as subgraph)
 * Loop (containing body as subgraph, annotated with "undividable", "map", "reduce")
 * Region ("basic block", containing clusters, statements and function calls)
 * Cluster (undividable sequence of statements)
 * Statements (three-address instruction)
 * Function call (with a nested function node, here recursion comes in)
 * Guard (conditional, with two subgraphs)


### EXOCHI
<http://web.cs.ucla.edu/~palsberg/course/cs239/papers/wang.pdf>

Check also the papers that references this one!

### Qilin

Parallel cost-model papers
--------------------------
<http://arxiv.org/pdf/1306.5076.pdf>

 * "A Practical Hierarchical Model of Parallel Computation" by Heywood and Ranka (Hierarchical PRAM)
 * "The Hierarchical Memory Machine Model for GPUs" by Koji Nakano
 * "A detailed GPU cache model based on Reuse Distance Theory" by Nugteren et al.
 * [Modeling parallel computers as memory hierarchies](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=315548&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D315548)
 * [LogP: A practical model of parallel computation](http://rsim.cs.uiuc.edu/arch/qual_papers/systems/5.pdf)
 * [A memory access model for highly-threaded many-core architectures](http://www1.cse.wustl.edu/~lin.ma/publications/ICPADS12.pdf)

### Simple Memory Machine Models for GPUs
by Koji Nakano, IPDPS'12 (IEEE)

This paper describes machine-models of GPU computing, that should make
it easier to reason about efficiency of parallel algorithms. The goal
is a theoretical model where you can reason about efficiency of
algorithms when considering memory coalescing and shared memory bank
conflicts.

He describes two models and evaluates how the big-O performance will
be on a few examples.

*DMM* (Discrete Memory Machine) is modelling the shared memory of
NVIDIA GPUs, where only a single simultaneous load to a memory bank
can be performed at a time, and thus simultaneous loads should be made
to different memory banks.

*UMM* (Unified Memory Machine) represents the global memory of GPUs,
where accesses are coalesced, and thus many contiguous addresses of
some block can read at once, but only one such block can be read at a
time.

The paper describes and evaluates the two models separately and a
hybrid approach is only mentioned as future work. Such a hybrid model
is necessary to make a realistic model for GPUs, and is discussed in
his next paper (as Hybrid Memory Machine).

There are also several papers describing efficient algorithms on the
DMM and UMM (e.g. string matching and an optimal parallel prefix-sum
algorithm), all with Koji Nakano as co-author.

### The Hierarchical Memory Machine Model for GPUs
by Koji Nakano, IPDPS'13 (IEEE)

DMM and UMM correspond to the computation using the shared memory and
the global memory of GPUs, respectively.

A Hierarchical Memory Machine (HMM), consists of *d* DMMs and a single
UMM (the figure they use to illustrate this, looks just like a diagram
of a NVIDIA GPU). The paper is mostly about analyzing a convolution
and a reduction algorithm running on this machine-model.

*Personal impression:* using this to analyse algorithms seems extremely
tedious, and even reading the asymptotic running times is hard,
because of the many parameters (memory bank width, latency, number of
processors and problem size). But this way of presenting bank
conflicts was good, and something like this should have been included
in the CUDA documentation.

### A memory access model for highly-threaded many-core architectures
<http://www1.cse.wustl.edu/~lin.ma/publications/ICPADS12.pdf>

High-level intermediate languages
---------------------------------

### Futhark

Futhark is intended as a intermediate language for high-performance
domain-specific languages e.g. a Monte Carlo-simulation + Automatic
Differentiation DSL or a Financial contract valuation DSL.

The Futhark project provides a rich language with a lot of work being
done on providing high-quality automatic fusion of their built-in
second-order array combinators, e.g. using the "redomap" construct
(MapReduce.)

Futhark is thus not low-level in the sense that you are in close
contact with the hardware, but low-level in the sence that is intended
to be generated by compilers, not written by hand.

### Single-assignment C

Many of the same things just mentioned about Futhark, can also be said
about Single-assignment C (SaC).

But in contrast to Futhark, which fusion-mechanism is built around a
lot of second-order data-parallel operators, and a restricted looping
construct, the fusion in SaC is performed on one single loop
construct, which all high-level operators are written in terms of.

### Bohrium

Bohrium is intended as a data-parallel bytecode, mainly for
interpreted languages such as Python, R or MATLAB. The idea is that
interpreters spit out bytecode instructions when they occur, and the
Bohrium runtime then buffers instructions and fuses as many as
possible before dispatching the computation in the GPU. In this way no
control-flow instructions are necessary in Bohrium bytecode, as only
the data-parallel instructions needs to be allocated. Currently only a
very limited set of instructions are implemented (e.g. no scan,
scatter or gather).

The Bohrium project targets MPI, OpenMP, CUDA and are starting doing
stuff on FPGAs.

### VCODE
The intermediate language used in NESL.

### CUDA PTX
This is what CUDA compiles to, so it might be too low-level to be
interesting, but maybe there's something to learn from it? I have
never looked at it in detail.

Intermediate languages for sequential compilers
-----------------------------------------------

### LLVM
Decompilation of LLVM IR <https://github.com/UplinkCoder/axtor> <http://compilers.cs.uni-saarland.de/publications/theses/moll_bsc.pdf>

### C-- (or Cmm, or C minus minus)

A language used as intermediate language in GHC. "C without type
declarations and pointers" + a runtime system for

"beamer-who-ya-gonna-call.pdf" and associated vimeo talk: <https://vimeo.com/69025829>
