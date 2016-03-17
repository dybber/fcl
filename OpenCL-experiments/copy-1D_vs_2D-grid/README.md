In this experiment we want to measure the difference between copying
a 2D-array using two different approaches to indexing:

 * 1D-indexing, launching a 1D-grid
 * 2D-indexing, launching a 2D-grid

We implement two versions of each case, one using shared memory, and
one not using shared memory.

(Maybe it would also be interesting to do 2D-indexing, but launching a
1D-grid and computing the 2D indices ourselves.)
