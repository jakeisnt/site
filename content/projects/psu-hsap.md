+++
title = "Portland State University HSAP"
draft = true
+++

During a high-school summer, I was able to participate in an iteration of the
Department of Defense's High School Apprenticeship program through Portland
State University. Along with two other high school students, we explored a
variety of mathematics and programming paradigms pertaining to generating prime
numbers, working with superclusters, and partitioning sparse graphs through the
development of OpenMP-parallelized C++ libraries.


## Primes {#primes}

We began the internship with a warm-up activity of sorts: writing an efficient
algorithm to identify and display a number of accurate prime numbers in quick
succession; in other words, a prime sieve. We began with a naive implementation
of the Sieve of Erasnosthenes, an ancient Greek method developed to locate
primes in the first 100 numbers, then made certain optimizations such as
prematurely excluding all even numbers and all multiples of three from our prime
search for greater efficiency.


## Sparse Matrices {#sparse-matrices}

However, the primary objective of the program was to implement various graph
algorithms rooted in linear algebraic concepts to partition unweighted sparse
graphs. These are also known as networks with low connectivity, and are
trivially encoded as matrices with few '1' characters to represent a connection
between two nodes, as well as a majority of '0' characters to represent a lack
of a connection. Depicted below is an asymmetric graph featuring apples, bananas
and oranges as nodes, with the existence of an edge denoted by '1' and 0
otherwise.
An arbitrary explanation for such a graph is a supermarket in which an apple can
only be purchased after a carrot, a banana can be purchased after either a
banana or a carrot, and a carrot can be purchased after another carrot. It's
unclear how the initial fruit was obtained. Feel free to view these as arbitrary
'a', 'b' and 'c' nodes of a graph as well.

|        | apple | banana | carrot |
|--------|-------|--------|--------|
| apple  | 0     | 0      | 0      |
| banana | 0     | 1      | 0      |
| carrot | 1     | 1      | 1      |

The graph represented by such a matrix resembles the following:

```latex
$ the graph goes here$
```

To save a significant amount of space when working with large
graphs, these matrices can be represented in compressed-sparse row format
instead - denoting both the row in which the '1' occurs as well as the column
position. Such a matrix looks like the following:

This arrangement is able to save us x amount of space while avoiding any
increase in time complexity - the value of an element given its coordinate in
a non-compressed matrix can be determined with more operations, but is still a
constant-time operation with regards to the number of cells seen.


## Matrix Algorithms {#matrix-algorithms}


## Supercluster Usage {#supercluster-usage}
