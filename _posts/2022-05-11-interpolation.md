---
layout: post
title: An exercise in interpolating data points
date: 2022-05-11
lastEdited: 2022-05-22 21:00
---

{% include mathjax.html %}

Suppose I gave you the following sequence and asked you to give me the next term.

$$ 4, 17, 46, 97, 176, 289, \ldots $$

How would you go about doing it?
In this note I will go over two ways to come up with a function that fits all of the terms in a given sequence, and can then be used to give the next term in the sequence.


## Warm up

Let's start by looking at an easier sequence.

$$ 7, 11, 15, 19, \ldots $$

It's pretty easy to see that we are just going up in fours starting from $$7$$, so the next term after $$19$$ should be $$23$$.
Now consider the next sequence.

$$ 3, 11, 25, 45, \ldots $$

The difference between the first two terms is $$8$$, then $$14$$, then $$20$$.
This forms a sequence itself, and we can see that each term in this sequence is $$6$$ more than the previous term.
The next term in this subsequence is therefore $$26$$, and we just need to add this to $$45$$ to find that the next term in the original sequence is $$71$$.
As a final example, take the following sequence.

$$ 8, 27, 76, 167, \ldots $$

We can repeat all the same reasoning as before, finding the first-level, second-level, and eventually third-level sequences.
Eventually with a bit of work we would find that the next term in the sequence would be $$312$$.
How can we automate this process to find an explicit term for the $$n$$th of the sequence?


## Method 1: systems of equations

Let's back up slightly and introduce some notation.
In our problem we are presented with a set of $$n$$ observations or "nodes", and we want to come up with a function $$f(x)$$ that agrees with the nodes for $$x = 1, 2, \ldots, n$$.
Let $$(y_n) = (y_1, y_2, \ldots, y_n)$$ denote the nodes.
What can we say about $$f(x)$$ that will allow us to derive an explicit form for the function.


### Example

Let's start by assuming we have only two nodes, $$y_1$$ and $$y_2$$, and suppose these are equal to $$8$$ and $$27$$.
The simplest function passing through both of these points is a line, and it is straightforward to verify that the equation of this line is given by $$19 x - 11$$.
This is found easily by observing that the common difference between the terms is $$19$$, so $$f(x)$$ must be of the form $$f(x) = 19 x + \alpha$$ for some number $$\alpha$$.
In order for $$f(1)$$ to equal $$8$$ as required we require $$\alpha = -11$$, hence $$f(x) = 19 x - 11$$.

Now suppose we have three nodes, $$(y_1, y_2, y_3) = (8, 27, 76)$$.
We can no longer come up with a line that passes through all three nodes, since the terms no longer all share a common first-level difference.
However, we can construct a quadratic curve that passes through each node.
Since $$f$$ is quadratic then we know it must have the form $$f(x) = c_1 + c_2 x + c_3 x^2$$ for some coefficients $$c_1, c_2, c_3$$.
For $$f$$ to pass through each node then we need $$f(1) = 8$$, $$f(2) = 27$$, and $$f(3) = 76$$.
In other words, $$f(x) = u_x$$ for $$x \in \{ 1, 2, 3 \}$$.
Note that $$f(1) = c_1 + c_2 (1) + c_3 (1)^2$$, $$f(2) = c_1 + c_2 (2) + c_3 (2)^2$$, and $$f(3) = c_1 + c_2 (3) + c_3 (3)^2$$.
Therefore, writing the equations out explicitly gives us the following.

$$
\begin{align*}
c_1 +   c_2 +   c_3 &= 8, \\
c_1 + 2 c_2 + 4 c_3 &= 27, \\
c_1 + 3 c_2 + 9 c_3 &= 76.
\end{align*}
$$

Notice that with $$n = 3$$ nodes, we have $$n$$ equations in $$n$$ variables.
Let's start by writing the system out more succinctly as a matrix equation.
Define $$M$$, $$c$$, and $$y$$ as follows.

$$
M =
\begin{bmatrix}
1 & 1 & 1 \\
1 & 2 & 4 \\
1 & 3 & 9
\end{bmatrix}, \quad
c = 
\begin{bmatrix}
c_1 \\
c_2 \\
c_3
\end{bmatrix}, \quad
y = 
\begin{bmatrix}
8 \\
27 \\
76
\end{bmatrix}.
$$

The system of equations can now instead be written $$M c = y$$.
We want to find the values of the coefficients $$c_1$$, $$c_2$$, and $$c_3$$ that will satisfy the system.
This is simply given by $$c = M^{-1} y$$, meaning all we have to do to find the coefficients is construct $$M$$, find its inverse, and apply it to $$y$$.
We can do this using [Gaussian Elimination](https://en.wikipedia.org/wiki/Gaussian_elimination).
We first want to create an augmented matrix $$\tilde{M}$$ that has the vector $$y$$ spliced into the rightmost column of $$M$$, like so:

$$
\tilde{M} = 
\left[ \begin{array}{ccc|c}
1 & 1 & 1 & 8 \\
1 & 2 & 4 & 27 \\
1 & 3 & 9 & 76 
\end{array} \right]
$$

After performing the steps of the algorithm we get the augmented matrix in reduced row echelon form as:

$$
\tilde{M} = 
\left[ \begin{array}{ccc|c}
1 & 0 & 0 & 19 \\
0 & 1 & 0 & -26 \\
0 & 0 & 1 & 15 
\end{array} \right] \implies
c = \begin{bmatrix}
19 \\ -26 \\ 15
\end{bmatrix}
$$

Therefore we have found that $$c = \begin{pmatrix} c_1 & c_2 & c_3 \end{pmatrix} = \begin{pmatrix} 19 & -26 & 15 \end{pmatrix}$$.
Our quadratic function $$f(x)$$ is therefore given by

$$ f(x) = c_1 + c_2 x + c_3 x^2 = 19 - 26 x + 15 x^2. $$

We can then verify that

$$
\begin{align*}
f(1) = 19 - 26(1) + 15(1) = 8 &= y_1, \\
f(2) = 19 - 26(2) + 15(2^2) = 27 &= y_2, \\
f(3) = 19 - 26(3) + 15(3^2) = 76 &= y_3.
\end{align*}
$$

So we've found the function $$f(x)$$ that passes through all our nodes!
The next term in the sequence under $$f$$ is $$f(4) = 155$$.


### Generalising the method

We have $$n$$ nodes $$(y_1, y_2, \ldots, y_n)$$, and for convenience suppose $$n = k+1$$.
To find a function passing through each of the nodes it suffices to find a polynomial function $$f(x)$$ of degree $$k$$.
In other words, $$f$$ takes the form

$$
\begin{align*}
f(x) &= c_1 + c_2 x + c_3 x^2 + \ldots + c_k x^{k-1} + c_{k+1} x^k \\
     &= \sum_{i=1}^{n} c_i x^{i-1}
\end{align*}
$$

Since $$f$$ must pass through each of the nodes, then this gives rise to the system of $$n$$ linear constraints $$\{ f(j) = y_j \}_{j \in [n]}$$, or, written explicitly:

$$
\begin{align*}
c_1 +   c_2 +   c_3 \ldots c_n &= y_1, \\
c_1 + 2 c_2 + 4 c_3 \ldots 2^k c_n &= y_2, \\
&\vdots \\
\sum_{i=1}^n j^{i-1} c_i &= y_j, \\
&\vdots \\
c_1 + n c_2 + n^2 c_3 \ldots n^k c_n &= y_n.
\end{align*}
$$

We then decompose this system of equations into the matrix product $$M c = y$$, where $$c = \begin{pmatrix} c_1 & \ldots & c_n \end{pmatrix}$$, $$y = \begin{pmatrix} y_1 & \ldots & y_n \end{pmatrix}$$, and $$M$$ is an $$n \times n$$ matrix where $$M_{ij} = j^{i-1}$$.
We solve this system by taking the inverse of $$M$$ and applying it to $$y$$, giving us the coefficients $$c = M^{-1} y$$, and we are done!


[gaussian-elimination]: https://en.wikipedia.org/wiki/Gaussian_elimination
