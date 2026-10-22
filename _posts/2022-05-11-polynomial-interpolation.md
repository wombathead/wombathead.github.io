---
layout: post
title: Polynomial interpolation
date: 2022-05-11
lastEdited: 2026-10-16 16:00
---

{% include mathjax.html %}

Suppose I gave you the following sequence and asked you what the next term was:

$$ 1, 3, 73, 445, 1641, 4591, \ldots $$

How would you go about doing it?
In this note I will go through one way you might like to solve this problem, and then show that we can generalise it to finding a function that fits any number of points.

**Warm up.**
Let's start with looking at an easier sequence.
Take $$7, 11, 15, 19, 23, 27 \ldots$$. 
It's not too difficult to see that each term is four more than the previous one, so we might reasonably say that the next term should be $$31$$.
How about the sequence $$3, 11, 25, 45, 71, 103, \ldots$$?
Now there is not a common difference between the terms; instead we can see that the differences between terms are $$8$$, $$14$$, $$20$$, $$26$$, and $$32$$ respectively.
Notice that in this sequence, each term *does* have a common difference of six.
Therefore, the next term in this sequence should be $$32+6=38$$, and we can reasonably say that the next term in the original sequence is $$103+38=141$$.
Now what if I gave you another sequence that didn't have a common second-level difference, like the one at the top of the page?
We could look at the third, fourth, and fifth level differences and work our way back up to find the next term, but can we come up with a neater way to automate all of this work?

We can formalise the problem in the following way.
Suppose we are given the first $$n=k+1$$ terms of a sequence $$y_0, y_1, \ldots, y_k$$.
We want to find a function $$f(x)$$ such that for each $$i = 0, 1, \ldots, y_k$$ we have $$f(x) = y_i$$.
Let's start by thinking about how we would do it if we were only given two terms, and let's take the sequence at the top of the page as our running example.
As there are two terms we have $$y_0 = 1$$ and $$y_1 = 3$$, therefore we want to find the function $$f(x)$$ such that $$f(0) = 1$$ and $$f(1) = 3$$.
We can then use this to find $$f(2)$$.
With two points, it suffices to find a polynomial of degree at most one, meaning the function $$f$$ must take the form $$f(x) = c_0 + c_1 x$$ for some coefficients $$c_0$$ and $$c_1$$.
We want to find these coefficients.
Since $$f$$ must pass through $$y_0$$ and $$y_1$$, we have the following system of equations:

$$
\begin{align*}
f(0) &= 1 &= c_0 + 0 \cdot c_1 \\
f(1) &= 3 &= c_0 + 1 \cdot c_1
\end{align*}
$$

The solution to this pair of equations is straightforward, $$c_0 = 1$$ and $$c_1 = 2$$.
Therefore $$f(x) = 2 x + 1$$.
Using this to find the third term in the sequence gives us $$f(2) = 2(2) + 1 = 5$$.

Let's now suppose we are given the first three terms, $$y_0 = 1$$, $$y_1 = 3$$, and $$y_2 = 73$$.
For two terms it sufficed to find a polynomial of degree one; now with three terms $$f$$ must instead be a polynomial of degree at most two.
Therefore $$f(x) = c_0 + c_1 x + c_2 x^2$$.
Again, since $$f$$ must pass through each of the terms we are given then we must have $$f(0)=1$$, $$f(1)=3$$, and $$f(2)=73$$.
As before gives rise to a system of equations:

$$
\begin{align*}
f(0) = 1 &= c_0 + c_1 + c_2 \\
f(1) = 3 &= c_0 + 2 c_1 + 2^2 c_2 \\
f(2) = 73 &= c_0 + 3 c_1 + 3^2 c_2
\end{align*}
$$

Again, we can solve these equations to find the coefficients $$c_0$$, $$c_1$$, and $$c_2$$.
We can still do this by hand, but as we increase the number of terms this will get more complicated and we are more liable to make mistakes.
We will represent the equations in a more convenient form so that we can automate the process of finding the coefficients.
Let $$M$$ be a $$3 \times 3$$ matrix, and let $$c$$ and $$y$$ be vectors of length three containing the coefficents $$c_0$$, $$c_1$$, and $$c_2$$ (which we will be solving the system for) and the terms of the sequence $$y_0$$, $$y_1$$, and $$y_2$$ as follows:

$$
M =
\begin{bmatrix}
1 & 0 & 0 \\
1 & 1 & 1 \\
1 & 2 & 4
\end{bmatrix}, \quad
c = 
\begin{bmatrix}
c_0 \\
c_1 \\
c_2
\end{bmatrix}, \quad
y = 
\begin{bmatrix}
y_0 \\
y_1 \\
y_2
\end{bmatrix}
=
\begin{bmatrix}
1 \\
3 \\
73
\end{bmatrix}.
$$

We can now see that the system of three equations is represented by the matrix equation $$M c = y$$.
Our goal is to solve this equation to find the coefficients $$c$$.
To do so, we can find the inverse $$M^{-1}$$ of $$M$$, and then apply it to the left of $$y$$ to get $$c = M^{-1} y$$.
Let's find the inverse of $$M$$ using Gaussian elimination.
We first create an augmented matrix $$\tilde{M}$$ that has the vector $$y$$ spliced into its rightmost column, like so:

$$
\tilde{M} = 
\left[ \begin{array}{ccc|c}
1 & 0 & 0 & 1 \\
1 & 1 & 1 & 3 \\
1 & 2 & 4 & 73 
\end{array} \right]
$$

After performing the steps of Gaussian elimination, we are left with $$\tilde{M}$$ in reduced row echelon form:

$$
\tilde{M} = 
\left[ \begin{array}{ccc|c}
1 & 0 & 0 & 1 \\
1 & 1 & 1 & -32 \\
1 & 2 & 4 & 34 
\end{array} \right]
\implies 
c =
\begin{bmatrix}
1 \\
-32 \\
34
\end{bmatrix}.
$$

Therefore $$c = (1, -32, 34)$$, and the function $$f(x)$$ passing through each of the three terms is

$$ f(x) = 1 - 32 x + 34 x^2 . $$

We can then verify by hand that

$$
\begin{align*}
f(0) = 1 &= y_0 \\
f(1) = 3 &= y_1 \\
f(2) = 73 &= y_2
\end{align*}
$$

With this function, we can generate the next term $$f(3)$$ as

$$f(3) = 1 - 32(3) + 34(3^2) = 211 .$$

Let's now generalise this technique to accommodate any number of initial terms of the sequence.


## General method: systems of equations

We are given the first $$n=k+1$$ terms of a sequence $$y_0, y_1, \ldots, y_k$$ and we want to find a function $$f(x)$$ such that $$f(i) = y_i$$ for $$i = 0, 1, \ldots, k$$.
Since there are $$k+1$$ points that $$f$$ must pass through then $$f$$ must be a polynomial of degree at most $$k$$.
Therefore $$f$$ takes the form

$$ f(x) = c_0 + c_1 x + c_2 x^2 + \ldots + c_k x^k, $$

or more concisely, $$f(x) = \sum_{j=0}^k c_j x^j$$.
With $$n$$ initial terms, we have the following system of equations:

$$
\begin{align*}
f(0) &= c_0 ,\\
f(1) &= c_0 + c_1 + c_2 + c_3 + \ldots + c_k ,\\
f(2) &= c_0 + 2 c_1 + 4 c_2 + 8 c_3 \ldots + 2^k c_k ,\\
& \vdots \\
f(k) &= c_0 + k c_1 + k^2 c_2 + k^3 c_3 + \ldots + k^k c_k .
\end{align*}
$$

Notice that each equation is linear in the coefficients $$c_0, c_1, \ldots, c_k$$.
Also notice that the left hand side of the equation on the $$i$$th line is given by $$\sum_{j=0}^k c_j \cdot i^j$$ for $$i \in \{0, 1, \ldots, k \}$$.
Just as in the three term example, we can now represent this system of equations by the $$n \times n$$ matrix $$M$$ whose entry in the $$i$$th row and $$j$$th column is given by $$M_{ij} = i^j$$.
Again introduce the vectors $$c = (c_0, c_1, \ldots, c_k)$$ and $$y = (y_0, y_1, \ldots, y_k)$$.
The system of equations can be represented by the matrix equation $$M c = y$$, and therefore the solution $$c = M^{-1} y$$ is given by:

$$
c = M^{-1} y =
\begin{bmatrix}
    1 & 0 & 0 & 0 & \ldots & 0 \\
    1 & 1 & 1 & 1 & \ldots & 1 \\
    1 & 2 & 4 & 8 & \ldots & 2^k \\
    1 & 3 & 9 & 27 & \ldots & 3^k \\
    \vdots & \vdots & \vdots & \vdots & \vdots \\
    1 & k & k^2 & k^3 & \ldots & k^k 
\end{bmatrix}^{-1}
\begin{bmatrix}
    y_0 \\
    y_1 \\
    y_2 \\
    y_3 \\
    \vdots \\
    y_k
\end{bmatrix}
$$

And that's all there is to it.
Once we have generated the matrix $$M$$ and found its inverse $$M^{-1}$$, we simply apply it to the vector $$y$$ and we have found our coefficients $$c_0, \ldots, c_k$$.
The resulting function $$f(x) = \sum_{j=0}^k c_j x^j$$ passes through each $$y_i$$ at $$i$$ for $$i \in \{0,1,\ldots,k\}$$.


### Going from sequences to points

We can take this further!
With a very simple modification, we can extend our method to finding a function that not only generates the terms of a sequence, but also passes through any initial set of two dimensional points.
Now suppose we are given as input the set of points $$P = \{ (x_0,y_0), (x_1,y_1), \ldots, (x_k,y_k) \}$$.
Again our goal is to design the function $$f(x) = \sum_{j=0}^k c_j x^j$$, but now we will need $$f(x_i) = y_i$$ for each $$i \in \{0,1,\ldots,k\}$$.
Therefore our system of equations instead becomes

$$
\begin{align*}
c_0 + c_1 x_0 + c_2 x_0^2 + \ldots + c_k x_0^k &= f(x_0) ,\\
c_0 + c_1 x_1 + c_2 x_1^2 + \ldots + c_k x_1^k &= f(x_1) ,\\
c_0 + c_1 x_2 + c_2 x_1^2 + \ldots + c_k x_1^k &= f(x_2) ,\\
\vdots \\
c_0 + c_1 x_k + c_2 x_k^2 + \ldots + c_k x_k^k &= f(x_k) .
\end{align*}
$$

As before, we decompose the system of linear equations into a matrix equation $$M c = y$$ as follows:

$$
M = 
\begin{bmatrix}
1 & x_0 & x_0^2 & \ldots & x_0^k \\
1 & x_1 & x_1^2 & \ldots & x_1^k \\
\vdots & \vdots & \vdots & \vdots & \vdots \\
1 & x_k & x_k^2 & \ldots & x_k^k
\end{bmatrix}
\begin{bmatrix}
c_0 \\
c_1 \\
\vdots \\
c_k
\end{bmatrix}
=
\begin{bmatrix}
y_0 \\
y_1 \\
\vdots \\
y_k
\end{bmatrix},
$$

and we can solve the system again by simply taking the inverse of $$M^{-1}$$ and applying it to $$y$$.
This gives us the values of the coefficients $$c_0, \ldots, c_k$$ and therefore gives us our polynomial $$f(x)$$ of degree $$k$$ passing through each of the points in $$P$$.
Let's now look at another way of solving the same problem known as Lagrange interpolation.


## Lagrange interpolation

So we've got our set of input points $$P = \{ (x_i,y_i) \}_{i \in \{ 0,1,\ldots,k \} }$$ and we still want to build a polynomial of degree at most $$k$$ that passes through all of them.
Can we go about it in a better way?
The idea behind Lagrange interpolation is to build a set of basis polynomials, one for each $$x_i$$, such that their weighted sum passes through all the points.
First consider the quantity $$P_i(x) = \prod_{j \neq i} (x-x_i)$$ and notice that it has $$k$$ roots $$\{ x_i \}$$.
If we now construct the basis polynomial $$\ell_i(x)$$ by defining

$$ \ell_i(x) = \frac{ P_i(x) }{ \prod_{j \neq i} (x_j - x_i) ,$$

then for every $$x_i$$ we have $$\ell_i(x_i) = 1$$, since $$P_i(x_i)$$ is simply equal to the denominator.
Likewise, for any $$x_j \neq x_i$$ we have $$\ell_i(x_j) = 0$$.
This is because although $$\ell_i(x)$$ skips over what would be the term $$\frac{(x-x_i)}{(x_i-x_i)}$$ to prevent division by zero, it does not skip over $$\frac{(x-x_j)}{(x_i-x_j)}$$ for any $$x_j$$, hence when $$x = x_j$$ then $$\ell_i(x_j)$$ contains the term $$\frac{(x_j-x_j)}{(x_i-x_j)}$$, sending the whole of $$\ell_i(x_j)$$ to zero.
If we take our interpolation function as

$$ L(x) = \sum_{i=0}^k y_i \ell_i(x) ,$$

then we can see that for all $$x_i$$ we have

$$
\begin{align*}
    L(x_i) &= \sum_{j \neq i}^k y_j \ell_j(x_i) \\
           &= y_i \ell_i(x_i) + \sum_{j \neq i} y_j \ell_j(x_i) \\
           &= y_i ,
\end{align*}
$$

just as we want.
Therefore $$L(x)$$ is a polynomial of degree at most $$k$$ that passes through each of the points in $$P$$!

**Example.**
Let's apply Lagrange interpolation to our previous three term example.
We have the points the terms of the sequence $$1, 3, 73, \ldots$$, which we can model as the points $$\{ (x_0, y_0), (x_1, y_1), (x_2, y_2) \} = \{ (0, 1), (1, 3), (2, 73) \}$$.
Applying the definitions of the Lagrange basis polynomials gives us the following:

$$
\begin{align*}
\ell_0(x) &= \frac{(x-x_1)}{(x_0-x_1)} \frac{(x-x_2)}{(x_0-x_2)} \\
          &= \frac{(x-1)(x-2)}{(-1) (-2)} \\
          &= \frac{1}{2} (x-1)(x-2) \\
\ell_1(x) &= \frac{(x-0)(x-2)}{(1)(-1)} \\
          &= -x (x-2) \\
\ell_2(x) &= \frac{(x-0)(x-1)}{(2)(1)} \\
          &= \frac{1}{2} x(x-1)
\end{align*}
$$

Therefore the Lagrange interpolating polynomial $$L(x) = \sum_i y_i \ell_i(x)$$ is simply

$$
\begin{align*}
L(x) &= y_0 \ell_0(x) + y_1 \ell_1(x) + y_2 \ell_2(x) \\
     &= \left( 1 \cdot \frac{1}{2} (x-1)(x-2) \right) + \left( 3 \cdot -x (x-2) \right) + \left( 73 \cdot \frac{1}{2} x (x-1) \right) \\
     &= \frac{1}{2} (x-1)(x-2) - 3x(x-2) + \frac{73}{2} x (x-1)
\end{align*}
$$

This simplifies to $$L(x) = 34 x^2 - 32 x + 1$$, which is exactly what we got by solving our system of equations before.

