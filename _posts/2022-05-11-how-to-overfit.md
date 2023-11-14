---
layout: post
title: How to Overfit
date: 2022-05-11
lastEdited: 2022-05-22 21:00
---

{% include mathjax.html %}

What's the next term in the sequence?

$$ 4, 17, 46, 97, 176, 289 $$

If we don't know the function that generates the sequence then it could be anything.
But can we construct a function that agrees with each of the terms given and gives us an idea of what the next term could be?

## The Setup

Suppose there is some function $$f : \mathbb{N} \to \mathbb{R}$$ that generates the $$n$$th term of our mystery sequence, but we only have access to the first $$k$$ terms of the sequence, i.e., $$(u_1,u_2,\ldots,u_k)$$.
We will guess the next term of the sequence by constructing a function $$g_k : \mathbb{N} \to \mathbb{R}$$ based on the first $$k$$ observations of the sequence such that $$g_k(n) = u_n$$ for all $$n \in [k] = \{ 1, \ldots, k \}$$.
If $$g_k(n)$$ agrees with $$u_n$$ for any $$n > k$$ then this is a bonus.

We will illustrate the approach by imagining how we would come up with the next terms in the sequence above, for $$1 \le k \le 6$$.
Let's see how the value of $$k$$ affects our ability to correctly guess the terms of the sequence that we know.
- $$k=1$$: All we are given is the sequence $$(4)$$. Since we have so little to go off then all we can do is assume that $$u_n$$ is always $$4$$, hence our guess would be $$g_1(n) = 4$$ for all $$n$$.
- $$k=2$$: We are given the samples $$(4,17)$$. In this case we would assume there is a single common difference of $$13$$ between each term and guess the next term to be $$17+13=30$$. For any $$n$$ we would guess the $$n$$th term to be $$g_2(n) = 13n - 9$$,[^1] which agrees with the samples we are given since $$g_1(1) = 4$$ and $$g_1(2) = 17$$. With two samples we assume there is a linear relationship between terms.
- $$k=3$$: We get the samples $$(4,17,46)$$. Now we assume there is a common second-level difference between the terms: we can see that the first and second term differ by 13 (as before) and that the second and third differ by 29, so now we assume that the difference between the third term and the fourth term will be $$29+(29-13) = 45$$. Our prediction for the fourth term is $$46 + 45 = 91$$. Since we consider *two* differences (first-order and second-order) we assume the terms have a quadratic relationship.

We can repeat this for increasing values of $$k$$ up to $$k=6$$.
We've seen that $$g_2(n) = 13n - 9$$, but how do we generate an explicit representation of the guess functions for greater values of $$k$$?

## Linear systems of equations

Given $$k$$ terms of a sequence we want to come up a function that agrees with the sequence at $$k$$ points.
The "simplest" function passing through two points is a straight line (or a polynomial of degree one) and we can extend this to a higher number of points and see that $$k$$ points suffice to define a polynomial of degree $$k-1$$.
So to come up with a "simple" guess function $$g_k(n)$$ that agrees with the $$k$$ terms we are given we just need to find a polynomial of the form:

$$ g_k(n) = x_{k-1} n^{k-1} + \ldots + x_1 n + x_0 $$

for some coefficients $$x_{k-1},\ldots,x_1,x_0$$.
How do we work out the values $$x_{k-1},\ldots,x_0$$?
We can deduce them using the terms we already know!
First suppose that $$k=2$$.
Since $$g_k$$ must agree with the terms we already have then $$g_2(n) = u_n$$ for all $$n \in \{ 1,2 \}$$.
Moreover from the reasoning above $$g_2(n)$$ takes the form $$g_2(n) = x_1 n + x_0$$.
Substituting in the appropriate values for $$n$$ we get the following constraints:

$$
\begin{align*}
x_0 + x_1 &= u_1, \\
x_0 + 2 x_1 &= u_2.
\end{align*}
$$

There are two unknowns are two equations meaning we can solve them to find the right values for $$x_1$$ and $$x_0$$.
In the example above we get:

$$
\begin{align*}
x_0 + x_1 &= 4, \\
x_0 + 2 x_1 &= 17.
\end{align*}
$$

Solving these equations gives us $$x_1 = 13$$ and $$x_0 = -9$$.
Therefore $$g_2(n) = 13n - 9$$ as we had previously worked out.
Now let $$k=3$$ and we get the following system of three equations:

$$
\begin{align*}
x_0 + x_1 + x_2 &= 4, \\
x_0 + 2 x_1 + 4 x_2 &= 17, \\
x_0 + 3 x_1 + 9 x_2 &= 46.
\end{align*}
$$

Now, we could solve this like we did for the two-equation case but I find it gets much easier to make mistakes when trying to do it "manually".
Instead, let's solve them using linear algebra.
Let's begin by being more concise and rewriting our system of equations using matrices.
We will introduce a matrix $$A$$ that will represent the coefficents of the variables $$x_2,x_1,x_0$$ and a vector $$b$$ that will represent the terms of the sequence $$u_2,u_1,u_0$$.
Denoting the variables as a vector $$x$$ we get for the example above:

$$
\begin{gather}
A =
\begin{bmatrix}
1 & 1 & 1 \\
1 & 2 & 4 \\
1 & 3 & 9
\end{bmatrix}, \quad
x = 
\begin{bmatrix}
x_0 \\
x_1 \\
x_2
\end{bmatrix}, \quad
b = 
\begin{bmatrix}
4 \\
17 \\
46
\end{bmatrix}.
\end{gather}
$$

Therefore the system of equations can be succinctly represented as $$Ax = b$$.
We want to find the vector $$x$$ that satisfies this equation, and to do so we must find the inverse of $$A$$ to get $$x = A^{-1} b$$.
Since our system of equations is in a convenient format, i.e.
represented by matrices, we can solve them using [Gaussian Elimination][gaussian-elimination].
We want to first create the augmented matrix that has the vector $$b$$ spliced into the rightmost column, like so:

$$
\left[ \begin{array}{ccc|c}
1 & 1 & 1 & 4 \\
1 & 2 & 4 & 17 \\
1 & 3 & 9 & 46 
\end{array} \right]
$$

After performing the steps of the algorithm we get the augmented matrix in reduced row echelon form as:

$$
\left[ \begin{array}{ccc|c}
1 & 0 & 0 & 7 \\
0 & 1 & 0 & -11 \\
0 & 0 & 1 & 8 
\end{array} \right] \implies
x = \begin{bmatrix}
7 \\ -11 \\ 8
\end{bmatrix}
$$

and now we can read off the solution $$x$$ to the equation $$Ax = b$$ as simply the rightmost column in the resulting matrix above.
Thus $$x = \begin{bmatrix} 7 & -11 & 8 \end{bmatrix}$$ or in other words, $$x_0 = 7, x_1 = -11, x_2 = 8$$, so we have constructed the guess function $$g_3(n) = 8n^2 - 11n + 7$$.
We can then verify that:

$$
\begin{align*}
g_3(1) = 4  & = u_1, \\
g_3(2) = 17 & = u_2, \\
g_3(3) = 46 & = u_3.
\end{align*}
$$

The matrix $$A$$ of coefficients for the system of equations we had above was specific to that particular instance: if we wanted to come up with a guess function given access to $$k=4$$ samples then we would need to use another system of equations and hence another matrix $$A$$.
However it is simple to generate this matrix if we know how many samples we get.
Note that row $$i$$ in the matrix $$A$$ represents the $$i$$th term of the sequence, while column $$j$$ represents the coefficients of the variable $$x_{j-1}$$.[^2] Therefore for some value of $$k$$ we will have a $$k \times k$$ matrix representing the coefficients since we have $$k$$ terms and we need at most $$k$$ variables to approximate it.
If we look at the expression for $$g_k(n)$$ again we can see that the coefficient for the $$j$$th variable in the $$i$$th term is $$i^{j}$$ for $$i \in [k]$$ and $$j \in \{ 0,\ldots,k-1 \}$$.
In other words, given $$k$$ samples of the sequence we construct the matrix of coefficients $$A \in \mathbb{R}^{k \times k}$$ as:

$$ a_{ij} = i^{(j-1)} \quad \text{for $i,j \in [k]$} $$

Using this relationship to generate the matrix $$A$$ for any value of $$k$$ we therefore have all we need to generate the guess $$g_k$$ for any sequence given access to $$k$$ samples.
The entire process for any mystery sequence is therefore as follows:
1. set up a system of $$k$$ linear equations given the $$k$$ terms of the sequence $$(u_1,u_2,\ldots,u_k)$$
2. solve this system of equations: generate the matrix of coefficients $$A$$, represent the system as $$Ax = b$$, then use Gaussian elimination to find $$x_{k-1},\ldots,x_1,x_0$$
3. output the function $$g_k(n) = \sum_{i=0}^{k-1} x_i n^i$$ to approximate $$u_n$$

## Wrap up

So what was our elusive mystery sequence $$(4,17,46,97,176,289)$$?
We can repeat the process described above for the values $$k=4,5$$ -- there is no need to go any higher since there are only six terms -- and we find that the values of the variables $$x$$ in both cases are:

$$
\begin{bmatrix}
x_0 \\
x_1 \\
x_2 \\
x_3 \\
x_4 \\
x_5
\end{bmatrix} =
\begin{bmatrix}
1 \\
0 \\
2 \\
1 \\
0 \\
0
\end{bmatrix} \implies g_k(n) = n^3 + 2 n^2 + 1
$$

This is good, as the function I had in mind when writing down the sequence at the top of the page was exactly $$f(n) = n^3 + 2 n^2 + 1$$.
So given the first $$k$$ terms of any sequence we now have a way to exactly generate the first $$k$$ terms using a polynomial of degree at most $$k-1$$.
Of course, if there is no restriction on the true generating function $$f(n)$$ then we will never truly replicate it with $$g_k(n)$$ for all values of $$n$$, and the difference between the true and guessed terms might become arbitrarily bad -- what if, for example, we had $$f(n) = 2^n$$?
That grows too quickly for any polynomial and so we could never get near the true values of the sequence using $$g_k$$ for large values of $$n$$.
Still, a fun problem that was fun to code too.

## Footnotes

[^1]: We first look at how we could have arrived at $$u_2$$ from $$u_1$$ and see that $$17 = 4 + 13$$, so $$u_2 = u_1 + 13$$. Then for subsequent terms we assume that $$u_n = u_{n-1} + 13$$, which we can rewrite as $$u_n = u_{n-1} + 13 = u_{n-2} + 2(13) = \ldots = u_0 + 13n$$. How do we know what $$u_0$$ is?  Simple: if $$u_n = u_{n-1} + 13$$ then $$u_1 = u_0 + 13$$, so $$u_0 = 4 - 13 = -9$$.  Thus we have $$u_n = 13n - 9$$.

[^2]: The index here, $$x_{j-1}$$, is a bit weird because of how I've numbered the various parts of this problem: terms of the sequence $$(u_n)$$ start from $$u_1$$ but the variables we play with are $$x_0,x_1,\ldots$$ so there is a mismatch in that the sequence indices start from 1 while the variable indices start from 0.

[gaussian-elimination]: https://en.wikipedia.org/wiki/Gaussian_elimination
