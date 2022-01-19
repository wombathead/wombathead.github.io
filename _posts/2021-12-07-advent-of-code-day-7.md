---
layout: post
title: "Advent of Code 2021 -- Day 7: The Treachery of Whales"
date: 2021-12-07
lastEdited: 2022-01-19 18:30
---

{% include mathjax.html %}

Having dealt with the lanternfish we now set to work to escape a potentially
grisly encounter with a whale. For various plot reasons we have to align a
collection of crab submarines -- which can obviously only move horizontally --
onto a single point. The submarines incur a cost for moving from its starting
position to the alignment point, so we want to find such a point that minimises
the total cost incurred by the crabs.

I found the two problems easy to solve, but after some consideration I noticed
that while I got the correct answers quickly they were actually very
inefficient. It makes for a nice discussion and learning point, so I will first
introduce some notation then proceed by discussing my solutions to the puzzles
and how they changed.

Our puzzle's input is a list $$x_1,\ldots,x_n$$ of the crab submarines'
starting positions. We want to find a point $$y$$ on which to align the crabs,
and on placing the alignment point at $$y$$ then crab $$i$$ will incur a cost
$$c_i(x_i,y)$$ to move from his starting position to that point. For the first
part we are told that each crab incurs the cost $$c_i(x_i,y) = |x_i-y|$ for
placing the point at $$y$$. In other words, a crab's cost is simply the
distance he must move from his starting position to the alignment point. The
second part slightly altered the crabs' cost functions, but we will talk about
that in due course.

We will denote the cost for choosing alignment point $$y$$ as $$c(y)$$, and for
both parts we are looking to choose a point that minimises this value. We will
denote the optimal alignment point as $$y^* = \arg \min_y c(y)$$. In our case
the value $$c(y)$$ is simply the sum of costs for each individual crab, so
$$c(y) = \sum_i c_i(x_i,y)$$, but note that this need not necessarily have been
the case: we might have wanted to minimise the maximum cost, minimise the
average cost, etc.

Now let's discuss the solutions.

One way to minimise our objective function $$c(y)$$, whatever it is, is to
iterate over all possible solutions and simply keep the point $$y$$ that
minimises cost, and this is the way I solved both parts initially. That is, for
each point $$y$$ between the minimum and maximum crab starting positions given
in the input, calculate its cost $$c(y)$$, and keep it if it is smaller than
the smallest value we currently have stored. The problem with this is that it
is *inefficient*, and in fact takes exponential time in the size of the input,
since if numbers are represented by $$b$$ bits then we have to check $$2^b$$
numbers in the worst case. I will now argue for a better method of computing
the optimal alignment point $$y^*$$ with the following claim.

**Claim:** Any point $$y$$ in the interval $$[m_1,m_2]$$, where $$m_1,m_2$$ are
the median values in the input, is an optimal alignment point.

**Proof:** First observe that placing the point at $$m_1$$ or $$m_2$$ results
in the same cost. This is trivial when $$n$$ is odd since there is only one
median point, so suppose $$n$$ is even. Let $$L(y)$$ denote the sum of costs of
all points in the input less than $$y$$, and $$R(y)$$ denote the analogous
value for all points greater than $$y$$. Thus the cost of aligning at $$y$$ is
$$c(y)=L(y)+R(y)$$. We want to show $$c(m_1)=c(m_2)$$, or
$$L(m_1)+R(m_1)=L(m_2)+R(m_2)$$. Since $$m_1$$ and $$m_2$$ are adjacent
points then we may rewrite $$L(m_2)$$ as
$$L(m_2)=L(m_1)+\frac{n}{2}(m_2-m_1)$$, where the second term follows from the
fact that there are $$\frac{n}{2}$$ points to the left of $$m_2$$ since it is
the median, each of whom must travel the extra distance of $$(m_2-m_1)$$ to
reach $$m_2$$ from $$m_1$$. Similarly we may rewrite $$R(m_2)$$ as
$$R(m_2)=R(m_1)-\frac{n}{2}(m_2-m_1)$$ since $$m_2$$ is $$(m_2-m_1)$$ closer to
the $$n/2$$ crabs on the right of $$m_2$$. Thus

$$
\begin{align}
c(m_2) & = L(m_2) + R(m_2) \\
& = L(m_1) + \frac{n}{2}(m_2-m_1) + R(m_1) - \frac{n}{2}(m_2-m_1) \\
& = L(m_1) + R(m_2)
$$

Moreover, any point $$y \in [m_1,m_2]$$ has the same cost as placing the point
at either median point. Since the number of points to the left of $$m_1$$ is
equal to the number of points to the right[^1] of $$m_2$$ then any reduction in
cost we achieve from moving $$y$$ towards $$m_2$$ will be met with an cost as
we move away from points to the left of (and including) $$m_1$$. Performing the
same tricks as before, for any $$y \in [m_1,m_2]$$:

$$
\begin{align}
c(y) & = L(y) + R(y) \\
& = L(m_1) + \frac{n}{2}(y-m_1) + R(m_1) - \frac{n}{2}(m_2-y) \\
& = c(m_1)
$$

It remains to show that $$c(y)$$ is optimal. Suppose we place $$y$$ outside
$$[m_1,m_2]$$, and without loss of generality (we can follow a symmetric
argument for the other case) let $$y > m_2$$. 

$$
\begin{align}
c(y) & = c(m_2)+(1+\frac{n}{2})(y-m_2)-\frac{n}{2}(y-m_2) \\
& = c(m_2)+(y-m_2) \\
& > c(m_2)
\end{align}
$$

So placing the alignment point outside the interval $$[m_1,m_2]$$ results in a
strictly larger total cost.

The entire code for the first part is therefore:

```lisp
(flet ((total-cost (position numbers)
         "Sum of absolute differences between POSITION and NUMBERS"
         (loop for n in numbers
               sum (abs (- position n)))))
  (let* ((input (get-numbers filename))
         (y (median input)))
    (total-cost y input))) 
```

The second part changed the crabs' cost functions, so instead of being the
absolute difference between $$y$$ and $$x_i$$ it was now 1 if the crab was 1
away from $$y$$, 1+2 if the crab was 2 away from $$y$$, 1+2+3 if it was 3 away
from $$y$$, and so on. In other words, with $$d=|x_i-y|$$, when placing the
alignment point at $$y$$ each crab $$i$$ now incurs the cost

$$
c_i(x_i,y) = \sum_{k=1}^d k = \frac{d(d+1)}{2}
$$

I currently don't have a better solution than the brute force one -- that is,
try all possible positions between the minimum and maximum starting position
and return the one that results in the smallest total cost. The code
implementing this is given below:

```lisp
(flet ((total-cost (position numbers)
         (loop for n in numbers
               for difference = (abs (- position n))
               sum (/ (* difference (1+ difference)) 2))))
  (loop with input = (get-numbers filename) 
        for i from (reduce #'min input) upto (reduce #'max input)
        minimize (total-cost i input))) 
```

As I've mentioned neither part of this puzzle was difficult to get right, since
the brute force solution is simple to understand and implement. The low
difficulty was not lost on the community as despite this being the quickest I
had solved both parts of a problem it has not led to my highest ranking in the
leaderboard. It was an interesting exercise to prove the median points are
optimal in the first part; now it remains to improve the efficiency of my
solution for the second part.

[^1]: If we write out the starting positions as a list as opposed to thinking
  about them on a number line.

