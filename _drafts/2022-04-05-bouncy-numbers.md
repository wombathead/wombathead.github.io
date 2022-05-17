---
layout: post
title: Bouncy numbers
date: 2022-04-05
lastEdited: 2022-04-05 22:30
---

{% include mathjax.html %}

Just a quick note about a nice bit of optimisation I did recently as part of a
programming puzzle. I don't want to be too specific so I don't spoil the fun
for whoever is reading this.

Let $$n = \sum_{k=0}^m d_k b^k$$ be any positive number, where $$m=\lfloor
{\log_b n} \rfloor$$. Then the sequence $$(d_m,d_{m-1},\ldots,d_1,d_0)$$
corresponds to writing out this number in base $$b$$. The $$b$$-digits of $$n$$
are said to be increasing if $$d_i \le d_{i-1}$$ for $$i \in [m]$$, and
decreasing if $$d_i \ge d_{i-1}$$ for $$i \in [m]$$. For example in base 10 the
number 13448 is increasing while 97622 is decreasing. If the $$b$$-digits are
neither increasing nor decreasing then the number is said to be bouncy.

Let $$B(n)$$ be the set of all bouncy numbers not exceeding $$n$$. Then given
some proportion $$r \in [0,1]$$ we wish to compute $$E(r) = \min \{ n \, : \,
\frac{|B(n)|}{n} \ge r \}$$.

Slowest: ~1.3s

```lisp
  (let ((digits (digits-of n)))
    (not (or (increasingp digits)
             (decreasingp digits)))) 
```

Medium: ~0.9s
```lisp
  (loop with digits = (digits-of n)
        with increasesp and decreasesp
        for i in digits and j in (rest digits)
        if (< i j) do (setf increasesp t)
        if (> i j) do (setf decreasesp t)
        if (and increasesp decreasesp) return t
        finally (return nil)))
```

Fastest so far: ~0.7s
```lisp
(loop with increasesp and decreasesp
      for m = n then (floor m base)
      for prev-digit = (mod m base) then digit
      for digit = (mod m base)
      until (zerop m)
      if (< digit prev-digit) do (setf increasesp t)
      if (> digit prev-digit) do (setf decreasesp t)
      if (and increasesp decreasesp) return t
      finally (return nil)) 
```

