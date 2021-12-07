---
layout: post
title: "Advent of Code 2021 -- Day 7: The Treachery of Whales"
date: 2021-12-07
lastEdited: 2021-12-07 10:00
---

{% include mathjax.html %}

Armed with the knowledge that the sheer mass resulting from the lanternfish's
exponential growth will one day cause the earth to collapse into a black hole
and extinguish humanity, meaning all your triumphs as well as mistakes will be
forgotten, we now set to work to escape a grisly encounter with a whale. For
various plot reasons, we have to align a collection of crab-piloted submarines,
which can only move horizontally, such that the sum of the costs for each crab
moving from his starting position to the aligned position is minimised.

Two simple loops to share for today's puzzles. In both cases, given our input
of crab positions as a list of horizontal positions, we must find a point on
which to align all the crabs to minimise their total cost. For the
first part, a crab's cost for a proposed position `x` is simply the difference
between its position and `x` -- that is, $$c_i(x_i,y)=||x_i-y||$. We can
minimise this by simply iterating over all numbers between the maximum and
minimum crab positions, calculating the cost of placing the alignment point
there, and keeping the minimum calculated cost. The `minimize` construct is
very useful here.

```lisp
(flet ((total-cost (position numbers)
         "Sum of absolute differences between POSITION and NUMBERS"
         (loop for n in numbers
               sum (abs (- position n)))))
  (let ((input (parse-numbers filename)))
    (loop for i from (reduce #'min input) upto (reduce #'max input)
      minimize (total-cost i input))))
```

The second part has us change the definition of a crab's cost: now for a point
placed at $$y$$ a crab incurs cost 1 if she is 1 away from $$y$$, 1+2=3 if she
is 2 away from $$y$$, 1+2+3=6 if she is 3 away, and so on. In other words, if
the difference between her position and the point $$y$$ is $$||x_i-y||=n$$,
then her total cost is the sum of natural numbers from 1 to $$n$$, or:

$$ c_i(x_i,y) = \sum_{i=1}^n i = \frac{n(n+1)}{2} $$

So we can simply substitute this formula when calculating the crab's cost in
the previous part to get our answer for part two.

```lisp
(flet ((total-cost (position numbers)
         (loop for n in numbers
               for difference = (abs (- position n))
               sum (/ (* difference (1+ difference)) 2))))
  (let ((input (parse-numbers filename)))
    (loop for i from (reduce #'min input) upto (reduce #'max input)
      minimize (total-cost i input)))) 
```

This is currently the puzzle I have solved the quickest, but apparently its
ease was not lost on everyone else because it wasn't my highest rank. Takeaway
from today: `minimize` is pretty useful and lets you essentially write out the
mathematical expression of the solution to find it. I'm not sure if there would
be a more efficient way to arrive at the answer (maybe using binary search?),
but I'm not too fussed about finding one as the minimum and maximum input
positions don't seem to be too far apart (now if they were *exponentially* far
apart it could be interesting) and the running time only depends on this
distance.
