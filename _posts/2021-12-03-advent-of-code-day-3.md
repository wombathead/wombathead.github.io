---
layout: post
title: "Advent of Code 2021 -- Day 3: Binary Diagnostic"
date: 2021-12-03
---

Looks like the submarine is making some concerning noises and we have to write
some code to figure out what's wrong with it. We are provided with the input as
a list of bitstrings that represents the submarine's diagnostic report. For the
first part we are building up a bitstring, call it `gamma`, using the most
common bit at each index. That is, for each index from 0 up to the length of
the input bitstrings, we must find the most common bit (0 or 1) and then write
it at that index in `gamma`.

Nothing too hairy in this loop and there are no hidden tricks: it was useful to
use a lambda in order to collect all of the bits at a given index and then pass
them to Lisp's `count` function. I also considered at first using `gamma` as a
list, then at each iteration pushing the most common bit to this list, though
this would have entailed an extra reversal (since `push` would mean elements
are stored in the reverse order) as well as a conversion from list to number.
Instead, `gamma` is considered a number from the get-go and at each iteration,
depending on whether we found 0 or 1 to be most common at the current index,
we either left-shift it once (to "write a zero" to the bitstring) or left-shift
it once and add 1 (to "write a one" to the bitstring).

The answer to the puzzle was then the product of `gamma` and `epsilon`, where
`epsilon` was obtained in exactly the same way except by finding the least
common bit at each index. This could be found from `gamma` by just flipping its
bits. Unfortunately we can't just call `(lognot gamma)` since this was
resulting in negative numbers[^1], so instead we find `epsilon` with XOR
between `gamma` and $2^n-1$, where $n$ is the length of the bitstring.

```lisp
(defun advent-03a (filename)
  (loop with input = (get-file filename)
        with n = (length (first input))
        with m = (length input)
        for i from 0 below n
        with gamma = 0
        for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) input))
        do (setf gamma (if (>= zeroes (/ m 2)) (ash gamma 1) (1+ (ash gamma 1))))
        finally (return (* gamma (logxor (1- (expt 2 n)) gamma)))))
```

Part two was a bit more complex. We have to find two bitstrings, `generator`
and `scrubber`, by successively filtering out bitstrings in the input that do
not meet certain criteria. For `generator`, for each index we must find the
most common bit among all remaining candidates (breaking ties in favour of 1)
then filter out those without that bit at that index. For `scrubber`, we do the
same but for the least common bit and break ties in favour of 0. Simple to
state, but I found it took a fair bit of code to implement since we must track
the remaining `generator` candidates and the remaining `scrubber` candidates
separately. Since the two rules only differ slightly, I decided to write a
function `filter-candidates` taking a list of candidates (the input) and a
filter rule, which is a function returning the character to check equality for
based on the number of ones and zeroes (to get the most or least common). 

```lisp
(filter-candidates (candidates filter-rule)
 (loop for i from 0 below (length (first candidates))
	   with remaining = candidates
	   for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) remaining))
	   for ones = (- (length remaining) zeroes)
	   for char = (funcall filter-rule zeroes ones)
	   until (= 1 (length remaining))
	   do (setf remaining (remove-if-not (lambda (bitstring) (nth-char= bitstring i char)) remaining))
	   finally (return (first remaining)))))
```

To filter out all invalid candidates for `generator` and `scrubber`, I could
just pass the puzzle input and a lambda function corresponding to the
appropriate rules, like so:

```lisp
;; filter out invalid bitstrings for generator
(filter-candidates input (lambda (zeroes ones) (if (> zeroes ones) #\0 #\1)))

;; filter out invalid bitstrings for scrubber
(filter-candidates input (lambda (zeroes ones) (if (<= zeroes ones) #\0 #\1)))
```

I ended up taking two attempts at this, the first one worked but reused a lot
of the code unnecessarily and had too many variables floating about, in order
to keep track of both `generator` and `scrubber` at the same time. I'm much
more pleased with the second attempt since it makes the code more generic and
about ten lines more concise. Overall very pleased that I could make good use
of higher-order functions. Already spent quite a bit more time on today's
puzzles, so I imagine I'm only going to waste more time as the month
progresses now.

[^1]: I think probably because of the way numbers are represented, e.g. two's
  complement.
