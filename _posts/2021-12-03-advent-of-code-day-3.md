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
same but for the least common bit and break ties in favour of 0. Although
simple to sate, I found it took a fair bit of code to implement the first time
around as I was performing the filtering for `generator` and `scrubber` all
within the same loop, which meant I had to keep track of separate variables for
each. While this worked, I figured I could do better. Since the rules for
filtering out candidates for the two bitstrings differed only slightly, I
decided to write a function `(filter-candidates candidates filter-rule)` that
took a list of candidate bitstrings and repeatedly applied the function
`filter-rule` to remove invalid elements. Note that in the code snippet below I
also use a function `nth-char=`, which simply determines whether the nth char
of the given string is equal to the given character.

```lisp
(flet (filter-candidates (candidates filter-rule))
 (loop for i from 0 below (length (first candidates))
	   with remaining = candidates
	   for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) remaining))
	   for ones = (- (length remaining) zeroes)
	   for char = (funcall filter-rule zeroes ones)
	   until (= 1 (length remaining))
	   do (setf remaining (remove-if-not (lambda (bitstring) (nth-char= bitstring i char)) remaining))
	   finally (return (first remaining)))))
```

`filter-rule` is a function that takes two numbers and decides whether to
return the character `0` or `1`. This is then run at each iteration to
determine which character we would filter the remaining candidates on (i.e.,
whether to look for a `0` or a `1` at the current index under consideration).
For `generator` this was the rule specifying that, if the number of zeroes at
the given index exceeds the number of ones then we remove all candidates
without a `0` at the given index; for `scrubber` we remove all candidates
without a `1` at the given index if the number of ones exceeds the number of
zeroes. Actually filtering out all the invalid candidates is then simply a case
of calling `filter-candidates` on the puzzle input, once for `generator` and
once for `scrubber`, and a lambda function corresponding to the appropriate
rule:

```lisp
;; filter out invalid bitstrings for generator
(filter-candidates input (lambda (zeroes ones) (if (> zeroes ones) #\0 #\1)))

;; filter out invalid bitstrings for scrubber
(filter-candidates input (lambda (zeroes ones) (if (<= zeroes ones) #\0 #\1)))
```

As I mentioned, I ended up taking two attempts at this. The first one completed
the puzzle in a single loop but required extra variables to keep track of
`generator` and `scrubber` candidates separately, and ended up reusing a lot of
the code. I'm much happier with my second attempt since it makes the solution
more generic and about ten lines more concise. I was also pleased to make use
of higher-order functions since it isn't something I use often and it always
feels powerful when it works. I already spent quite a bit more time on today's
puzzles than the previous two days, so I'm preparing for the steady increase in
difficulty and inevitable loss of more time as the month progresses.

[^1]: I think this is probably because of the way numbers are represented, e.g. two's complement.
