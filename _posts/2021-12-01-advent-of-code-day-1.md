---
layout: post
title: Advent of Code 2021 -- Day 1 Sonar Sweep
date: 2021-12-01
---

It's the most wonderful time of year -- I'm not talking about silly things like
warm blankets, family, and Christmas cheer... no, I'm excited that [Advent of
Code](https://adventofcode.com/2021) is finally here![^1]

In case you don't know Advent of Code is a fun set of programming challenges
devised by Eric Wastl that you must complete in order to save Christmas. Each
day you are presented with two problems to solve, the second being an extension
unlocked after successfully completing the first. You are rewarded for your
efforts with a shiny gold star. The problems start off somewhat easy and amp up
the difficulty over the course of the month and are a great way to learn a new
language. I'll write my solutions in Common Lisp again this year because there
is still so much for me to improve on with it.

The first part of today's puzzle required counting the number of times an entry
in a list of numbers was greater than the one before. This was done relatively
painlessly using Lisp's `loop` construct (which I feel I am only just using
properly). It simply sets `b` to the current list item, `a` to the previous
item (and `nil` to begin with), then for every item but the first compares `b`
with `a`. I particularly like the `count` clause which counts the number of
times the expression following it evaluates to `t`.

```lisp
(defun advent-01a (filename)
  (loop with input = (mapcar #'parse-integer (get-file filename))
        for a = nil then b
        for b in input
        when a
        count (> b a)))
```

Part two was a slight modification of the previous part: now we had to keep
track of a sliding window of size three and count the number of times the sum
of the elements in the window exceeded the sum of the elements in the previous
window. Only a few changes were required, and the `on` clause was very useful
to be able to assign and work with the values `a`, `b`, and `c` of the sliding
window.

```lisp
(defun advent-01b (filename)
  (loop with input = (mapcar #'parse-integer (get-file filename))
        for (a b c) on input
        while (and a b c)
        for prev-sum = nil then sum
        for sum = (+ a b c)
        when prev-sum
        count (> sum prev-sum)))
```

Quite a relaxed introduction back into Advent of Code. I'm pleased with what
I've written, not least because it looks particularly clean, and I'm looking
forward to spending and inevitably wasting more time on these puzzles as the
month goes on (not like I have an important viva to work on...).

[^1]: That opening, while catchy, took far too much effort.
