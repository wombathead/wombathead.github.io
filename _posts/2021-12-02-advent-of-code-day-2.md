---
layout: post
title: "Advent of Code 2021 -- Day 2: Dive!"
date: 2021-12-02
---

Something I neglected to mention in yesterday's write up if you are new is that
not only do you get 50 puzzles to get stuck into, but there is also an engaging
story to boot. In 2019 you had to save Santa from being stranded in outer
space, and last year you were flying all over the world to collect coins to pay
for your holiday. This year it looks like we'll be underwater -- yesterday the
input was the depth readings from our submarine's sonar and we were determining
how the underwater landscape to safely navigate it. With that successfully done
we are now following the route programmed into the submarine's systems.

The input for the puzzle consists of a number of lines of the form `(direction
x)` telling us how to move the submarine, where `direction` can be one of `up`,
`down`, or `forward` and `x` is the number of units to move. The first part is
simply a case of starting at (0,0) and following the directions, where
`forward` increases our horizontal position, and `down` and `up` increase and
decrease our depth, respectively.

```lisp
(defun advent-02a (filename)
  (loop for (dir dst) in (mapcar #'str:words (get-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y) = '(0 0)
        do (case direction
             (forward (incf x distance))
             (down (incf y distance))
             (up (decf y distance)))
        finally (return (* x y))))
```

The second part again only entailed a slight modification: now we
have a third value to keep track of, `aim`, and the movement rules are slightly
different. `up` and `down` now modify `aim`, and `forward` both
increases our horizontal position and increases our depth by `aim` multiplied
by `x`.

```lisp
(defun advent-02b (filename)
  (loop for (dir dst) in (mapcar #'str:words (get-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y aim) = '(0 0 0)
        do (case direction
             (forward (incf x distance)
                      (incf y (* aim distance)))
             (down (incf aim distance))
             (up (decf aim distance)))
        finally (return (* x y))))
```

I like Lisp's `read-from-string` function, which converts a string into a
symbol, because it means you can effectively get a `switch` statement for
strings without numerous calls to, for example, `string=`. I believe it's also
quicker to compare two symbols for equality than to compare two strings, so
even though the difference is probably insignificant it does feel a bit nicer.

Two simple loops, two stars. Nice, onto the next.
