---
layout: post
title: "Advent of Code 2021 -- Day 4: Giant Squid"
date: 2021-12-04
lastEdited: 2021-12-06 23:00
---

Having successfully deciphered the submarine's esoteric diagnostic reports, we
now find ourselves in the situation that a giant squid has attached itself to
us. It is now our job to program the submarine's bingo subsystem to win a game
against the squid.

This puzzle required a lot of parsing code, or at least a fair bit more than I
had used for the previous days' puzzles. I chose to represent a bingo card as a
two-dimensional matrix in which each entry was a list containing the number in
the bingo card cell and a marker that signifies whether the number has been
called.[^1] These are all stored in the list `bingo-cards`. The first part is
then as simple as iterating over the numbers, marking the appropriate cards,
and returning the first winning card found. In the snippets below, `mark-card`
simply sets the marker for the given number to `t` in the given `card`, while
`card-wins-p` is a function that checks whether the given `card` has a whole
line of numbers marked `t`.

```lisp
(let (winning-card final-number)
  (loop for number in numbers
        do (loop for card in bingo-cards
                 do (mark-card number card)
                 if (card-wins-p card)
                 do (setf winning-card card
                          final-number number))
        until winning-card)
  (* (card-value winning-card) final-number))
```

The second part gets us to reverse our strategy against the squid and find the
last bingo card to win, and finally find the last number to be called that
would complete it. This only requires a slight change to the logic from the
first part, and we can use all of the same functions already defined for the
first part. Now, instead of looping over all cards until we find a winning one,
we instead loop over all cards that have not won yet, removing newly-winning
ones as we discover them. 

```lisp
(loop for number in numbers
	  with remaining-cards = (reverse bingo-cards)
	  do (loop for card in remaining-cards
			   do (mark-card number card)
			   (setf remaining-numbers (rest numbers))
			   unless (card-wins-p card)
			   collect card into next-remaining-cards
			   finally (setf remaining-cards next-remaining-cards))
	  until (= 1 (length remaining-cards))
	  finally (setf last-winning-card (first remaining-cards)))
```

Once the list containing the remaining non-winning cards reaches length one, we
know we have found the last card to win and we can go about finding the number
it will win on. To do so, we iterate over the remaining numbers in our input
and simply mark the card until it is winning.

```lisp
(loop for number in remaining-numbers
	  do (mark-card number last-winning-card)
	  until (card-wins-p last-winning-card)
	  finally (return (* (card-value last-winning-card) number)))))
```

The hardest part about today's puzzle was probably the parsing code, which I
found I ran into trouble with a fair bit using the loop constructs that I did.
I find using Lisp's language for `loop` is fine for single loops, but it
becomes a bit cumbersome to structure the code to accurately carry out what you
mean when you start nesting them. No doubt this is because I am still getting
accustomed to this mini DSL[^2] but nonetheless it did increase time spent
debugging significantly. Sometimes it is quite nice for a puzzle to have no
hidden tricks, for there just to be a clearly defined problem and for it to
just be about automating the process to solve it, and that was what Day 4 felt
like. I feel the code could be cleaned up and perhaps made more concise, but
I'll leave that until later now.

[^1]: Originally I planned to just track whether a number had been called by whether or not it was positive, but this would have run into problems when calculating a card's score later on.

[^2]: Domain Specific Language
