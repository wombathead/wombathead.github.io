---
layout: post
title:  "A Brief Introduction to Parity Games"
date:  	2019-10-17 20:38:40 +0100
tags: 	[computer science, game theory, maths]
---
{% include mathjax.html %}

Over the past week I have been having more of a think about the area in which I
would like to do my masters thesis, so I decided to see my lecturer in
algorithmic game theory for some suggestions of what to explore. He told me of
his and the department's particular interest in [parity games][parity-games] --
they are apparently an area of quite active research within the computer
science/discrete mathematics group. It seemed interesting, so he pointed me in
the direction of a few papers on the topic, and I have spent the week trying to
get better acquainted with it. Parity games are a subtopic of the more general
area of "games on graphs" within game theory, and it turns out they are a
rather important open problem.

{% 
	include image.html
	url="/images/parity_game.png"
	width="400"
	description="A simple parity game"
%}

## The setup
A parity game is played on a directed graph $$G$$ between two players, Even and
Odd, who move a token from vertex to vertex along the edges of the graph.
Vertices are labelled with a (not necessarily unique) natural number, known as
its *priority*, and each player "owns" a subset of the vertices. The players
continue to move the token until a cycle is formed.  A parity game is an
infinite game, and as soon as an infinite path has been made, the winner is
determined in the following way: if the parity of the highest priority
appearing in the loop is even, then Even wins, else the victory is all Odd's.
We assume that each node has at least one outgoing edge, meaning that it is
always possible to form a cycle.

## The problem
We are interested in determining whether or not a player can always win in such
a game -- a *winning strategy* for a player $$i$$ is a strategy which, if
played by player $$i$$, then the other player $$j$$ cannot win. The decision
problem is therefore: given a parity game $$G = (V, \vec{E})$$, priority
function $$p : V \rightarrow \mathbb{N}$$, and starting vertex $$v_0 \in V$$,
does player $$i$$ have a winning strategy from $$v_0$$? Naturally, the search
problem is to find the set $$W_i(G)$$, the set of nodes from which $$i$$ has a
winning strategy in $$G$$.

## Why it's interesting
This problem is intriguing for two reasons, both practically and in terms of
complexity theory. Firstly, the problem is known to be in $$NP \cap
\text{co-}NP$$, meaning it is relatively easy to verify the correctness of both
"yes" and "no" instances, but it has not been proven to be $$NP$$-complete,
meaning, informally, it may not be *that* hard to solve. Secondly, parity games
are equivalent to model-checking in the [modal μ-calculus][mu-calculus] (I
don't know why this is important yet, but fast automated verification sounds
nice).

{% 
	include image.html
	url="/images/npconp.png"
	width="400"
	description="Parity games lie in the intersection of NP and co-NP"
%}

I should say that I have only just finished reading my first paper on the topic
-- about a deterministic subexponential time algorithm for solving parity games
[\[1\]][det-subexp] -- but will be reading more throughout the week, in
particular one about an implementation and evaluation of various parity game
solvers [\[2\]][oink]. It would also be fun to implement a way of playing and
solving parity games in code, which I also plan to do over the next week or
two.

[mu-calculus]: https://en.wikipedia.org/wiki/Modal_%CE%BC-calculus
[det-subexp]: https://www.dcs.warwick.ac.uk/~mju/Papers/JPZ08-SIAMJComp.pdf
[oink]: https://arxiv.org/pdf/1801.03859.pdf

