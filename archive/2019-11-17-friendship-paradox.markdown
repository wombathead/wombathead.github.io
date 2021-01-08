---
layout: post
title:  "The Friendship Paradox"
date:   2019-11-17 16:00:30 +0100
tags: [computer science, graph theory]

---

{% include mathjax.html %}

Here's an interesting paradox: on average, a randomly chosen person will have
fewer friends than their friends. On top of serving as a knock to one's
confidence, it also seems impossible -- how can it be that a random person's
friend has on average more friends than them, when they are part of the same
network? In this post I will attempt to explain to you one of my favourite
paradoxes from graph theory/statistics.

Firstly, this paradox isn't unique to the fact that we are talking about
peoples' friends -- it could apply to any relation on a network (for example,
most peoples' sexual partners have had more sexual partners than they have, on
average). The paradox arises from a sampling bias in that people with many
friends are more likely to be counted as a friend of a given person. Consider
some social network represented by an undirected graph $$G=(V,E)$$, where $$V$$
denotes the set of all people within the network and edge $$(u,v) \in E$$ if
$$u$$ and $$v$$ are friends (note that this relation is symmetric -- if $$u$$
is friends with $$v$$, then $$v$$ is friends with $$u$$). The average number of
friends of a given person in the network is:

$$
	\mu = \frac{\sum \text{deg}(v)}{|V|} = \frac{2|E|}{|V|}
$$

where $$\text{deg}(v)$$ denotes the degree of node $$v$$, i.e. the number of
friends of node $v$. Note that the second equality comes from the handshaking
lemma. We can then count the average number of friends by choosing a random
person in the network and counting how many friends they have, and taking the
expected value of this.  This is the same as picking a random edge in the graph
(a friendship), picking one of the friends in this friendship, counting their
friends. The probability of choosing such a node $$v$$ in the graph is:

$$
	\frac{\text{deg}(v)}{|E|} \cdot \frac{1}{2}
$$

The first factor comes from the fact that we are more likely to pick a node
with a higher number of connections in the graph, and the second comes from the
fact that each edge connects two vertices. Recall that the expression for the
expected value of random variable $$X$$ is given by $$\sum_x x \cdot Pr[X=x]$$,
thus we the expected average number of friends in the network as:

$$
	\sum_v \text{deg}(v) \cdot \frac{\text{deg}(v)}{2|E|} = \sum_v \frac{\text{deg}(v)^2}{2|E|}
$$

Now recall that variance $$\sigma^2$$ is defined by the formula:

$$
	\sigma^2 = \frac{1}{N} \sum_i (x_i - \mu)^2
$$

Applying this to our case, we get:

$$
\begin{align}
	\sigma^2 & = \frac{1}{|V|} \sum_v (\text{deg}(v) - \mu)^2 \\
	& = \frac{1}{|V|} \sum_v \left( \text{deg}(v)^2 - 2\mu \cdot \text{deg}(v) - \mu^2 \right) \\
	& = \frac{\sum_v \text{deg}(v)^2}{|V|} - \frac{2\mu}{|V|} \sum \text{deg}(v) + \mu^2 \\
	& = \frac{\sum_v \text{deg}(v)^2}{|V|} - 2\mu^2 + \mu^2 \\
	\mu^2 + \sigma^2 & = \frac{\sum_v \text{deg}(v)^2}{|V|}
\end{align}
$$

The expected number of friends of a randomly chosen node is therefore given by:

$$
\begin{align}
	\sum_v \frac{\text{deg}(v)^2}{2|E|} & = \frac{\sum_v \text{deg}(v)^2}{|V|} \cdot \frac{|V|}{2|E|} \\
	& = \frac{|V|}{2|E|} (\mu^2 + \sigma^2) \\
	& = \frac{(\mu^2 + \sigma^2)}{\mu} \\
	& = \mu + \frac{\sigma^2}{\mu}
\end{align}
$$

Now, $$\mu$$ and $$\sigma^2$$ are both obviously non-negative. For a graph with
varying degrees among its nodes, we also have that the two values are also
strictly positive (i.e. positive and non-zero). Hence the average degree of a
randomly chosen friend is strictly greater than the average degree of a random
node.

Hopefully this had shed some light on what I find to be an interesting result
from graph theory and statistics -- I particularly like it as it says a lot and
the explanation is fairly simple. I have included most steps in the derivation
so that it is relatively easy to follow, and while there's not much too it I
definitely think it is beneficial to fully understand where each term comes
from and how it leads to the next.

I decided to start learning Lisp this week -- maybe there will be posts in the
near future about things I have found interesting in that regard.
