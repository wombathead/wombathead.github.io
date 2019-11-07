---
layout: post
title:  "NLP, Nash Equilibria, and Two-Sided Auctions"
date:   2019-11-07 14:00:30 +0100
tags: [computer science, game theory, mechanism design, auctions, natural language processing]

---

{% include mathjax.html %}

Been a bit quiet on here the last few weeks as a bunch of assignments'
deadlines all converged to this week -- in fact I have just submitted my fourth
of four in four days, so I haven't bored at least. One was just a bunch of
things in Python to do with stock trends and the like, but the other three were
more interesting, so I will write about them in this blog post.

## Natural Language Processing -- Language Models and Perplexity
The first of the assignments I'll talk about was probably the one which
required the most effort, and that was Natural Language Processing (NLP). The
assignment consisted of a collection of tasks to do with analysing a collection
of news articles, ultimately leading up to creating a language model to
construct a new sentence. The first part involved preprocessing the text,
meaning I got familiar with Python's `re` library for regular expressions, as
well as [lemmatisation][lemmatisation] -- splitting the words of the text into
lemmas, to reduce the number of unique words and thus making learning from it
easier. The `WordNetLemmatizer` library was especially useful here. The next
tasks involved getting to grips with [NLTK][nltk], and involved computing the
most frequently-occurring trigrams in the body of text. After then computing
which articles were mostly positive and mostly negative, it was now time to
create our own language model and evaluate it. In my solution I create a
trigram model, meaning for each sequence $$(w_1, w_2, w_3)$$ of three words
that appears in the corpus (a _trigram_), we count the number of times it
occurs so that, in the future, we can decide the probability that a particular
word follows a given _bigram_ (sequence of two words). We then generated a
sentence of 10 words using this language model and it made no sense, but that
wasn't the point -- the point was that we used statistical methods to generate
some data that wasn't entirely random. Finally we had to evaluate our model's
[perplexity][perplexity] -- this is done according to the formula:

$$
	PP(W) = \sqrt[N]{\prod_i^N \frac{1}{p(w_i | w_{i-2} w_{i-1})}}
$$

Since the probabilities we are multiplying end up being so small, we suffer
from underflow -- even with Python's 64-bit floats we do not have enough bits
to represent the numbers precisely. Hence we use instead the equivalent
formula:

$$
	PP(W) = 2 ^ { - \frac{1}{N} \sum_i^N \log_2{p(w_i | w_{i-2} w_{i-1})} }
$$

When testing the model on the test set, some trigrams appeared that hadn't
appeared in the training corpus, hence it would have zero probability of
occurring. Obviously, we can't expect to observe all the trigrams we will ever
see in a relatively small training set, so we opt instead for [Laplace
smoothing][laplace], which simply adds one to all trigram counts (of course, we
end with a bias towards unseen trigrams as opposed to seen trigrams, so this
smoothing isn't the best). Other, better options include Good-Turing Smoothing
and Kneser-Ney Smoothing, but there wasn't the time to implement them
successfully.

## The Theory of Number-Guessing Games
Next up was Algorithmic Game Theory -- there is not much to say about this
other than it consisted of computing some Nash Equilibria given a game, and in
the more general sense commenting on the Nash Equilibria if the payoff matrices
took some form for variable payoffs. Most interesting was constructing the
normal-form of the following game:

Two players, I and II, play a game as follows: I selects a number from the set
$${2, 3, 4}$$ and II picks two numbers from the same set. If II picks any of
the same numbers as I, I pays II that amount to II. Else, the game is a draw.

We get the following payoff matrix:

| I, II | $${2,3}$$ | $${2, 4}$$ | $${3, 4}$$ |
|------:|:---------:|:----------:|:----------:|
| $$2$$ | $$-2, 2$$ |  $$-2, 2$$ |  $$0, 0$$  |
| $$3$$ | $$-3, 3$$ |  $$ 0, 0$$ |  $$3, 3$$  |
| $$4$$ | $$ 0, 0$$ |  $$-4, 4$$ |  $$4, 4$$  |

Eventually we find that there is Mixed Nash Equilibrium at $$((\frac{6}{13},
\frac{4}{13}, \frac{3}{13}), (\frac{7}{13}, \frac{5}{13}, \frac{1}{13}))$$.

## The truth, the whole truth, and nothing but the truth
The final assignment was a critical analysis of a scientific paper of our
choosing: I went for *Approximately Efficient Two-Sided Combinatorial Auctions*
by Colini-Baldeschi et al. [[1]][AETSCA], as I am interested in reading more on
the area of auctions and mechanism design. The paper introduces two
approximation mechanisms for two-sided auctions -- auctions in which we have a
set of buyers and sellers with multiple distinct items, each with their own
valuation functions (for receiving a set of items for the buyers, and for
retaining or selling a set of items for the sellers). These markets differ to
combinatorial exchange markets in that in the latter, agents may act as both
seller and buyer. The paper introduces two mechanisms which, subject to
maintaining slightly different properties of the mechanism and subject to the
agents' valuation functions fulfilling slightly different constraints, achieve
a $$(2+4\alpha)$$-approximation of the optimal social welfare. They also
introduce a new concept *Direct Trade Strong Budget Balance*, which prevents
agents from receiving money if they are not directly involved in a trade (that
is, selling a bundle). For more context, I would recommend reading the classic
paper [[2]][MyersonSatterthwaite] which proves the impossibility result of an
exact mechanism, and [[3]][DynamicDoubleAuctions] and [[4]][MUDA] for some more
contemporary work in the area.

Well that's what I've been working on these past few weeks -- workload seems to
be a bit lighter after next week, so I may be able to get some more stuff out
on things I decide to learn about in my own time, or indeed the things I'm
learning as part of my course. In any case, I'll certainly try to keep posts in
the future slightly more focused than this has been -- until next time.

[lemmatisation]: https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html
[nltk]: https://www.nltk.org/
[perplexity]: https://towardsdatascience.com/perplexity-intuition-and-derivation-105dd481c8f3
[laplace]: https://medium.com/syncedreview/applying-multinomial-naive-bayes-to-nlp-problems-a-practical-explanation-4f5271768ebf
[AETSCA]: https://dl.acm.org/citation.cfm?id=3085128
[MyersonSatterthwaite]: https://cpb-us-w2.wpmucdn.com/campuspress.yale.edu/dist/3/352/files/2011/01/1983-bilateral-trade.pdf
[DynamicDoubleAuctions]: https://epubs.siam.org/doi/pdf/10.1137/1.9781611975482.11
[MUDA]: https://pdfs.semanticscholar.org/abfb/f52b64e728d0a2bf52467a9fffce8926f849.pdf
