---
layout: post
title: "Generating functions via counting change"
date: 2022-01-31
lastEdited: 2022-01-31
---

Earlier this week I was giving a tutorial to first-year computer science
undergraduates in which we were looking at recursive functions. For one
question they were given the function $$g$$ below, and asked to play around
with a few inputs, implement, and describe what the function was doing.

$$ g(x,n) = \begin{cases} 1 & n \le 0 \\ 1 + x g(x,n-1) & \text{otherwise} \end{cases} $$
