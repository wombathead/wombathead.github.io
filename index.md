---
layout: default
title: Home
navOrder: 1
---

Hello! This page isn't going to be very interesting until I write some more
blog posts so in the meantime feel free to check out my [about](about.html)
and [misc.](misc.html) pages.

<ul>
{% for post in site.posts %}
<li>({{ post.date | date: "%b %y" }}) <a href="{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>
