---
layout: default
title: Home
navOrder: 1
---

Hello!
This is my website.
Maybe you will find the things below interesting or useful.

<ul>
{% for post in site.posts %}
<li>({{ post.date | date: "%b %y" }}) <a href="{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>
