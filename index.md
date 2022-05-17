---
layout: default
title: Home
navOrder: 1
---

Hello! Look at these things I've written and feel free explore my website.

<ul>
{% for post in site.posts %}
<li>({{ post.date | date: "%b %y" }}) <a href="{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>
