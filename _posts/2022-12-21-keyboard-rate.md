---
layout: post
title: Adjusting the keyboard rate in Linux
date: 2022-12-21
---

Here's two ways to adjust your keyboard rate:
- `xset r rate 200 30`
- `kbdrate -d 250 -r 30`

I use `xset` since on Intel systems the delay using`kbdrate` must be between 250ms and 1000ms.
I also put that line in my `.xinitrc` so that it is set correctly at the start of each X session, though when I update my system without restarting I always have to `source .xinitrc`, so maybe there's a better way.
