---
layout: post
title: Adjusting the keyboard rate in Linux
date: 2022-12-21
---

Today I received a nice new [keyboard](https://vortexgear.store/products/tab-75). After plugging it into my laptop to test it out I found it felt a bit sluggish as the repeat rate was slightly slower compared to the laptop's built-in keyboard. There are at least two ways to adjust this in Linux, one using the `kbdrate` command and the other using `xset`.

You can use `kbdrate -d delay -r rate` to set the delay before the keyboard recognises a key has been held down and the rate at which new characters are sent to the computer. Specifying the command without arguments sets the delay and rate to their default values, which for me were a delay of 250ms and rate of 10.9 characters per second.

This command didn't actually seem to have an effect, and would not set a rate below 250ms, since I believe `kbdrate` only works for the TTY. Instead I came across a [website](http://suso.suso.org/xulu/Setting_your_keyboard_repeat_rate_under_X.) that helped remedy the issue. Using `xset r delay rate` sets the delay and rate without issue for X windows.

Hope this helps!
