---
layout: post
title: A note on netctl
date: 2021-09-14
---

Here is a quick note on how to connect to an open access wireless network using
`netctl` that should connect you to an online login page (but doesn't):
1. run `sudo wifi-menu -o`
2. run `route -n`
3. type the address under the `Gateway` column in the default (`0.0.0.0`) row
   into your web browser
4. fill out the login details form

Step 1 will enter a new profile in `/etc/netctl` and if you are using
`netctl-auto` and everything is configured correctly (it never is) then it
should connect automatically (on my setup it shows the SSID in `i3status` in
green). At this point I was expecting to be redirected to the network login page when I
tried to do anything on my web browser, but since I wasn't Step 2 shows the IP
routing table with numerical addresses so you can find the address you need to
type in in order to get there manually.

Credit goes to the answers on
[this](https://unix.stackexchange.com/questions/89630/how-to-sign-into-an-open-wireless-network)
unix.stackexchange post, and especially to `ekeyser`.
