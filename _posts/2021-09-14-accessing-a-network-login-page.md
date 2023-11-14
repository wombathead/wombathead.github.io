---
layout: post
title: Accessing a network login page manually
date: 2021-09-14
---

Recently I had problems trying to connect to an open access network (with
`wifi-menu` and `netctl` if it matters) since it should have taken me to an
online network login page but didn't. Here is a note on how to get to the login
page.

1. run `sudo wifi-menu -o` and select the desired network
2. run `route -n`
3. type the address under the `Gateway` column in the default (`0.0.0.0`) row
   into your web browser
4. fill out the login details form

Step 1 will enter a new profile in `/etc/netctl` and if you are using
`netctl-auto` and everything is configured correctly (it never is) then it
should connect automatically (on my setup it shows the SSID in `i3status` in
green). 

Usually I would then be directed to the network login page if I tried to do
anything on my web browser (or at least be given the option to). Since I
wasn't, Step 2 shows the IP routing table. This provides you with the numerical
address to type into the search bar to get to the login page manually.

Credit goes to the answers on
[this](https://unix.stackexchange.com/questions/89630/how-to-sign-into-an-open-wireless-network)
unix.stackexchange post, and especially to `ekeyser`.
