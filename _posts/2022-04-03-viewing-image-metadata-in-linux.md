---
layout: post
title: Viewing image metadata in Linux
date: 2022-04-03
---

Recently I was writing up my [camping trip to Dartmoor]({% post_url
2021-09-06-tor-torture %}) and I wanted to transfer some photos from my phone
to my computer. A quick note on that first: since my phone is Android I have to
use [Media Transfer
Protocol](https://wiki.archlinux.org/title/Media_Transfer_Protocol) to transfer
files. There are a bunch of options to do so but I've found that
[simple-mtpfs](https://github.com/phatina/simple-mtpfs) is the easiest to use
and doesn't seem to go wrong. To use it you simply plug in the USB cable with
the phone attached, run `simple-mtpfs <mountpoint>`, and voilà, you can now
transfer files back and forth between devices. To unmount just run `fusermount
-u <mountpoint>`.

Two commands you can use to view the metadata of image files are `file` and
`identify`. The former can be used on any file type while the latter is
specific to image files.
