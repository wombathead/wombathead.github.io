---
layout: post
title: Transferring files between Linux and Android
date: 2023-11-01
---

Here's a quick note on the ways to transfer files between an Android device and a computer running Linux.
I've found this is particularly useful recently when I've wanted to transfer high-quality image scans (i.e., large files) from my laptop to my phone which I've edited on my laptop beforehand.

## With a wire

There are two tools I’ve used that implement the Media Transfer Protocol, Android File Tranfer[^1] and Simple MTPFS.[^2]
Both provide a command-line utility, and after connecting the Android device to the computer via a cable you can mount it at e.g. `~/mnt` with either `aft-mtp-mount ~/mnt` or `simple-mtpfs ~/mnt`.
You can then use all the regular Linux commands to transfer files between the two devices.
After you’re done you can unmount using `fusermount -u ~/mnt`.
Android File Transfer also provides a GUI utility, which you can run connecting the device and then running the `android-file-transfer` program.

## Without a wire

If for some reason you can't use a cable (mine decided to just stop working for transferring files recently) then you can do so wirelessly using [KDEConnect](https://kdeconnect.kde.org/).
This requires you to install the KDEConnect app on both devices.
Then you can run the program on your computer and control the connected device from there, or vice versa and run the KDEConnect app on the Android device and use the interface to transfer the files.
You can run `kdeconnect-app` to run the GUI, but so far I have been using it a lot to transfer photo albums between my laptop and phone and the GUI makes it difficult/tedious/impossible to bulk send files.
Instead I have found it easier to use the CLI by running `kdeconnect-cli`.
If I'm in the directory where the photos I want transferred live then I can get the whole album across by running `kdeconnect-cli -n <device name> --share *.jpg`.

## Footnotes

[^1]: [`android-file-transfer`](https://archlinux.org/packages/?name=android-file-transfer) on the AUR.
[^2]: [`simple-mtpfs`](https://aur.archlinux.org/packages/simple-mtpfs) on the AUR.
