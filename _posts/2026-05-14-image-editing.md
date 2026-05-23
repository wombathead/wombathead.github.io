---
layout: post
title: Open source image editing on a Mac
date: 2026-05-15
---

I like to shoot film.
For now I don't develop the exposed rolls myself, so I drop them off at a lab to do it for me.
I also like to receive scans so that I can show them to my friends and family and print them out (after editing them).
I currently use a Mac so here is a free process to get those photos from the zip file sent by the lab to a fully edited and correctly timestamped version that I am happy with:
- Download scans from Dropbox or WeTransfer and unzip.
- Use [exiftool](https://exiftool.org/) to edit the orientation if necessary.
  - Example: `exiftool -orientation=8 {22,32}.tif -n`
  - `-orientation=6` (rotate 90 CW) or `-orientation=8` (rotate 90 CCW)
- Use [XnView](https://www.xnview.com/en/) to edit the `DateTimeOriginal` field relatively quickly.
- Use [darktable](https://www.darktable.org/) to edit the scan into something I'd want to print and show people.
- Use [OpenMTP](https://github.com/ganeshrvel/openmtp) to get the edited photos from my computer to my Android phone.

Done!
