---
title: closed captioning
---

Baking captions _onto_ a video isn't accessible. Captions need to be embedded _into the video stream_ as text so that the screen reading software such as JAWS can parse the captions. There appears to be two ways to do this:

* [CEA-708/EIA-608](https://en.m.wikipedia.org/wiki/CEA-708) embedded in the video elementary stream as described in ATSC A/72 ([SEI user_data](https://software.intel.com/en-us/blogs/2014/08/18/how-to-add-closed-caption-messages-in-avc-and-mpeg2-streams))
* [CEA-708/EIA-608](https://en.m.wikipedia.org/wiki/CEA-708) transmitted via RTMP onCaptionInfo script/AMF0 tag

# availability

## mixer

Not implemented, see https://watchmixer.uservoice.com/forums/382521-mixer-feedback-ideas/suggestions/17632456-closed-captioning and https://watchmixer.uservoice.com/forums/382521-mixer-feedback-ideas/suggestions/34671694-closed-captioning

## periscope

Any CEA-608/CEA-708 subtitles encoded into the original video will appear if accessibility options are enabled (regardless of the API used to upload the video).

From https://twittercommunity.com/t/closed-caption-support/98570/7

## twitter

Closed captions must be baked into the video and the captions are not onto screen readers

https://twittercommunity.com/t/closed-captions-support-in-twitter-videos/60881/8

Confirmed via uploads 

## twitch

Twitch accepts captions _in line 21 CEA-708/EIA-608 format and in CC1 NTSC field 1. Captions may be transmitted using CEA-708/EIA-608 embedded in the video elementary stream as described in ATSC A/72 (SEI user_data)_ or _CEA-708/EIA-608 transmitted via RTMP onCaptionInfo script/AMF0 tag_. 

When transmitting via RTMP, the payload must contain an ECMA Array with two element pairs:
- A string named "type" containing the characters "708"
- A string named "data" that contains a base64 encoded CEA-708/EIA-608 payload

```
Example:
00000000  12 00 00 69 00 00 00 00  00 00 00 02 00 0d 6f 6e  |...i..........on|
00000010  43 61 70 74 69 6f 6e 49  6e 66 6f 08 00 00 00 02  |CaptionInfo.....|
00000020  00 04 74 79 70 65 02 00  03 37 30 38 00 04 64 61  |..type...708..da|
00000030  74 61 02 00 3c 74 51 41  78 52 30 45 35 4e 41 4e  |ta..<tQAxR0E5NAN|
00000040  4c 41 50 79 55 72 76 79  55 49 50 79 52 51 50 7a  |LAPyUrvyUIPyRQPz|
00000050  49 35 66 7a 73 37 50 7a  76 4c 50 77 67 56 50 7a  |I5fzs7PzvLPwgVPz|
00000060  33 36 66 7a 30 34 2f 78  6f 67 50 79 55 4c 2f 38  |36fz04/xogPyUL/8|
00000070  3d 00 00 09 00 00 00 74                           |=......t|
```

From https://help.twitch.tv/customer/portal/articles/2564215

## youtube

Upload the video and then manually upload the `srt` afterwards.

{{< youtube nI6WNLi2fh8 >}}

## youtube live

- At the bottom of the Ingestion Settings tab, there is a Closed Caption section. Select the Embedded 608/708 item in the dropdown menu.
- Hit Save.
- In your encoder settings choose EIA 608/CEA 708 captions, sometimes called "embedded" captions. While the 608/708 standard supports up to 4 language tracks, YouTube currently only supports one track of captions.

From https://support.google.com/youtube/answer/3068031?hl=en

# creating closed captions

http://www.aegisub.org/

# closed caption data stream

Twitch have open-sourced a library for working with closed captions over at https://github.com/szatmary/libcaption/

Additionally ffmpeg can extract caption data via

`ffmpeg -f lavfi -i movie=input.ts[out+subcc]  -map 0:1  output.srt`

From https://stackoverflow.com/questions/3169910/can-ffmpeg-extract-closed-caption-data 

# misc
* https://youtu.be/Dx-iqLD5b3I?t=7723
* https://github.com/WatershedArts/LiveCaptions
* https://github.com/WatershedArts/LiveCaptions

