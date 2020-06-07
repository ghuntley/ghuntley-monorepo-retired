---
title: "Open-Source"
---

Open-source is a way of life for Geoff; when he's not authoring code, you will find him building communities, passing down knowledge to others, mentoring first timers and pondering how to make open-source more inclusive.

Through his leadership as an open-source maintainer and servant leader, Geoff has managed to revive a once dead community into a vibrant community of 1000+ developers from 22 timezones that supports the projects they depend on. Reactive programming in .NET is now [financially subsidised](https://opencollective.com/reactiveui) by community crowdsourcing, and we are on track to exceed $10,000USD by the end of 2018.

# Contributions


## Chrome

Authored [an extension](https://github.com/ghuntley/send-from-outlook-dot-com) that makes Outlook.com your default email application and provides a button to compose a message to quickly share a link via email

## Debian

### SeaBIOS SLIC
If your computer is installed with Windows 7/8 by default but you'd prefer to
run Linux as your desktop but on ocassion run a single Windows Virtual Machine
under KVM using the activiation credentials within your computers BIOS then
SeaSLIC can help you achieve this.

As of 6th of Apr 2014 thanks to the excellent work by Michael Tokarev this
patch is now integrated by default into Debian which removes the need to roll
and maintain your own your own copy of SeaBIOS.

https://github.com/ghuntley/seaslic

## Cake.Build

### Cake.AppleSimulator

Manage (simctl) Apple iOS/watchOS/TV Simulators using Cake. I created this
addin so folks could run automated end-to-end tests locally before running them
on real devices in the Xamarin Test Cloud. Here's a testamonial from [Robert](
https://github.com/cake-contrib/Cake.AppleSimulator/pull/4?utm_campaign=Weekly%2BXamarin&utm_medium=web&utm_source=Weekly_Xamarin_116#issuecomment-266957347)
who is using it to save $200K USD a year:

> I am using it as one of a series of required criteria for submitting
> commits/pull requests before allowing those changes to ripple up through a
> chain of tiered automated unit testing that start on a small CI and mobile
> test cloud before allowing it to get to the full-blown test cloud. Just
> yesterday it blocked 6 pull requests due to iOS specific code mistakes that
> would have normally consumed about 3 hours of private mobile cloud time at
> $500/hour, over a year we are expecting it to save a minimum of 120 hours on
> cloud time, we are also doing with Android AVDs, and macOS VMs and are seeing
> a saving $200k a year in test hours.

<video autoplay="true" controls="" loop="true" poster="https://pbs.twimg.com/tweet_video_thumb/CnyRVknWAAAWpsA.jpg" width="700"><source src="https://pbs.twimg.com/tweet_video/CnyRVknWAAAWpsA.mp4" type="video/mp4"><div class="_3tixQkQf"><img src="https://pbs.twimg.com/tweet_video_thumb/CnyRVknWAAAWpsA.jpg"><span class="_1rwV18JF">Your browser does not support playback of this video.</span></div></video>

The add-in is available via
[NuGet](https://www.nuget.org/packages/Cake.AppleSimulator/) or
[GitHub](https://github.com/ghuntley/Cake.AppleSimulator/).

### Cake.Raygun

Objective-C debug symbols (dSYM) are essential to debugging Xamarin iOS
application crashes. I created a Cake add-in uploads these symbols to Raygun.io
which creates delightfully readable stack traces which aid you in tracking down
those bugs. The add-in is available via
[NuGet](https://www.nuget.org/packages/Cake.Raygun/) or
[GitHub](https://github.com/ghuntley/Cake.Raygun/).

# NixOS