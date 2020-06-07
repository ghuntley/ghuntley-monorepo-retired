---
title: what's next
date: 2019-09-17 15:14:00
layout: blog
---

If you have been keeping a careful eye on GitHub commits you might have already guessed that [after a long break from .NET](https://reactiveui.net/blog/2018/05/reactiveui-succession) - I'm back.

I'm still a super huge fan of functional programming (and Haskell) but due to multiple waves of redundancies (and judging off their careers page - a departure of the CFO) at a previous employer I've had to pause previous engineering leadership ambitions to help [bring haskell to the masses](https://haskellguide.com/). There definitely is a huge pool of people out there wanting to do Haskell as their day job and use it to build real products that make money. Haskell is definitely a way companies can solve their talent acquisition problems.

Anyway, [Jérôme Laban](https://twitter.com/jlaban) upon hearing the news that my previous employer had left me stranded in Seattle hinted that I should work with him on the [Uno Platform](https://platform.uno/). To be honest - at first I wasn't sure as I've previously done quite [a-lot in the Xamarin ecosystem](http://weeklyxamarin.com/) and was half-way through a ecosystem transition. I distinctly remember asking Jérôme _"What makes Uno special?"_:

1. Until recently, all Windows 10 XAML platform controls were tightly-coupled to the version of the OS run by the user. This meant fixing any bugs related to platform controls would require an update to the user's entire OS. [WinUI changes everything](https://github.com/microsoft/microsoft-ui-xaml).

1. [Uno](https://platform.uno/) took the correct approach and implements the Windows 10 `windows.ui.xaml.dll` contracts which means your existing Windows 10 XAML should just work on all platforms - AS-IS. If it doesn't [please raise a GitHub feedback issue](https://github.com/unoplatform/uno/issues/new?labels=kind%2Ffeedback%2C+triage%2Funtriaged&template=feedback.md). This translates into the ability to share styles, layouts, and data-bindings while retaining the ability to mix XAML-style and native layouts on a per-platform basis.

1. With Uno, all views inherit from the native base view type. On Android this means View. On iOS this means UIView. Since Uno couldn't change the design of the iOS or Android frameworks, Uno made DependencyObject a interface with a implementation that is automatically supplied by code generation. This design decision allows an Uno FrameworkElement to be a View or UIView and at the same time to be a DependencyObject.

1. WebAssembly, [WebAssembly changes everything](https://ericsink.com/entries/wasm_wasi_dotnet.html). Most folks think WebAssembly is about the "web" but that's missing the big picture - wasm [is so much more than that](https://wasmer.io/). I kinda wish I had of listened more to [Jay Phelps](https://twitter.com/_jayphelps), sooner.

1. Project Umbrella. Right now, Uno is a cross-platform UI framework but there's more to come. <!-- <spoilers>There's eight years of user-interface development best practices and knowledge - based upon reactive programming principles that has yet to open-sourced.</spoilers> -->

and that won me over. For the last couple of months I've been shipping features along side the engineering team, crafting code examples, supporting + pair programming with community contributors, documenting knowledge, and acting as an internal advocate and feedback-getter.

Even though I'm in love with Montréal, I'm [not going to be moving here](https://twitter.com/MaximRouiller/status/1174429543165255680). I'll be shipping commits remotely from my office back home — Sydney, Australia.

![My remote office](https://user-images.githubusercontent.com/127353/65178235-4e5f6d80-da26-11e9-9ddd-0608cf26ab09.png)

Speaking of Montréal, Québec. I'm here for [UnoConf](https://unoconf.com) and will be staying around for the next couple weeks so if you want to catch up for a cup of ☕ [just ask](/contact). 

UnoConf is tomorrow and we are releasing some cool stuff (tm) that I've been working on. If this is your first time here in Montréal here’s a [list of ideas](
https://ghuntley.com/blog/welcome-to-montreal-quebec/) to help you fall in love with one of my favourite cities. 

If you can't make it, stay tuned to [the livestream](https://twitter.com/UnoPlatform/status/1173654792146755585) to learn more.
