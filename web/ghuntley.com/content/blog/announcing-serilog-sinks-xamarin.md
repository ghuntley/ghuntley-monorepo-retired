---
title: Announcing Serilog.Sinks.Xamarin
date: '2016-08-22 16:00:00'
categories:
- logging
- xamarin android
- xamarin ios
- serilog
summary: ''

---

[Serilog](https://serilog.net) has, for quite a time now been my favourite logging library for .NET because _unlike_ other logging libraries, Serilog is built with structured event data in mind which is incredibly powerful. 

Despite using Serilog exclusively server-side with event-based logging for a couple of years now, I've only just started exploring applying this theory to the domain of mobile phone applications. 

After a day of playing around, my imagination is [running wild at all of the possibilities](https://github.com/serilog/serilog/issues/829#issuecomment-241157512):

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/hashtag/serilog?src=hash">#serilog</a> + <a href="https://twitter.com/getseq_net">@getseq_net</a> and <a href="https://twitter.com/hashtag/xamarin?src=hash">#xamarin</a> with event based, structured logging = heavenly.  <a href="https://twitter.com/nblumhardt">@nblumhardt</a> amazing. <a href="https://t.co/6DSNP25BYd">pic.twitter.com/6DSNP25BYd</a></p>&mdash; Geoffrey Huntley (@GeoffreyHuntley) <a href="https://twitter.com/GeoffreyHuntley/status/765744329671311361">August 17, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Today I'm releasing a first of many add-ins to come for Serilog; this one is a sink that writes events to the console of Xamarin.iOS (NSLog) / Xamarin.Android (AndroidLog) applications. It's now available via [NuGet](https://www.nuget.org/packages/serilog.sinks.xamarin) or [GitHub](https://github.com/serilog/serilog-sinks-xamarin).

### Getting started

Install from [NuGet](https://nuget.org/packages/serilog.sinks.xamarin)

```powershell
Install-Package Serilog.Sinks.Xamarin -Pre
```

When using Xamarin.iOS

```csharp
Log.Logger = new LoggerConfiguration()
    .WriteTo.NSLog();
    .CreateLogger()
```

When using Xamarin.Android

```csharp
Log.Logger = new LoggerConfiguration()
    .WriteTo.AndroidLog();
    .CreateLogger()
```

Within your portable class libary or within your application

```csharp
Log.Information("This will be written to either NSLog or AndroidLog");
```

Because the memory buffer may contain events that have not yet been written to the target sink, it is important to call `Log.CloseAndFlush()` or `Logger.Dispose()` when the application/activity exits.
