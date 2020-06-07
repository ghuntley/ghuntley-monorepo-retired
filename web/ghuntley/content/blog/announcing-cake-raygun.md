---
title: Announcing Cake.Raygun
date: '2016-08-09 01:59:00'
categories:
- cakebuild
- xamarin ios
- devops
summary: ''

---
Objective-C debug symbols (dSYM) are essential to debugging Xamarin iOS application crashes. This Cake add-in uploads these symbols to Raygun.io which creates delightfully readable stack traces which aid you in tracking down those bugs. As Raygun adds additional functionality to their API, these features will also be made available in this add-in. The add-in is now available via [NuGet](https://www.nuget.org/packages/Cake.Raygun/) or [GitHub](https://github.com/ghuntley/Cake.Raygun/).


## Installation

Add the following reference to your cake build script:

```csharp
#addin "Cake.Raygun"
```

## Usage

```csharp
var filePath = new FilePath(@"./artifacts/ios/appstore/MyCoolApp.app.dSYM.zip");
var settings = new RaygunSymbolSettings() { ApplicationIdentifier = "", Username = "", Password = "" };

UploadSymbolsToRaygun(filePath, settings);
```
