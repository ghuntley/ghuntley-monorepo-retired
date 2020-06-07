---
title: Compiling Xamarin.Mac libraries on Windows or AppVeyor
date: '2016-08-02 21:13:00'
categories: []
summary: ''

---
## Introduction
* Why this is a problem for library authors, packaging build artifacts into nupkgs from different hosts is nightmareish, advoid.
* How this was resolved (brute force, learning slowly as you go).

## Theory
* Explain how assembly resolution works in .NET
* Explain the various msbuild options
* Explain the difference between xm45 and mac mobile.
* Explain the current problems with packaging - https://github.com/NuGet/Home/issues/2662


## How to actually do it.
* Step by step

## With thanks
* Chris & Miguel.

1. Hack csproj or deploy custom msbuild targets
2. Compile

    C:\PROGRA~2\MSBuild\14.0\Bin\msbuild.exe MyCoolXamarinMacLibrary.sln /p:"ReferencePath=C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\Xamarin.Mac\v1.0" /p:"NoStdLib=true"

