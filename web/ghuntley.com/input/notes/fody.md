---
layout: fody
title: fody
---

Last night at an ungodly hour I did a massive refactor on a prototype codebase with no unit tests and now the once working project now no longer compiles because something I did is causing the most excellent [Fody](https://github.com/Fody/Fody) to blow up. Normally you would just go and do a [bisect using git](https://git-scm.com/docs/git-bisect) but in this case I didn't incrementally commit on each change as I was on my own branch and the work was all related to a single higher unit of work. Old habits die hard, sometimes they come back to bite you hard...

So how exactly to you get yourself out of this situation? Your application won't launch/debug because it won't compile, hmm pickle.....

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/GeoffreyHuntley">@GeoffreyHuntley</a> if u want to debug the actual build u need to attach to msbuild.exe</p>&mdash; Simon Cropp (@SimonCropp) <a href="https://twitter.com/SimonCropp/status/684972737811447811">January 7, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

It turns out back when .NET 4.0 landed Microsoft [added an unofficial](http://blogs.msdn.com/b/visualstudio/archive/2010/07/06/debugging-msbuild-script-with-visual-studio.aspx
) `/debug` flag:

    # cd C:\Windows\Microsoft.NET\Framework\v4.0.30319>
    # msbuild /debug

    Microsoft (R) Build Engine version 4.6.1038.0
    [Microsoft .NET Framework, version 4.0.30319.42000]
    Copyright (C) Microsoft Corporation. All rights reserved.

    MSBUILD : error MSB1001: Unknown switch.
    Switch: /debug

But it won't work until you enable it from an elevated (admin) command prompt:

    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\MSBuild\14.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\MSBuild\12.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\MSBuild\4.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\MSBuild\3.5" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSBuild\14.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSBuild\12.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSBuild\4.0" /v DebuggerEnabled /d true
    # reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSBuild\3.5" /v DebuggerEnabled /d true


The next step is to debug the actual code that is being weaved, in my case the project was a portable class library whereby all dependancies came from NuGet so this process was super simple:

* Clone the source code of the weaver that was misbehaving
* Open the project in visual studio
* Add "your misbehaving" library to the weaver solution.
* Remove nuget references for that weaver.
* Manually add the references to the weaver from the local project.
* Save the solution.
* Follow the [Microsoft recommendations](http://blogs.msdn.com/b/visualstudio/archive/2010/07/06/debugging-msbuild-script-with-visual-studio.aspx
) and enable `Just My Code` in the Visual Studio options.


The next steps from here is to invoke msbuild manually and wait for your build to fail:

    # cd C:\Windows\Microsoft.NET\Framework\v4.0.30319>
    # msbuild.exe /debug C:\...\ghuntley-reactiveui.fody\reactiveuifody.sln

![Attach debugger to MSBuild](/images/debug-msbuild.png)

From this point, the workflow is pretty much same/same:

* Select Debug, then the version of Visual Studio you wish to use.
* Point Visual Studio to the source code of the Fody weaver.


![Now we are cooking with gas!](/images/reactiveui-fody-exception.png)

Rewind the stack and inspect the previous variables to discover the offending attribute/viewmodel.

![Offending ViewModel/Attribute found!](/images/offending-viewmodel-found.png)

Remember kids, commits are cheap and [git rebase](https://www.atlassian.com/git/tutorials/rewriting-history/git-rebase/) exists - use it. 
