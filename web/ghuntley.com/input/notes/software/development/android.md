---
layout: notes
title: android
---

# aapt2
- https://connortumbleson.com/2018/02/19/taking-a-look-at-aapt2/


# automatically updating the sdk on windows

[AppVeyor](http://www.appveyor.com/) is a fantastic CI service for .NET and Xamarin. By default however, it does not have all Android API levels pre-installed so you will need to bootstrap the environment before kicking off a build. 

Sounds like an easy task right? Nope, as the headless installation mode provided by Google does not provide an option to automatically accept license agreements (``--accept-license``) and the SDK's will not install until a human actually types "yes" on the keyboard. If your continuous integration environment is immutable (which it should be) this presents a challenge. 

On UNIX there are [plenty of really great solutions](https://stackoverflow.com/questions/4681697/is-there-a-way-to-automate-the-android-sdk-installation) to this problem but on Windows there wasn't many options available. Below you will find the powershell script we are using for [ReactiveUI](http://www.reactiveui.net) to bootstrap AppVeyor with the appropriate Android SDKs.

```powershell
$AndroidToolPath = "${env:ProgramFiles(x86)}\Android\android-sdk\tools\android"
#$AndroidToolPath = "$env:localappdata\Android\android-sdk\tools\android"

Function Get-AllAndroidSDKs() {
    $output = & $AndroidToolPath list sdk --all
    $sdks = $output |% {
        if ($_ -match '(?<index>\d+)- (?<sdk>.+), revision (?<revision>[\d\.]+)') {
            $sdk = New-Object PSObject
            Add-Member -InputObject $sdk -MemberType NoteProperty -Name Index -Value $Matches.index
            Add-Member -InputObject $sdk -MemberType NoteProperty -Name Name -Value $Matches.sdk
            Add-Member -InputObject $sdk -MemberType NoteProperty -Name Revision -Value $Matches.revision
            $sdk
        }
    }
    $sdks
}

Function Execute-AndroidSDKInstall() {
    [CmdletBinding()]
    Param(
        [Parameter(Mandatory=$true, Position=0)]
        [PSObject[]]$sdks
    )

    $sdkIndexes = $sdks |% { $_.Index }
    $sdkIndexArgument = [string]::Join(',', $sdkIndexes)
    Echo 'y' | & $AndroidToolPath update sdk -u -a -t $sdkIndexArgument
}

Function Install-AndroidSDK
{
    param([string]$Level)

    $sdks = Get-AllAndroidSDKs |? { $_.name -like "sdk platform*API $Level*" -or $_.name -like "google apis*api $Level" }
    Execute-AndroidSDKInstall -sdks $sdks
}
```

Which we then invoke as follows:

```powershell
Install-AndroidSDK 10
Install-AndroidSDK 11
Install-AndroidSDK 12
Install-AndroidSDK 13
Install-AndroidSDK 14
Install-AndroidSDK 15
Install-AndroidSDK 16
Install-AndroidSDK 18
Install-AndroidSDK 19
Install-AndroidSDK 20
Install-AndroidSDK 21
Install-AndroidSDK 20
```
