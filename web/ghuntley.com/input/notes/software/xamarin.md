---
title: xamarin
layout: notes
---

When working in a corporate environment I usually have my Macbook Pro setup as follows:


	+--------------------+    USB PASS-THROUGH INTO VIRTUAL MACHINE
	|                    | <------------------------------------------+
	|                    |    WHEN DOING UWP DEVELOPMENT              |
	|                    |                                            |
	|   VIRTUAL MACHINE  |                                            |
	|                    |                                            |
	|                    |           MACBOOK PRO LAPTOP               |
	|                    | +-----------------------------------+      |
	|                    | |                                   |      |
	|                    | |                                   |      |
	|   CORPORATE SOE    | |                                   |      |
	|   WINDOWS 7/8/10   | |              MAC OSX              | +----+---+
	|   VISUAL STUDIO    | |          XAMARIN STUDIO           | |        |
	|                    | |                                   | |        |
	|    UWP UI & PCL    | |        IOS UI & ANDROID UI        | |        |
	|                    | |                                   | |        |
	|                    | |                                   | | MOBILE |
	|                    | |                                   |<+        |
	|                    | |                                   | |        |
	|                    | |                                   | |        |
	|                    | |                                   | |        |
	|                    | |                                   | |        |
	+---------^----------+ +--------------------^--------------+ +---^----+
	          |                                 |                    |
	+---------+----------+ +--------------------+--------------------+----+
	|    CORP NETWORK    | |              DIRECT INTERNET                 |
	|  USB ETH ADAPTER   | |           INBUILT WIFI ADAPTER(s)            |
	+--------------------+ +----------------------------------------------+

By using an Apple USB Network Adapter ([which does work on Windows](/archive/2015/12/14/apple-usb-network-drivers-for-windows/)) and using the USB pass-through feature of VMWare Workstation it is possible for a single machine to be connected to two different networks without voilating your IT security policy. 

When I need direct internet access on the virtual machine then that is achieved by disconnecting the adapter and temporarily changing the virtual network adapter mode from host to bridged/nat. 

This setup allows the best of both worlds, simultaneous direct internet access and access to corporate resources such as bug trackers, source control, email, intranet and timesheet systems.

I find some form of direct internet connectivity is an absolute must when doing mobile development otherwise productivty tends to go out the window. If you are in the unfortunate circumstance whereby you have to use a Windows NTLM based authentication proxy server on Mac OSX then I suggest reading [this article for a solution](/archive/2015/12/15/to-view-this-page-you-need-to-login-to-the-proxy-server/). 


# iOS

## Globally change appearance of elements

https://docs.microsoft.com/en-us/xamarin/ios/user-interface/ios-ui/introduction-to-the-appearance-api

## Change colour of the status bar

```csharp
UIView statusBar = UIApplication.SharedApplication.ValueForKey(new NSString("statusBar")) as UIView;
if (statusBar.RespondsToSelector(new ObjCRuntime.Selector("setBackgroundColor:")))
{
    statusBar.BackgroundColor = UIColor.Black;
}
```

```xml
<key>UIViewControllerBasedStatusBarAppearance</key>
<false/>
<key>UIStatusBarStyle</key>
<string>UIStatusBarStyleBlackOpaque</string>
```

## example of xamarin android with cake

```csharp
#addin "Cake.Xamarin"
#addin "Cake.FileHelpers"
#addin "Cake.AndroidAppManifest"

var manifestFile = File("./src/MyCoolApplication.Droid/Properties/AndroidManifest.xml");
var assemblyInfoFile = File("./src/MyCoolApplication.Droid/Properties/AssemblyInfo.cs");
var projectFile = File("./src/MyCoolApplication.Droid/MyCoolApplication.Droid.csproj");
var solutionFile = File("./src/MyCoolApplication.sln");

// should MSBuild treat any errors as warnings.
var treatWarningsAsErrors = "false";

// Parse release notes
var releaseNotes = ParseReleaseNotes("./RELEASENOTES.md");

// Get version
var version = releaseNotes.Version.ToString();
var epoch = (long)(DateTime.UtcNow - new DateTime(1970, 1, 1)).TotalSeconds;
var semVersion = string.Format("{0}.{1}", version, epoch);

Task("Build")
    .IsDependentOn("RestoreComponents")
    .IsDependentOn("RestorePackages")
    .IsDependentOn("UpdateAssemblyInfo")
    .IsDependentOn("UpdateAndroidManifest")
    .Does (() =>
{
    var keyStore = EnvironmentVariable("ANDROID_KEYSTORE");
    if (string.IsNullOrEmpty(keyStore))
    {
        throw new Exception("The ANDROID_KEYSTORE environment variable is not defined.");
    }

    var keyStoreAlias = EnvironmentVariable("ANDROID_KEYSTORE_ALIAS");
    if (string.IsNullOrEmpty(keyStoreAlias))
    {
        throw new Exception("The ANDROID_KEYSTORE_ALIAS environment variable is not defined.");
    }
    
    var keyStorePassword = EnvironmentVariable("ANDROID_KEYSTORE_PASSWORD");
    if (string.IsNullOrEmpty(keyStorePassword))
    {
        throw new Exception("The ANDROID_KEYSTORE_PASSWORD environment variable is not defined.");
    }


    DotNetBuild(projectFile, settings =>
        settings.SetConfiguration("Debug")
            .WithTarget("SignAndroidPackage")
            .WithProperty("DebugSymbols", "true")
            .WithProperty("DebugType", "Full")
            .WithProperty("OutputPath", "bin/Debug/")
            .WithProperty("TreatWarningsAsErrors", treatWarningsAsErrors));

    // For more information about MSBuild properties and how they function, read:
    // https://developer.xamarin.com/guides/android/under_the_hood/build_process/

    DotNetBuild(projectFile, settings =>
        settings.SetConfiguration("Release")
            .WithTarget("SignAndroidPackage")
            .WithProperty("AndroidKeyStore", "true")
            .WithProperty("AndroidSigningStorePass", keyStorePassword)
            .WithProperty("AndroidSigningKeyStore", keyStore)
            .WithProperty("AndroidSigningKeyAlias", keyStoreAlias)
            .WithProperty("AndroidSigningKeyPass", keyStorePassword)
            .WithProperty("DebugSymbols", "false")
            .WithProperty("OutputPath", "bin/Release/")
            .WithProperty("TreatWarningsAsErrors", treatWarningsAsErrors));
});


Task("UpdateAndroidManifest")
    .Does (() =>
{
    var manifest = DeserializeAppManifest(manifestFile);
    manifest.VersionName = semVersion;
    manifest.VersionCode = Int32.Parse(version.Replace(".", string.Empty) + epoch.ToString().Substring(epoch.ToString().Length - 6));

    SerializeAppManifest(manifestFile, manifest);
});

Task("UpdateAssemblyInfo")
    .Does (() =>
{
    CreateAssemblyInfo(assemblyInfoFile, new AssemblyInfoSettings() {
        Product = "Geoffrey Huntley",
        Version = version,
        FileVersion = version,
        InformationalVersion = semVersion,
        Copyright = "Copyright (c) Geoffrey Huntley"
    });
});

Task("RestoreComponents")
    .Does(() =>
{
    var username = EnvironmentVariable("XAMARIN_USERNAME");
    if (string.IsNullOrEmpty(username))
    {
        throw new Exception("The XAMARIN_USERNAME environment variable is not defined.");
    }

    var password = EnvironmentVariable("XAMARIN_PASSWORD");
    if (string.IsNullOrEmpty(password))
    {
        throw new Exception("The XAMARIN_PASSWORD environment variable is not defined.");
    }

    RestoreComponents(solutionFile, new XamarinComponentRestoreSettings()
    {
        ToolPath = "./tools/xamarin-component/xamarin-component.exe",
        Email = username,
        Password = password
    });
});

Task("RestorePackages")
    .Does (() =>
{
    NuGetRestore(solutionFile);
});

RunTarget("Build");
```

## example of xamarin ios with cake

```csharp

// needed to get 'dynamic' to work (Cake.Plist)
#reference "Microsoft.CSharp.dll"

// then run it with the '-experimental' flag parameter, e.g. 
// .\build.ps1 -script mycoolapp.iOS.cake -experimental

#addin "Cake.Xamarin"
#addin "Cake.FileHelpers"
#addin "Cake.Plist"

var assemblyInfoFile = File("./src/CommonAssemblyInfo.cs");
var plistFile = File("./src/MyCoolApplication.iOS/Info.plist");
var projectFile = File("./src/MyCoolApplication.iOS/MyCoolApplication.iOS.csproj");
var solutionFile = File("./src/MyCoolApplication.sln");

// should MSBuild treat any errors as warnings.
var treatWarningsAsErrors = "false";

// Parse release notes
var releaseNotes = ParseReleaseNotes("./RELEASENOTES.md");

// Get version
var version = releaseNotes.Version.ToString();
var epoch = (long)(DateTime.UtcNow - new DateTime(1970, 1, 1)).TotalSeconds;
var semVersion = string.Format("{0}.{1}", version, epoch);

Task("Build")
    .IsDependentOn("RestoreComponents")
    .IsDependentOn("RestorePackages")
    .IsDependentOn("UpdateAssemblyInfo")
    .IsDependentOn("UpdateApplePlist")
    .Does (() =>
{

    DotNetBuild(projectFile, settings =>
      settings.SetConfiguration("Debug")
          .WithProperty("Platform", "iPhoneSimulator")
          .WithProperty("OutputPath", "bin/Simulator/")
          .WithProperty("TreatWarningsAsErrors", treatWarningsAsErrors));

    DotNetBuild(projectFile, settings =>
      settings.SetConfiguration("AppStore")
          .WithProperty("Platform", "iPhone")
          .WithProperty("OutputPath", "bin/AppStore/")
          .WithProperty("TreatWarningsAsErrors", treatWarningsAsErrors));
});

Task("UpdateApplePlist")
    .Does (() =>
{
    dynamic plist = DeserializePlist(plistFile);

    plist["CFBundleShortVersionString"] = version;
    plist["CFBundleVersion"] = semVersion;

    SerializePlist(plistFile, plist);
});

Task("UpdateAssemblyInfo")
    .Does (() =>
{
    CreateAssemblyInfo(assemblyInfoFile, new AssemblyInfoSettings() {
        Product = "Geoffrey Huntley",
        Version = version,
        FileVersion = version,
        InformationalVersion = semVersion,
        Copyright = "Copyright (c) Geoffrey Huntley"
    });
});

Task("RestoreComponents")
    .Does(() =>
{
    var username = EnvironmentVariable("XAMARIN_USERNAME");
    if (string.IsNullOrEmpty(username))
    {
        throw new Exception("The XAMARIN_USERNAME environment variable is not defined.");
    }

    var password = EnvironmentVariable("XAMARIN_PASSWORD");
    if (string.IsNullOrEmpty(password))
    {
        throw new Exception("The XAMARIN_PASSWORD environment variable is not defined.");
    }

    RestoreComponents(solutionFile, new XamarinComponentRestoreSettings()
    {
        ToolPath = "./tools/xamarin-component/xamarin-component.exe",
        Email = username,
        Password = password
    });
});

Task("RestorePackages")
    .Does (() =>
{
    NuGetRestore(solutionFile);
});

RunTarget("Build");
```
