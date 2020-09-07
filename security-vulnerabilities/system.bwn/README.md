# System.Bwn

The evil cousin of the super fast and super secure by default framework by Ben Adams that uploads stuff to the internet every time you open visual studio.

## Installation

Create a brand new .NET core application via -> File New Project in Visual Studio. Then install the super insecure package.

> Install-Package System.Bwn -Version 1.0.0

**Warning:** This will upload your environment variables and .NET Core UserSecrets ([which are unencrypted](https://docs.microsoft.com/en-us/aspnet/core/security/app-secrets)) to the internet, not only upon installation but every time Visual Studio opens a project that has a reference to the package.

## Disclosure Timeline

* 2nd Aug - [Typosquatting attacks in the npm/javascript ecosystem harvested stored credentials](https://www.theregister.co.uk/2017/08/02/typosquatting_npm/).
* 06th Sept - Discovered vector similar to the one used in the npm/javascript ecosystem. Reported to MSRC as case 40612. Created System.Bwn and published to NuGet as unlisted package.
* 14th Sept - Microsoft disables the ability to publish new packages to the System.* or Microsoft.* or Windows.* namespaces on NuGet except for whitelisted accounts (i.e. MSFT product teams) to mitigate against typosquatting attacks.
* 14th Sept - [NuGet publishes blog post on package signing, policies and package restrictions](https://blog.nuget.org/20170914/NuGet-Package-Signing.html).
* 20th Sept - Case closed by MSRC and public disclosure.
