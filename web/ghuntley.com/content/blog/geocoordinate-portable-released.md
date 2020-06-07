---

title:      GeoCoordinate Portable Released
date:       2016-01-05
summary:    GeoCoordinate is a Portable Class Library compatible implementation of System.Device.Location.GeoCoordinate. It is an exact 1:1 API compliant implementation and will be supported until MSFT sees it fit to embed the type. Which at that point this implementation will cease development/support and you will be able to simply remove this package and everything will still work.
categories: system.devices.location geocoordinate pcl xamarin universal-windows-platform windows-phone
---
It has always irked me that `System.Device.Location.GeoCoordinate` was never included in any portable class library profiles. I mean, it's a type that is used all the freaking time when doing any form of mobile development involving mapping or location services. There's a [open ticket on UserVoice](https://visualstudio.uservoice.com/forums/121579-visual-studio-2015/suggestions/5221530-geocoordinate-class-included-in-portable-class-lib) requesting for it to be implemented, but until that happens pretty much every Xamarin/UWP developer out there is copy and pasting their own implementation/interpretation of the class between their applications.

Enough of this madness, today I'm [releasing a 1:1 exact API compliant implementation](https://github.com/ghuntley/geocoordinate) and which will be supported until MSFT sees it fit to include the type by default. The thoery is if you take this package as a dependency, when/if MSFT embeds the type you will be able to uninstall the library and everything will still work after deleting all of the `using GeoCoordinatePortable;` references.

# Supported Platforms

* Mono
* .NET 4.5
* .NET Core
* Windows Phone 8.x
* Universal Windows Platform
* Xamarin iOS
* Xamarin Android

# Installation
Installation is done via NuGet:

    PM> Install-Package GeoCoordinate
    
# Usage

    GeoCoordinate pin1 = new GeoCoordinate(lat, lng);
    GeoCoordinate pin2 = new GeoCoordinate(lat, lng);
    
    double distanceBetween = pin1.GetDistanceTo(pin2);

For more examples, refer to the MSDN reference documentation over at: [https://msdn.microsoft.com/en-us/library/system.device.location.geocoordinate(v=vs.110).aspx](https://msdn.microsoft.com/en-us/library/system.device.location.geocoordinate(v=vs.110).aspx)
