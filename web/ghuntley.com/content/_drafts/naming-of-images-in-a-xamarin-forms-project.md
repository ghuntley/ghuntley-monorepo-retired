---
title: Naming of Images in a Xamarin Forms project
date: '2016-07-04 06:35:00'

draft: true
---
# Rules of thumb

https://developer.xamarin.com/guides/xamarin-forms/working-with/images/

beachImage.Source =  Device.OnPlatform(
            iOS: ImageSource.FromFile("Images/waterfront.jpg"),
            Android:  ImageSource.FromFile("waterfront.jpg"),
            WinPhone: ImageSource.FromFile("Images/waterfront.png"));

NOTE: as discussed above, to use the same image filename across all platforms the name must be valid on all platforms. Android drawables have naming restrictions – only lowercase letters, numbers, underscore, and period are allowed – and for cross-platform compatibility this must be followed on all the other platforms too. The example filename **waterfront.png** follows the rules, but examples of invalid filenames include "water front.png", "WaterFront.png", "water-front.png", and "wåterfront.png". 

# Embed using Resource Dictionaries
https://developer.xamarin.com/guides/xamarin-forms/working-with/images/


# Android

Note: Place all your launcher icons in the res/mipmap-[density]/ folders, rather than the res/drawable-[density]/ folders. The Android system retains the resources in these density-specific folders, such as mipmap-xxxhdpi, regardless of the screen resolution of the device where your app is installed. This behavior allows launcher apps to pick the best resolution icon for your app to display on the home screen. For more information about using the mipmap folders, see Managing Projects Overview.



```
res/layout/my_layout.xml              // layout for normal screen size ("default")
res/layout-large/my_layout.xml        // layout for large screen size
res/layout-xlarge/my_layout.xml       // layout for extra-large screen size
res/layout-xlarge-land/my_layout.xml  // layout for extra-large in landscape orientation

res/drawable-mdpi/graphic.png         // bitmap for medium-density
res/drawable-hdpi/graphic.png         // bitmap for high-density
res/drawable-xhdpi/graphic.png        // bitmap for extra-high-density
res/drawable-xxhdpi/graphic.png       // bitmap for extra-extra-high-density

res/mipmap-mdpi/my_icon.png         // launcher icon for medium-density
res/mipmap-hdpi/my_icon.png         // launcher icon for high-density
res/mipmap-xhdpi/my_icon.png        // launcher icon for extra-high-density
res/mipmap-xxhdpi/my_icon.png       // launcher icon for extra-extra-high-density
res/mipmap-xxxhdpi/my_icon.png      // launcher icon for extra-extra-extra-high-density
```

Note: The mipmap-xxxhdpi qualifier is necessary only to provide a launcher icon that can appear larger than usual on an xxhdpi device. You do not need to provide xxxhdpi assets for all your app's images

* https://stackoverflow.com/questions/10785281/android-standards-for-prefixing-files-in-the-res-directory-apart-from-ic
* https://developer.android.com/guide/practices/ui_guidelines/icon_design.html
* http://iconhandbook.co.uk/reference/chart/android/
* https://petrnohejl.github.io/Android-Cheatsheet-For-Graphic-Designers/
* https://developer.android.com/guide/practices/screens_support.html

# iOS
* https://github.com/dkhamsing/ios-asset-names
* http://iconhandbook.co.uk/reference/chart/ios/