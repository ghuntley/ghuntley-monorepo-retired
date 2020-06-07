---
layout: notes
title: React Native
---


# Developer Menu
The developer menu is disabled on production builds. Use the `⌘D` shortcut when the application is running in the iOS Simulator or `⌘M` when running in an Android emulator. Alternatively the command `adb shell input keyevent 82` will open the developer menu.

## Reload
Use `⌘R` in the iOS Simulator, or tap `R` twice on Android emulators but why do it manually when you can do it automatically? Enable automatic reloading by selecting "Enable Live Reload" from the Developer Menu.

# Exposing ObjC/Java to the NativeModules Bridge

Use `RCT_EXPORT_METHOD`

```objc
#import "CalendarManager.h"
#import <React/RCTLog.h>

@implementation CalendarManager

RCT_EXPORT_MODULE();

RCT_EXPORT_METHOD(addEvent:(NSString *)name location:(NSString *)location)
{
  RCTLogInfo(@"Pretending to create an event %@ at %@", name, location);
}
```

# Consuming ObjC/Java via the NativeModules bridge

```js
import {NativeModules} from 'react-native';
var CalendarManager = NativeModules.CalendarManager;
CalendarManager.addEvent('Birthday Party', '4 Privet Drive, Surrey');
```
# Links
- https://facebook.github.io/react-native/docs/native-modules-ios.html
