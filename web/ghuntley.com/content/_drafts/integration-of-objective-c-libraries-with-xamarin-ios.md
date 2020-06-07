---
title: Integration of Objective-C libraries with Xamarin iOS
date: '2016-08-01 04:43:00'
categories:
- objective-c
- xamarin ios
- pinvoke
- cocoapods
summary: ''

---
# Notifications
Uses "smart tokens" - ie nsstring instead of string. _Anything_ that starts with Notification from the objective-c libary needs to use nsstring, not string.

## Basic API
* NSNotificationCenter takes a string + method which invokes method when notification is posted that contains a NSDictionary with the notification data.

## Observing notifications

You'll find a subclass on most API's that begins with ```Observe.....``...

```csharp
var obs = NSFileHandle.Notifications.ObservReadCompleition ((sender, args) => {
     Console.WriteLine (args.AvailableData);
     Console.WriteLine (args.UnixError);
});
```

**Which need to be disposed of!**

```csharp
obs.Dispose();
```

# Distributing Bindings
* The dll contains, the public constract specification, contains the binding AND the native code. i.e. mycoollibrary.dll includes everything, including mycoollibrary.a which simplifies distribution, bundled as resources. Contents are automatically unpacked before the final build. One DLL can be fat - meaning that it can contain x86, arm and thumb code. Idealy you should build x86 (simulator) and arm (physical device) so that the library works on simulator and device. Good libaries contains both - i.e. fat library. Gotcha - the first time you drag in a native libary it will define/specify all of the platforms found in the file but if the library updates and adds say ARMv8 - then you also need to update the c# library definition.

## Smart Link
* SmartLink merges Mono and the Sytem Linker - i.e. only code that is referenced in C# AND objective-c is included in your final app. Otherwise the old behaviour is all code in objective-c and then only the code you used in c#. Caveat - dynamic objective-c code (which is rare) doesn't link well with the apple objective-c linker. Run into similar problems that can be experienced with C# linker - too greedy etc.

## Controlling linker behaviour
* Use assembly-level attribute ```[assembly:LinkWith(...)```
* See https://developer.xamarin.com/api/type/MonoTouch.ObjCRuntime.LinkWithAttribute/

```csharp
[assembly: LinkWith ("libGoogleAdMobAds.a", 
             LinkTarget.Simulator | LinkTarget.ArmV7, 
             ForceLoad = true, 
             Frameworks = "AudioToolbox MessageUI SystemConfiguration CoreGraphics MediaPlayer StoreKit", 
             WeakFrameworks = "AdSupport", 
             IsCxx = true, 
             SmartLink = true,
             LinkerFlags = "-lz -lsqlite3")]
[assembly: LinkerSafe]
```

If the framework you linking against has a dependancy, it needs to be specified in the `Frameworks` stanza. If your application/the library has an dependancy that is optional, specify it in the `WeakFrameworks` stanza. i.e. "if dept defects XXX is available, it will gracefully upgrade and provide functionality but the library will work without the dept"

If you need to pass flags to the NATIVE (i.e. objectivec) linker then specify them in the `LinkerFlags` stanza.


# Arrays
* NSArray represents arrays, they are untyped and have no code completion
* When beinging, use strong types instead i.e. "NSArray" would become "UIView []" - look at Objc documentation to work this out.

# Linking Libaries


# Core Type Mappings

| Objective-C             | C#                           |
|-------------------------|------------------------------|
| BOOL, GLBoolean         | bool                         |
| NSString *              | C# string or NSString        |
| char *                  | [PlainString] string         |
| NSInteger, NSUInteger   | int, uint                    |
| CGRect, CGPoint, CGSize | RectangleF, PointF, SizeF    |
| id                      | NSObject                     |
| SEL                     | ObjCRuntime.Selector         |
| dispatch_queue_t        | CoreFoundation.DispatchQueue |
| CGFloat, GLFloat        | float                        |

* Define NSString in C# vary rarely, used when Apple use strings as tokens - lookup in KVP dictionary, working with notifications or uitableviewcell
* ID is shorthand for NSObject.
* dispatch_queue_t is a datatype that belongs to grand central dispatch.

# Unit Testing for Correctness

Common Problems:
- Accidental setters, or wrong getters

Write tests:
- Subclass ApiCtorInitTest

```csharp
// doing this will ensure that every method that is defined can be invoked

[TestFixture]
public class BindingCtorTest : ApiCtorInitTest {
  protected override Assembly Assembly {
   get {
    return typeof (CCAccelAmplitude).Assembly; }
   }
}
```

# Objective-C Property Selectors

Assume the property ```@property (readwrite) int foo;```

Objective-C:
* The property is read/write
* The return type is int.
* The selector (pair!) is "foo" (get operations) and "setFoo:") (set operations)

In C# IDL:
```csharp
[Export ("foo")]
int Foo { get; set; }
```

# Objective-C Method Selectors

## Example 1
Assume the method ```(float) getNumber)```

Objective-C:
* "-" means it is an instance method
* The return type is float
* The selector name is "getNumber"

C# IDL:

```csharp
[Export("getNumber")]
float GetNumber();
```

## Example 2
Assumbe the following method ```+(float) add:(int) first and:(int) second;```

Selector is created by removing type declaration and paramater name, internally within Objective-C selectors are just strings.

Objective-C:
* "+" means it is a static method
* The return type is float
* Takes two int arguments
* Selector name is "add:and:"

C# IDL

```csharp
[Export("add:and:")]
float Add (int first, int second)
```

# Class Declarations

|                         | Objective-C                     | C#                                                    | Mapping Result                         |
|-------------------------|---------------------------------|-------------------------------------------------------|----------------------------------------|
| Class Declaration       | ```@interface Foo : Bar```      | ```[BaseType( typeof (Bar))] interfaceFoo```          | C# class                               |
| Class Adopting Protocol | ```@inteface Foo : Bar <Pro>``` | ```[BaseType (typeof (Bar))] interface Foo : Pro```    | C# class with inlined protocol methods |
| Protocol                | ```@protocol Foo <Bar>```       | ```[BaseType (typeof (Bar))] [Model] interface Foo``` | C# class with methods to override      |
| Category                | ```@interface Foo(Cute)```      | ```[BaseType(typeof(Foo))] interface Cute```          | C# extensions method class             |


# Extension Methods
* Also known as categories
* https://stackoverflow.com/questions/2159034/extension-methods-in-objective-c
* https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/CustomizingExistingClasses/CustomizingExistingClasses.html#//apple_ref/doc/uid/TP40011210-CH6-SW1

# Basics of Bindings
* Binding Classes
  * Exactly the same as C# classes  
* Binding Protocols
  * Are like C# interfaces, except they have optional methods (i.e. don't have to implement the entire contract) which makes them weak interfaces.
  * C# doesn't like optional methods on an inferface but can be worked around.
  
  
* Methods, PRoperties
 * Type mappings
 * Arrays
 * Strings
* Delegate classses (and events)
* Exposing Weak and Strong Types
* Binding Blocks
  * Blocks are like callbacks in C# 

# Example

```objc
typedef NS_ENUM(NSInteger, SDStatusBarManagerBluetoothState)
{
  SDStatusBarManagerBluetoothHidden = 0,
  SDStatusBarManagerBluetoothVisibleDimmed,
  SDStatusBarManagerBluetoothVisibleConnected
};

@interface SDStatusBarManager : NSObject

@property (copy, nonatomic) NSString *carrierName;
@property (copy, nonatomic) NSString *timeString;
@property (assign, nonatomic, readonly) BOOL usingOverrides;
@property (assign, nonatomic) SDStatusBarManagerBluetoothState bluetoothState;

// two instance methods
- (void)enableOverrides;
- (void)disableOverrides;

// a static method that belongs to the type
+ (SDStatusBarManager *)sharedInstance;
+
```


# Binding to Global Variables
* Bind using the `Field` attribute to c# properties

```
[Field("FooServiceKEy", "__Internal")]
NSString ServiceKEy { get; set; }
```

Supports NSArray, NSStrint, int, long, float, double, IntPtr and System.Drawing.SizeF.

# ObjectiveC
 - Constructors in objective c is done via instance methods that follow a naming convention (`initWithString(xxxx`))
 - `+(NSArray*) getAllBeans` - means that it is a static method because of the plus sign.

# Datatypes
Objective-C
.NET

* Objective-C is a dynamic lanaugage, with some type safety but the majority is untyped - arrays, dictionaries.

# Themes
* Adding C# type information to a dynamic language (Obj-C).
* Convert C _DEFINES_ into ENUMS.
* Translate Objective-C idioms into C# idoms.
* Rename Objective-C methods into methods that conform with the .NET framework design guidelines.

# Enums
* Exposed in Objective-C as constants, need converting to C# enum idoims.


# Enumerations and Structures

# Interface Definition

# Concepts
* Zero-copy marshalling (i.e. use existing pointers in memory, no need to copy data)

# Packaging
* Do not need to distribute the native `libmagicbeans.a` just distribute the `libmagicbeans.dll`

# Reading
* https://msdn.microsoft.com/en-us/library/ms229042(v=vs.110).aspx
* 
# Watching
* https://youtu.be/DV7cGR0ySgo
