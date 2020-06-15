---
title: xamarin android
layout: notes
---

## Building from source

https://www.youtube.com/watch?v=8qaQleb6Tbk&feature=youtu.be

## Approaching a Xamarin.Android Bindings Case

Originally from https://gist.github.com/JonDouglas/dda6d8ace7d071b0e8cb

### 1. Investigation

One of the best ways to investigate a problematic Xamarin.Android Binding is to first ensure you have the proper tooling available:

* Diagnostic MSBuild Output Enabled([Instructions](https://developer.xamarin.com/guides/android/troubleshooting/troubleshooting/#Diagnostic_MSBuild_Output))
* Java Decompiler([http://jd.benow.ca/](http://jd.benow.ca/))
* .NET Decompiler([https://www.jetbrains.com/decompiler/](https://www.jetbrains.com/decompiler/))
* Binding SDK Documentation
* Android API Level Documentation
* **Optional**: Beyond Compare(Or a similar tool for comparing files)

After you have all of your tools available, you can take a preliminary look at the problem at hand.

1. Build the problematic Bindings project
2. Get the full Diagnostic Build Log
3. Examine the error (You may need to look through each Build Task to get a further idea of where the problem lies)

You now have the diagnostic build output, which should already give us clues to the problem at hand. Now we can do some investigation in the Android Library(`.aar/.jar`) and various documentation.

First, let's decompile the Android Library

* If it's a `.jar`, simply drag/open the .jar in a Java Decompiler
* If it's an `.aar`, you can extract/unzip the archive and find the `classes.jar` file to open in a Java Decompiler

Now it's time for an initial look. Do you see anything out of the ordinary or closely related to the error message at hand?

Here are a couple of things that come to mind:

* Are there any classes that have characteristics of obfuscation? (only lowercase letters/numbers/$)
**EX:** `a.class / a$.class`

* Are there any `import` statements of libraries not referenced?
* What respective versions of dependencies does the Binding SDK use?
* What Android API level does the `.jar/.aar` support?
* What version of Java/JDK was this library compiled with?


### 2. Fixing Issues

#### Pick your tooling

There are now two different `AndroidClassParser` available to use with binding projects:

1. `jar2xml` which uses Java reflection to extract types and members from a `.jar` file

2. `class-parse` which parses Java bytecode directly

You can set the respective parser via the `<AndroidClassParser>` MSBuild property inside your csproj:

i.e.

- `<AndroidClassParser>class-parse</AndroidClassParser>` - Would turn on Class Parse
- `<AndroidClassParser>jar2xml</AndroidClassParser>` - Would turn on jar2xml

*Note*: If you do not specify a value, it will default to `jar2xml` as it's more stable.

https://developer.xamarin.com/guides/android/under_the_hood/build_process/#Binding_Project_Build_Properties

#### Investigate the `api.xml` File

The `api.xml` file is typically found in the `obj\Debug` folder of the Bindings project. This will be an XML definition of the API at hand. This is a great starting place to see what is currently being generated, and what can be missing. It also gives a reference to other generated classes/types that can help assist you when you're fixing `Metadata.xml`.

#### Missing References

* If the library already exists on NuGet, simply download the NuGet package to the Bindings project. (Support libraries / etc)
* Otherwise add the missing library to your bindings project as a `ReferenceJar`, `EmbeddedReferenceJar`, or `LibraryProjectZip`

#### Java Version Mismatch

Sometimes types will not be generated or unexpected crashes may occur because you are using either a newer or older version of Java compared to what the library was compiled with. Ensure that the JDK Version is the same or compatible with the library.

### Common Paths

* `/interface` **EX:** `/interface[@name='AuthListener']`
* `/class` **EX:** `/class[@name='MapView']`
* `/method` **EX:** `/method[@name='setTileSource']`
* `/method(with parameters)` **EX:** `/method[@name=``onCreate``and count(parameter)=2 and parameter[1][@type=``com.my.CustomActivity``] and parameter[2][@type=``android.os.Bundle``]]"`
* `/parameter` **EX:** `/parameter[@name='p0']` 
* `/parameter(with type)` **EX:** `parameter[1][@type=``com.my.CustomActivity``]`

### Common Names

* `name="managedType"` - **EX:** `Java.Lang.Object`
* `name="obfuscated"` - Changes the obfuscation **EX:** `true` / `false`
* `name="managedName"` - Changes the managed name **EX:** `MyCSharpName`
* `name="propertyName"` - Changes the property name **EX:** `MyPropertyName`
* `name="managedReturn"` - Changes the managed return type **EX:** `Java.Lang.Object`
* `name="argsType"` - changes the argument type **EX:** `MyCustomErrorEventArgs`
* `name="sender"` - Changes which parameter of a method should be the sender parameter when it's mapped to an event **EX:** `true` / `false`
* `name="eventName"` - Changes the event name **EX:** `MyEventName`

#### Missing Types / Obfuscated Types

Typically we will see characteristics of obfuscated types in our respective `.jar/.aar` libraries and we must unobfuscate them for the Bindings Generator to generate the respective C# types.

```
 <attr path="/api/package[@name='{package_name}']/class[@name='{name}']" name="obfuscated">false</attr>
```

[See Common Paths for more types](#common-paths)

#### Duplicate Names or Normalizing Names

Sometimes you'll run into duplicate `managedNames` or you might need to normalize your generated C# classes for sanity reasons.

```
<attr path="/api/package[@name='{package_name}']/class[@name='{name}']" name="managedName">NewManagedName</attr>
```

[See Common Paths for more types](#common-paths)

#### Class Visibility

Your class might not have the proper visibility for the Bindings Generator to traverse through as it does not generate bindings for non-public classes or derived classes. Typically switching the visibility to `public` fixes this.

```
<attr path="/api/package[@name='{package_name}']/class[@name='{name}']" name="visibility">public</attr>
```

[See Common Paths for more types](#common-paths)

#### Adding Types

You can use `<add-node>` to add just about anything to your binding which will generate in the `api.xml` file. Typically you may want to add a class, change a constructor, or switch a generic type.

**EX:** (Creates a class with a constructor and field):

```
  <add-node path="/api/package[@name='org.alljoyn.bus']">
    <class abstract="false" deprecated="not deprecated" final="false" name="AuthListener.AuthRequest" static="true" visibility="public" extends="java.lang.Object">
      <constructor deprecated="not deprecated" final="false" name="AuthListener.AuthRequest" static="false" type="org.alljoyn.bus.AuthListener.AuthRequest" visibility="public" />
      <field name="p0" type="org.alljoyn.bus.AuthListener.Credentials" />
    </class>
  </add-node>
```

#### Removing Types

Typically it's easiest to just remove anything in a binding that we will not use. You can look at the class that you want to use and see everything it references to get a better idea of what you will need and what you will not.

```
<remove-node path="/api/package[@name='{package_name}']/class[@name='{name}']" />
```

[See Common Paths for more types](#common-paths)

#### Common Metadata Fixes

[https://gist.github.com/brendanzagaeski/c32d65c21e152799af69](https://gist.github.com/brendanzagaeski/c32d65c21e152799af69)

[https://gist.github.com/brendanzagaeski/6d1052a8b76f9067548e](https://gist.github.com/brendanzagaeski/6d1052a8b76f9067548e)

[https://gist.github.com/brendanzagaeski/69f490e31ca6a71136ff](https://gist.github.com/brendanzagaeski/69f490e31ca6a71136ff)

[https://gist.github.com/brendanzagaeski/3868e30b85a1feb1181b](https://gist.github.com/brendanzagaeski/3868e30b85a1feb1181b)

[https://gist.github.com/brendanzagaeski/9607158](https://gist.github.com/brendanzagaeski/9607158)

#### Using Java Annotations

1. Be sure to `[Export]` the respective Method/Class/etc.
2. Also ensure you reference Mono.Android.Export in your Xamarin.Android Project

[https://developer.xamarin.com/api/type/Java.Interop.ExportAttribute/](https://developer.xamarin.com/api/type/Java.Interop.ExportAttribute/)

#### XPath Customization

You can use up to XPath 1.0 features within `Metadata.xml` transformations. There are quite a few great resources out there for explaining different functions, operators, etc.

[Mono Recommended 1](http://www.digilife.be/quickreferences/QRC/XML%20Path%20Language%201.0.pdf)

[Mono Recommended 2](http://www.digilife.be/quickreferences/QRC/XSLT%20and%20XPath%20Quick%20Reference.pdf)

[Mono Recommended 3](https://www.w3.org/TR/xpath/)

There are other general tutorials on XPath that you can find here.

[XPath Tutorial](https://www.tutorialspoint.com/xpath/index.htm)

[XPath Functions](https://developer.mozilla.org/en-US/docs/Web/XPath/Functions)

Quick examples:
```
 <!-------Remove nodes in @name that match com.example.internal----->
<remove-node path="/api/package[starts-with (@name, 'com.example.internal')]" />

 <!-------Remove nodes in @name that contain the string com.example.internal----->
<remove-node path="/api/package[contains (@name, 'com.example.internal')]" />
```

### 3. Terms

#### JNI (Java Native Interface)

In computing, the Java Native Interface (JNI) is a programming framework that enables Java code running in a Java Virtual Machine (JVM) to call and be called by native applications (programs specific to a hardware and operating system platform) and libraries written in other languages such as C, C++ and assembly.

#### Android Callable Wrappers (ACW)

Android callable wrappers are a JNI bridge that are used whenver the Android runtime needs to invoke managed code.

#### Managed Callable Wrappers (MCW)

Managed callable wrappers are a JNI bridge that are used whenever managed code needs to invoke Android code and provide support for overriding virtual methods and implementing Java interfaces.

#### Embedded vs. Non-Embedded

When using a `Build Action` such as `EmbeddedJar` or `EmbeddedReferenceJar`, it will embed the respective library into the .apk so it will be available at runtime.

Otherwise it is expected that either the Device or the application will provide the .jar at runtime. (I.E. It is already loaded on device or will be provided via a download/etc)

#### Reference vs. Non-Reference

When using a `Build Action` such as `ReferenceJar` or `EmbeddedReferenceJar`, it will not generate Manage Callable Wrappers(ACW) and will not be exposed to the client.

#### Java is not the same as C# #

Because of this limitation, you will need to be aware of the respective generated C# code as there might be certain things that the languages handle differently.

**EX: Java -> C#**

* get/set methods -> properties
* fields -> properties
* listeners -> events
* static nested class -> nested class
* inner class -> nested class with an instance constructor


### 4. Conclusion

Although Xamarin.Android Binding errors might be confusing and the JNI might be intimidating, there is always a few ways to work around the issue at hand.

**Documentation:**

[Binding a Jar](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/binding-a-jar/)

[Binding a Library Project](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/binding-a-library-project/)

[Java Bindings Metadata](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/java-bindings-metadata/)

[Mono Metadata](http://www.mono-project.com/docs/gui/gtksharp/gapi/#metadata)

[Creating Bindings Using Metadata](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/creating-bindings-using-metadata/)

[Naming Parameters With Javadoc](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/naming-parameters-with-javadoc/)

[Troubleshooting](https://developer.xamarin.com/guides/android/advanced_topics/java_integration_overview/binding-a-java-library/troubleshooting-bindings/)

[XPath Tutorial](https://www.tutorialspoint.com/xpath/index.htm)

[XPath Functions](https://developer.mozilla.org/en-US/docs/Web/XPath/Functions)

**Xamarin Univeristy Course:**

[https://university.xamarin.com/classes/track/xamarin-android#and450-binding](https://university.xamarin.com/classes/track/xamarin-android#and450-binding)
