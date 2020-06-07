---
title: Creating a Cake (C# Make) addin
date: '2016-07-04 00:00:00'

draft: true
---
# Core-concepts

core concepts for creating a cake addin - a) cake aliases b) cake context is just a bunch of extension methods c) toolsettings d) toolrunners e) testing c+d without invoking tool.

# Add-in
* Explain how `Aliases`, `Runner`, `Settings` works.
* Explain how to auto-import your namespaces into Cake.
* You may need a base `Settings` and define child settings i.e. `"app.exe --globalSettings run --runSettings"` for the TaskRunner

# Testing
* How to set the console output in a fixture so the runner succeeds without having to actually run the tool.
* Fixtures can have properties, use these to set command line arguments.
## Test Cases
* Check tool fails if not found.
* Check tool succeeds if tool is found.
* Check tool fails on non 0 return code. (failure)
* Check tool succeeds on 0 return code. (success)
* Check command line arguments are set.
* Check settings are applied to command line arguments.


## Packages
* Install `Cake.Testing`
* Install `Xunit`


# How to add the addin to the website gallery

https://github.com/cake-build/website/pull/107

