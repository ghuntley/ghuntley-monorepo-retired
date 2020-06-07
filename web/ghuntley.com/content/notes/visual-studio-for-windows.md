---
title: visual studio for windows
layout: notes
---

# cool extensions
- https://marketplace.visualstudio.com/items?itemName=vsdbgplat.MicrosoftChildProcessDebuggingPowerTool
- https://marketplace.visualstudio.com/items?itemName=MukulSabharwal.ClrHeapAllocationAnalyzer
- https://marketplace.visualstudio.com/items?itemName=GrzegorzKozub.ClearRecent

# creating a extension
- https://stackoverflow.com/questions/23106519/attaching-files-in-vsix-container

# create offline installer

```
"Visual Studio Enterprise 2019 (version 16.1.1).exe" --layout "Visual Studio Enterprise 2017 (version 16.1.1)" --all --lang en-US
```

# side by side installation

The steps:

* Make a copy of one of the folders `C:\ProgramData\Microsoft\VisualStudio\Packages\_Instances`, making sure the ID does not already exist
*	Open the `state.json` file and change the value of:
  * `installationPath` to a custom folder (e.g. `C:\VS\Pro-Preview`)
  * `nickname` to a distinctive name (e.g. `Pro-Pre1`)
  * optional: [adjust versions at your own risk](https://twitter.com/mrhestew/status/1137011525342785536).
* Launch the Visual Studio Installer
* The added layout will be in error
* Click retry
* Once finished, click Repair
* VSWin will install it as configured :-)

# debugging tips

http://michaelscodingspot.com/2018/01/09/7-debugging-techniques-know-c-net/
