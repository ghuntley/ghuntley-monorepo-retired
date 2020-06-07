---
title: PowerShell
layout: notes
---

# Approved Verbs

https://msdn.microsoft.com/en-us/library/ms714428(v=vs.85).aspx

# Params

Usage `.\build.ps1 -version 1.0.0`

```ps
param(
    [Parameter(Mandatory = $true)]
    [string] $version
)
```

A PowerShell function can have different parameters depending on how it is called. This is called `ParameterSet` Simon has detailed them over at http://blog.simonw.se/powershell-functions-and-parameter-sets/

# Strict Mode

```
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$PSDefaultParameterValues['*:ErrorAction']='Stop'
```

# Variables

```
Set-Variable -Name "GitRepository" -Visibility Private -Value (Get-Location)
Set-Variable -Name "TemporaryArchiveFileName" -Visibility Private -Value ([System.IO.Path]::GetTempFileName().Split('.')[0]
```

# Functions

```
function Zip-File( $zipfilename, $sourcedir )
{
   Add-Type -Assembly System.IO.Compression.FileSystem
   $compressionLevel = [System.IO.Compression.CompressionLevel]::Optimal
   [System.IO.Compression.ZipFile]::CreateFromDirectory($sourcedir,
        $zipfilename, $compressionLevel, $false)
}
```

# Filesystem Operations

Creating a folder:

```
New-Item -ItemType Directory -Force -Path $OutputFolder
```

Moving a file:

```
Move-Item -Force -Path $TemporaryArchiveFileName -Destination $BuildArtifact
```
