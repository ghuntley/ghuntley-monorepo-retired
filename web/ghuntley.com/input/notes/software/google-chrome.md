---
title: Google Chrome
layout: notes
---

https://flaviocopes.com/chrome-devtools-tips/

# Extensions

## chrome.tabs

The `chrome.tabs` API is used to create, modify, and rearrange tabs in the browser. Most methods and events can be used without declaring any permissions in the extension's manifest file. However, if access to the url, title, or favIconUrl properties of tabs.Tab is required then the `tabs` permission in the manifest must be declared. For more specifics see https://developer.chrome.com/extensions/tabs
