---
title: Xamarin.UITest cheat sheet
date: '2016-08-11 01:28:00'
categories:
- xamarin uitest
- xamarin ios
- xamarin forms
- xamarin android
- dotnet
summary: ''

---
## Checkboxes

```
// ios
app.Query(x => x.Marked("MyCoolCheckbox")).Invoke("isOn").Value<int>());

// android
app.Query(x => x.Marked("MyCoolCheckbox")).Invoke("isChecked").Value<bool>());

// forms - tba
```
