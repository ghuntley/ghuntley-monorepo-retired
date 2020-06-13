# UWP l10n Convention

* Create resources as follows `MyCoolApp.UWP/Strings/$lang/Resources.resw` and use the UWP resolution/translation thing to your advantage via folder structure
* Controls use the `x:Uid` markup `<TextBlock x:Uid="Username" Text="-username-" />`
* Surround my defaults with dashes, so if you ever see `-username-` on a form, you know that you forgot to generate a new string.
* Somewhere in the Resources file is a `Username.Text` entry
* in theory, it's really handy for doing RTL language translations, because they can add `Username.RightToLeft` (or whatever), and it will Just Work
