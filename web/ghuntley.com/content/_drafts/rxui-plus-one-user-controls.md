
    I've tried having the really simple UserControls not have a matching ViewModel, and I've ​_always_​ been
    burned in the long run. No matter how simple and it makes it really easy to set up - just set or
    Bind the UserControl's ViewModel property to some property on your containing view's own ViewModel

    ​ie. `this.OneWayBind(ViewModel, vm => vm.ErrorViewModel, v => v.ErrorView.ViewModel);`

* Use Matt Oswald's user control as inspiration.
* Explain the golden rule of how to use `WhenActivated`
