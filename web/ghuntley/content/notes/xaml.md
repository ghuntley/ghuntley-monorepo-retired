---
title: XAML
layout: notes
---

# User Controls

When working with UserControls on Xamarin Forms / WPF, values of the control should be bindable by the consumer. Here's a quick example of this in action and how to specify a default value if the consumer does not bind to the property.

```csharp
public static readonly BindableProperty IsBusyProperty =
    BindableProperty.Create("IsBusy",
        typeof(bool),
        typeof(bool),
        default(bool)); 

public bool IsBusy
{
    set { SetValue(IsBusyProperty, value); }
    get { return (bool)GetValue(IsBusyProperty); }
}

public ContractStatusCard()
{
    InitializeComponent();
}
```

In the above sample you will see that a default value is passed into the constructor of `PropertyMetadata`. If a client does not bind/specify a value then `Loading` will be default value. For more information see MSDN - [PropertyMetadata Constructor](https://msdn.microsoft.com/en-us/library/ms557329(v=vs.110).aspx).

