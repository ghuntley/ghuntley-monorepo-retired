![Icon](https://i.imgur.com/fnAialR.png)
# WeakEventListener

The WeakEventListener allows the owner to be garbage collected if its only remaining link is an event handler. See https://docs.microsoft.com/en-us/dotnet/framework/wpf/advanced/weak-event-patterns for more information; the wisdom found on that page is applicable for other platforms inc UWP, Xamarin.Mac, Xamarin.iOS & Xamarin.Android.

# Supported Platforms

* netstandard v1.1 (and up)

# Installation
Installation is done via NuGet:

    PM> Install-Package WeakEventListener

# Usage

```csharp
public class SampleClass
{
    public event EventHandler<EventArgs> Raisevent;

    public void DoSomething()
    {
        OnRaiseEvent();
    }

    protected virtual void OnRaiseEvent()
    {
        Raisevent?.Invoke(this, EventArgs.Empty);
    }
}

public void Test_WeakEventListener_Events()
{
    bool isOnEventTriggered = false;
    bool isOnDetachTriggered = false;

    SampleClass sample = new SampleClass();

    WeakEventListener<SampleClass, object, EventArgs> weak = new WeakEventListener<SampleClass, object, EventArgs>(sample);
    weak.OnEventAction = (instance, source, eventArgs) => { isOnEventTriggered = true; };
    weak.OnDetachAction = (listener) => { isOnDetachTriggered = true; };

    sample.Raisevent += weak.OnEvent;

    sample.DoSomething();
    Assert.True(isOnEventTriggered);

    weak.Detach();
    Assert.True(isOnDetachTriggered);
}
```
