using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace WeakEventListener.Tests
{
    public class Fixtures
    {
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

        [Fact]
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
    }
}
