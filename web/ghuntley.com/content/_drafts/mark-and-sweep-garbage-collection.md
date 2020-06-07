# Mark & Sweep Garbage Collection

## Mark Phase

### Concepts

* Block Colours (black, white, grey)
* References
* GC Root (frames, global variables)

Repeat the following process:

a) For each white block.
b) Visit all white references, mark them as grey.
c) Mark the white block from a) as Black.


## Sweep Phase

Finally find black blocks with no references.

## Generations


### Concepts

* One of the jobs of the garbage collector is seperate the heap into more than one generations.
* Generational Hypothesis (most objects die young)
* Nursey (Where small objects are born)
  * Nursey has a fixed size, allocations are very efficient and once it fills up the nursey is collected.
  * The few Objects which survive this collection process are copied/evicted into the Major collection.
* Major (Where they are copied to when they mature)
* Large Object Space (Part of major, objects above Kb, typically large arrays)
  * Part of the major generation heap, objects typically above > 8Kb
  * Moving items from Nursey to Major is cheap, moving Objects around within the Large Object Space is expensive.

## Stop The World Garbage Collection

* Nursery collection pauses are short
* Major collection pauses can take a long time
* All threads registered with the runtime are stopped, including the main run loop. Animations on iOS are not effected as they are run in a seperate process.


## Common Isssues with Xamarin
* The garbage collector <?$condition?> runs when the nursey is full. 
* The garbage collector cannot see what's behind an innocent object. For example UIImage (C#) is 32bytes but behind the scenes is a 2MB JPEG UIImage (ObjC)
  * Solution is to dispose of resources.. i.e. imageView.Dispose(), uiView.Dispose() and implement IDisposable pattern (rememebr IDisposable is not a destructor) 
* On Xamarin.iOS - Indirect Cycles
* On Xamarin.Android - Cross-Heap References

* Measure before assuming something is wrong, always, here's how to enable on Android

> adb shell setprop debug.mono.env "MONO_LOG_LEVEL=debug|MONO_LOG_MASK=gc"

### iOS

#### Indirect Cycles
* Inherited from Objective-C
* When multiple objects point to each other.
* Break/prevent these cycles by
  * Using WeakReference<T>
  * Disposing of the parent
  * Explicitly nulling the links.
* What can trigger it
  * Non-wrapper subclasses of NSObject
  * With reference count > 2

### Android
* Imposes 4k object limit when running on emulator, 50k when running on a device (if the device manufacture is nice to you)

#### Cross-heap References
* Oversharing, passing c# objects into java object. (i.e. c# array into a java arrayadapter) causes cross-heap references. Objects that live in both worlds need to be managed. Solution is to do it all from C# land.
* It is expensive for Java to see a C# object (and vice-versa)
* Performance cost of language crossing is high
* Higher Garbage Collection costs
* Your objects are effectively being mirrored, your using twice as much memory.
* Where the object was created defines ownership, for example ArrayAdapter containing C# objects means owned by Java with overhead for Java to bridge the C# objects. 
* There are special cases (numbers/strings) which are immutable where Xamarin handle this concern for you and the overhead does not exist. No way to add/tag something, this is done under the hood.

## Performance Tips
* The less you allocate, the less often the GC runs
* The less live data you have, the quicker the GC runs
* Small, short-lived objects are cheap
* Don't allocate large (>8kb ) objects that die young.
* Avoid writing to reference fields (actually, avoid having reference fields all together)
* Don't use free lists aka manually managing memory blocks - https://msdn.microsoft.com/en-us/library/ee292184(v=vs.100).aspx

## Videos

* https://www.youtube.com/watch?v=VJsmrTQWD2k

## Reading Materials
* https://www.quora.com/What-is-the-generational-hypothesis-in-the-context-of-garbage-collection
* http://www.brpreiss.com/books/opus5/html/page424.html
* http://c2.com/cgi/wiki?MarkAndSweep
* http://www.dotnetperls.com/weakreference
* http://rypress.com/tutorials/objective-c/memory-management
