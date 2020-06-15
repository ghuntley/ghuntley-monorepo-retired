---
title: big o notation and time complexity
layout: notes
---

![](/images/big-o%20complexity%20chart.png)

# exponent

the exponent is a quantity representing the power to which a given number or expression is to be raised, usually expressed as a raised symbol beside the number or expression (e.g. 3 in 23 = 2 × 2 × 2).


* `linear time O(n)`
* `constant time O(1)`
* `quadratic time O(n2)`

1. find the fastest growing term
2. take out the coefficient

# constant time aka o(1)

a situation that does not change.
occurring continuously over a period of time.

```csharp
public int Add(int a, int b)
{
  return a + b
}
```

# linear time o(n)

linear is a straight line

```csharp
{

}
```

# resources
* https://www.youtube.com/watch?v=D6xkbGLQesk
* https://en.wikipedia.org/wiki/Time_complexity
* http://www.bigocheatsheet.com/
