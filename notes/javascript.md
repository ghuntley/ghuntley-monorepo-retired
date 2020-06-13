---
layout: notes
title: JavaScript
---

# event loop
https://twitter.com/jaffathecake/status/961980260194684928

# Operators

## Spread
The spread operator allows an iterable such as an array expression or string to be expanded in places where zero or more arguments (for function calls) or elements (for array literals) are expected, or an object expression to be expanded in places where zero or more key-value pairs (for object literals) are expected.

```js
function sum(x, y, z) {
  return x + y + z;
}

const numbers = [1, 2, 3];

console.log(sum(...numbers));
// expected output: 6

console.log(sum.apply(null, numbers));
// expected output: 6
```

# ES2015
## Modules
* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
