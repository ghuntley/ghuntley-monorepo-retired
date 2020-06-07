---
title: RegEx
layout: notes
---

# Don't forget the carrot and the stick

This aection was authored by http://www.secretgeek.net/regex_cs 



When using a regular expression for validation.... don't forget the caret^ and the $tick

Let's say, you want to make sure the user types in a 3 digit number.

Simple! Can't be simpler:

`\d\d\d`

Let's test if this suits the following:

```
"500" -- Yep!
"999" -- Yep!
"123" -- Yep!
```

All good so far. Let's check that it fails on the following:

```
"5" -- Yep!
"99" -- Yep!
"ABC" -- Yep!
"" -- Yep!
```

That's good too. I guess we're done, right?

Uh-oh! It looks like we forget the caret^ and the $tick!

Therefore the following would return a match!

`"HEY I like the film 2001 a space odyssey!" -- Oops! matches our expression!`

as would

`"But 2010 was just junk right?" -- Oops! another match!`

What if we use a caret "^" and a dollar sign "$" to anchor the expression to the start and end...

```
^\d\d\d$
```

Now we'll get the result we want!

So when using a regular expression for validation, don't forget to anchor them to the front and the back, by using... you guessed it... the caret^ and the $tick.
