---
title: data structures
layout: notes
---

# primitives

A primitive data type is either of the following:

- a basic type is a data type provided by a programming language as a basic building block. Most languages allow more complicated composite types to be recursively constructed starting from basic types.
- a built-in type is a data type for which the programming language provides built-in support.
The actual range of primitive data types that is available is dependent upon the specific programming language that is being used. For example, in C#, strings are a composite but built-in data type, whereas in modern dialects of BASIC and in JavaScript, they are assimilated to a primitive data type that is both basic and built-in.

Classic basic primitive types may include:

* Character (character, char);
* Integer (integer, int, short, long, byte) with a variety of precisions;
* Floating-point number (float, double, real, double precision);
* Fixed-point number (fixed) with a variety of precisions and a programmer-selected scale.
* Boolean, logical values true and false.
* Reference (also called a pointer or handle or descriptor), a value referring to another object. The reference can be a memory address, or an index to a collection of values.

More sophisticated types which can be built-in include:

* Tuple in Standard ML, Python, Scala, Swift, Elixir
* List in Common Lisp, Scheme, Haskell
* Complex number in C99, Fortran, Common Lisp, Python, D, Go
* Rational number in Common Lisp, Haskell
* Associative array in Perl, Python, Ruby, Javascript, Lua, D, Go
* First-class function, in all functional languages, Javascript, Lua, D, Go, and in newer standards of C++, Java, C#, Perl

# arrays

## fixed

## dynamic array aka resizable array

A random access, variable-size list data structure that allows elements to be added or removed. 

In .NET use `List<T>` instead of `ArrayList` as `List<T>` is a generic class which supports storing values of a specific type without casting to or from object (which would have incurred boxing/unboxing overhead when `T` is a value type in the `ArrayList` case). `ArrayList` simply stores object references. As a generic collection, `List<T>` implements the generic `IEnumerable<T>` interface and can be used easily in LINQ (without requiring any `Cast` or `OfType` call).

# dictionary

# functional
https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf

# hash table

{{< youtube MfhjkfocRR0 >}}

A [hash table](https://github.com/dotnet/corefx/blob/9cb4ea0045eed954d72aea6f283d22f2d7fb3c7b/src/Common/src/CoreLib/System/Collections/Hashtable.cs) is a collection of key/value pairs that are organized on the hash code of the key. It's a data structure that maps keys to values for highly efficient lookup. Think of it along the lines of `data to a key, mapped to an data structure (array or balanced binary search tree) to distribute this data in a performant manner, backed by a linkedlist`. A linkedlist must be used because of hashing collisions, you could have two different keys with the same hashcode or two different hash codes that map to the same index.

1. compute the the keys hashcode, typically a long or an int. for example a key of `"hi"` hashing to `10320` long or int.
2. map the hash code to an index in the array. an example of this would be `hash(key) % array_length`. Note, two different keys with the same hashcode could map to the same index.
3. at the index there is linked list of keys and values. store the keys and values in this index. At linkedlist must be used because of collisions, you could have two different keys with the same hashcode or two different hash codes that map to the same index.

To retrieve the value pair by it's key, compute the hash code from they key, and compute the index from the hash code, then search through the linked list for the value with this key.

If the number of hashing collisions is very high, the worst case runtime performance is `O(n)`, where `n` is the number of keys. If we assume a good implementation where collisions are at a min the lookup time is `O(1)`. Hashing collisions can be reduced by using a techinque called [double hashing](https://en.wikipedia.org/wiki/Double_hashing).

## collisions

# graphs

## directed and undirected

## weighted and unweighted

## cycles

## directed acyclic graphs

# linked List

## singly

## double

# stack

# trees

## binary Tree

## heap

## binary search tree

## trie


# queue

# references
- [Cracking the code interview](http://plusteach.com/digbooks/b_img/three.pdf)
- [Primitive data type](https://en.wikipedia.org/wiki/Primitive_data_type)
- [ArrayList vs List<T> in C#](https://stackoverflow.com/questions/2309694/arraylist-vs-list-in-c-sharp)
