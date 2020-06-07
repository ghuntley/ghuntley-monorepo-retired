---
title: golang
---

# structs

structs are typed collections of fields. They’re useful for grouping data together to form records. 

```go
type Person struct {
	GivenName string
	Surname   string
	Age       int
}
```

Instances of the struct can be created in a variety of ways

```go
var p Person
```

Above will create a local Person variable that is by default set to zero. For a struct zero means each of the fields is set to their corresponding zero value (0 for ints, 0.0 for floats, "" for strings, nil for pointers, ...)

```go
p := new(Person)
```

Above allocates memory for all the fields, sets each of them to their zero value and returns a pointer `*Person`. More often we want to give each of the fields a value. We can do this in two ways. Like this:

```go
p := Person{GivenName: "Geoffrey", Surname: "Huntley", Age: 18}
```

Field names can be left off if you know the order that they are defined

```go
p := Person{"Geoffrey", "Huntley", 18}
var p Person
```

Structs are mutable

```go
p := Person{GivenName: "Geoffrey", Surname: "Huntley", Age: 18}
p.GivenName = "Geoff"
```
