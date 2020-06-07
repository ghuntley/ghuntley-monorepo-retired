---
layout: notes
title: TypeScript
---

The best way to figure out why a feature was implemented in TypeScript is to read the [What's new in TypeScript](https://github.com/Microsoft/TypeScript/wiki/What%27s-new-in-TypeScript) page on GitHub and work backwards.

# Configuration

## Aliasing

Use `compilerOptions.paths` to define aliases in your `tsconfig.json`. Use these aliases instead of the physical file location in your code and you'll be able to abstract your software aware from where it sits on the filesystem. This is important as it allows effortless refactoring and restructing of your code.

```js
{
    "compilerOptions": {
        "paths": {
            "@shared-framework": ["../shared-framework"],
            "@shared-framework": ["../shared-framework/*"],
            "@components": ["src/components"],
            "@components/*": ["src/components/*"],
            "@store": ["src/redux"],
            "@store/*": ["src/redux/*"],
            "@apps-config": ["src/applications/apps-config"],
            "@assets": ["src/assets"],
            "@utils": ["src/utils"],
            "redux": ["node_modules/redux"],
            "redux-thunk": ["node_modules/redux-thunk"],
            "native-base": ["typings/native-base"],
            "react-native-linear-gradient": ["typings/react-native-linear-gradient"],
            "moment": ["node_modules/moment"],
            "query-string": ["node_modules/query-string"],
            "deep-equal": ["node_modules/deep-equal"]
        }
}
```



# Asynchrony

`Async` and `Await` was added in version 1.7. Asynchronous functions are prefixed with the `async` keyword; `await` suspends the execution until an asynchronous function that returns the promise is fulfilled and unwraps the value from the Promise returned.

# Arrow Functions

Lambdas

```ts
var foo = () => ({
    bar: 123
});
```

# Documentation

```ts
/**
 * @param text  Comment for parameter ´text´.
 */
function doSomething(target:any, text:string):number;

/**
 * @param value  Comment for parameter ´value´.
 * @returns      Comment for special return value.
 */
function doSomething(target:any, value:number):number;

/**
 * Comment for method ´doSomething´.
 * @param target  Comment for parameter ´target´.
 * @returns       Comment for return value.
 */
function doSomething(target:any, arg:any):number {
    return 0;
}
```

# Discriminated Union

We have algebraic data types in TypeScript but in this ecosystem [and fsharp] they are called discriminated unions

```ts
interface Square {
    kind: "square";
    size: number;
}

interface Rectangle {
    kind: "rectangle";
    width: number;
    height: number;
}

type Shape = Square | Rectangle;
```

# Enums

Unlike many other programming languages JavaScript does not have `enums` but TypeScript does

```ts
enum CardSuit {
    Clubs,
    Diamonds,
    Hearts,
    Spades
}
```
# Literals

## String

```ts
let foo: 'Hello';
```

# Recommended Reading
* https://basarat.gitbooks.io/typescript/content/docs/getting-started.html
