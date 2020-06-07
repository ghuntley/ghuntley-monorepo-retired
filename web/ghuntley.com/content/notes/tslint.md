---
title: tslint
layout: notes
---

# React
```js
{
    "$schema": "http://json.schemastore.org/tslint",
    "extends": ["tslint:latest", "tslint-react"],
    "rules": {
        "indent": [true, "spaces"],
        "no-any": true,
        "no-empty-interface": false,
        "no-unused-variable": [true, {
            "ignore-pattern": "^React"
        }],
        "object-literal-sort-keys": false,
        "ordered-imports": [false],
        "quotemark": [true, "single", "jsx-double"],
        "react-this-binding-issue": [ true, { "allow-anonymous-listeners": true } ],
        "trailing-comma": [false, {
            "multiline": "always",
            "singleline": "never"
        }],
        "no-implicit-dependencies": false,
        "semicolon": [false, "always", "ignore-interfaces"],
        "align": [true, "statements", "members"],
        "no-submodule-imports": false,
        "no-object-literal-type-assertion": false,

        "jsx-alignment": false,
        "jsx-curly-spacing": false,
        "jsx-no-multiline-js": false,
        "jsx-boolean-value": false
    }
}
````
