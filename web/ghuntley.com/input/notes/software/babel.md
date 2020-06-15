---
layout: notes
title: Babel
---

# Aliases

There's a [plugin that allows you to (map, alias, resolve)](https://www.npmjs.com/package/babel-plugin-webpack-alias) directories as different directories during the Babel process. It's particularly useful when you have files you don't want to use with relative paths (especially in big projects).

```js
const path = require('path');

module.exports = {
    resolve: {
        alias: {
            '@shared-framework': path.join(__dirname, 'build/shared-framework'),
            '@components': path.join(__dirname, 'build/mobile-application/src/components'),
            '@store': path.join(__dirname, 'build/mobile-application/src/redux'),
            '@apps-config': path.join(__dirname, 'build/mobile-application/src/applications/apps-config'),
            '@assets': path.join(__dirname, 'build/mobile-application/src/assets'),
            '@utils': path.join(__dirname, 'build/mobile-application/src/utils'),
        }
    }
};
```

Usage of aliases is highly recommended as it abstracts your code from the filesystem and allows effortless filesystem location refactoring.
