---
title: React
layout: notes
tags:
  - react
  - javascript
  - web
  - redux
---

{{< tweet 1008614266482692097 >}} 

# Tooling
* https://github.com/infinitered/reactotron
* https://github.com/maicki/why-did-you-update
* https://github.com/storybooks/storybook


# Testing

## Snapshots
* https://facebook.github.io/jest/docs/en/snapshot-testing.html

# Conditional Rendering
* https://github.com/ajwhite/render-if
* https://reactjs.org/docs/conditional-rendering.html

# State
Don't reach for redux by default. Have a read of [Lifting State Up](https://reactjs.org/docs/lifting-state-up.html) on the React website.

# Performance

Don't allocate what you don't need to in the component render path. Move everything you can out into consts and these functions can take the props interface.

# JSX

## Pure Functional 

If you are staring at your screen you need a lambda. 

```jsx
export const Header = (props) => (
        <Text testID="ui-header-text" style={ styles.content }>
            { props.text }
        </Text>
);
```

## Class

```jsx
class Header extends Component {
  render() {
    return (
        <Text testID="ui-header-text" style={ styles.content }>
            { this.props.text }
        </Text>
    );
  }
}
```

# Component Lifeycles

## ComponentWillMount
## ComponentWillUnmount
## ComponentWillReceiveProps
