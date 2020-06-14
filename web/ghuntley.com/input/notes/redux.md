---
title: Redux
layout: notes
---

> You totes probs don't need redux. It's pretty garbage. Every stack trace is meaningless. Async actions have no way to watch for completion. Everything is ungreppable, debugging is impossible. Everything is a GOTO without a target. We need to go back in time and stop Dan - "YOU'LL REGRET IT"

{{< tweet 963236435837960192 >}}

https://www.smashingmagazine.com/2018/07/redux-designers-guide/

![](https://cdn-images-1.medium.com/max/2000/1*4fogNBtZrtR_3Xx6sIP9RQ.png)


# Actions
Actions are payloads of information that send data from the application to the store. They are the only source of information for the store. 

# Methods
# Reducers
# State

# Middleware
1. https://github.com/gaearon/redux-thunk
1. https://github.com/redux-saga/redux-saga
1. https://github.com/redux-observable/redux-observable

# Tooling
[Reactotron](https://github.com/infinitered/reactotron) is a desktop application that allows introspection of state in both React and React Native applications.

# Recommended Reading
* https://medium.com/@stowball/a-dummys-guide-to-redux-and-thunk-in-react-d8904a7005d3
