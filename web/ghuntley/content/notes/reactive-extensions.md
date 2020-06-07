---
title: Reactive Extensions
layout: notes
---

backpressure, or flow control, is when the consumer of the data is slower than the producer of the data. This can lead to excess memory usage because if the chain doesn't want to lose data over an asynchronous boundary, it has to queue. That queue may grow out of hand. Backpressure is a form of negotiation about how much items to produce until further notice which allows the bounding of the queue between the producer and the consumer.
Remember the old days when you typed too fast and the computer started beeping? That's backpressure.
