---
title: docker
layout: notes
---

# destroy all containers
```
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)
```
