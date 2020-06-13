---
title: kubernetes
layout: notes
---

# visualisation

https://code.benco.io/kubeview/

# contributing 
https://gist.github.com/tpepper/aad07af79ed6d098e3a41e1db452ce79

# concepts

* `kube-apiserver` is the http api service
* `kube-controller-manager` management of resources like services and storage
* `kube-scheduler` schedules resources like services and storage
* `kube-proxy` load balancing proxy
* `kublet` container management and reporting on minions

# terminology

* `namespace` a seperate group of pods, replication fcontrollers and services
* `minon` a worker node
* `pod` a group of containers running on the same node
* `replication controller` a controller for a group of pods
* `service` internal load balancer for a group of pods

# reference implementations

* https://developer.atlassian.com/blog/2017/07/kubernetes-infra-on-aws/

# DNS

Applications (pods) are deployed into a namespace. These pods (docker images) are automatically provided with an private ip address via an internal DHCP server that uses the `kube-dns` resolver. The DNS server is configured to append the namespace to the search domain.

If an application was deployed into the `staging` namespace and needed to talk to the `rabbitmq` service in the `staging` namespace then configure the application to use the unqualified name as it will automatically expand.

```
nslookup rabbitmq
Server:     192.168.1.11
Address:    192.168.1.11#53

Name:   rabbitmq.staging.svc.cluster.local
Address: 192.168.1.5
```

If the application needs to talk to the `rabbitmq` service in the `production` namespace then use the fully qualified hostname ie. `rabbitmq.production.svc.cluster.local`

See https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/ for more information. 

# external DNS

There's an addon called [ExternalDNS](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/
) that makes Kubernetes resources discoverable via public DNS servers and allows you to control DNS records dynamically via Kubernetes resources in a DNS provider-agnostic way.

# event log

The event log for pods/containers can be accessed via

```shell
$ kutectl get events --watch
```

An interactive session can be launched to watch/follow the state of pods in the cluster via

```shell
$ kubectl get pods --watch
```

# troubleshooting guides

The rule of thumb is when you do `kubectl logs` or `kubectl exec` the API server makes a request _to the_ kubelet. If you experience problems then [refer to this cheat cheat](https://s.itho.me/day/2017/k8s/1020-1100%20All%20The%20Troubles%20You%20Get%20Into%20When%20Setting%20Up%20a%20Production-ready%20Kubernetes%20Cluster.pdf).

# prefabricated applications

The equivilant of BitNami in the Kubernetes world is https://github.com/kubernetes/charts which are deployed using https://helm.sh/


# recommended consumption
* https://youtu.be/PH-2FfFD2PU
* https://github.com/kelseyhightower/kubernetes-the-hard-way
* https://github.com/hobby-kube/guide
