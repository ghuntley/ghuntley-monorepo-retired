---
title: Security Information and Event Management
layout: notes
tags:
  - security
  - logging
  - blue-team
---

# Signatures
Every SIEM vendor has rules to detect port scans, ping sweeps and threats like the `smurf attack` but the higher layers contain numerous applications and protocols with special characteristics that write their own custom log files. SIEM vendors consider the signatures and correlations as their intelectual property and do not tend to share details on the coverage. This lack of a standard prompted the creation of [Sigma](https://github.com/dschaudel/sigma) - An open standard in which detection mechanisms can be defined, shared and collected in order to improve the detection capabilities on the application layers for everyone.

