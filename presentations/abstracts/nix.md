Title: What if the industry didn’t use Docker?
---

The recent announcements by the Docker corporation related to pricing surprised many people and has prompted organisations to consider moving to alternatives such as podman which unfortunately - at it’s core - is still docker. Docker solved many problems in the industry related to distribution of software via encapsulation but also created a raft of software supply chain problems.

In the session Geoff will run through folks through his personal monorepo in Gitpod where everything is built from source and every component (inc operating system) is 100% hackable but without the developer experience problems experienced with compilation from source. You’ll learn about source/binary substitution, how nix differs from docker, how to generate smaller docker images from nix expressions and how to deploy them onto your kubernetes cluster using kubenix and nixery without a single line of YAML.

# Bio

Geoffrey Huntley is a software engineer from Gitpod.  He is currently travelling around Australia and working remotely from his van. https://ghuntley.com/the-office
