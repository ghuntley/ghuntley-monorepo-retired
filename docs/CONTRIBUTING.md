Contribution Guidelines
=======================

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Contribution Guidelines](#contribution-guidelines)
    - [Before making a change](#before-making-a-change)
    - [Commit messages](#commit-messages)
    - [Commit content](#commit-content)
    - [Code quality](#code-quality)
    - [Builds & tests](#builds--tests)

<!-- markdown-toc end -->

This is a loose set of "guidelines" for contributing to my depot. Please note
that I will not accept any patches that don't follow these guidelines.

Also consider the [code of conduct](/tree/docs/CODE_OF_CONDUCT.md). No really,
you should.

## Before making a change

Before making a change, consider your motivation for making the change.
Documentation updates, bug fixes and the like are *always* welcome.

When adding a feature you should consider whether it is only useful for your
particular use-case or whether it is generally applicable for other users of the
project.

When in doubt - just ask! You can reach out to me via
[mail](mailto:mail@tazj.in) or on Twitter / IRC / etc.

## Commit messages

All commit messages should be structured like this:

```
type(scope): Subject line with at most a 68 character length

Body of the commit message with an empty line between subject and
body. This text should explain what the change does and why it has
been made, *especially* if it introduces a new feature.

Relevant issues should be mentioned if they exist.
```

Where `type` can be one of:

* `feat`: A new feature has been introduced
* `fix`: An issue of some kind has been fixed
* `docs`: Documentation or comments have been updated
* `style`: Formatting changes only
* `refactor`: Hopefully self-explanatory!
* `test`: Added missing tests / fixed tests
* `chore`: Maintenance work

And `scope` should refer to some kind of logical grouping inside of the project.

Please take a look at the existing commit log for examples.

## Commit content

Multiple changes should be divided into multiple git commits whenever possible.
Common sense applies.

The fix for a single-line whitespace issue is fine to include in a different
commit. Introducing a new feature and refactoring (unrelated) code in the same
commit is not fine.

`git commit -a` is generally **taboo**.

In my experience making "sane" commits becomes *significantly* easier as
developer tooling is improved. The interface to `git` that I recommend is
[magit][]. Even if you are not yet an Emacs user, it makes sense to install
Emacs just to be able to use magit - it is really that good.

For staging sane chunks on the command line with only git, consider `git add
-p`.

## Code quality

This one should go without saying - but please ensure that your code quality
does not fall below the rest of the project. This is of course very subjective,
but as an example if you place code that throws away errors into a block in
which errors are handled properly your change will be rejected.

In my experience there is a strong correlation between the visual appearance of
a code block and its quality. This is a simple way to sanity-check your work
while squinting and keeping some distance from your screen ;-)

## Builds & tests

My projects are built using [Nix][] to avoid "build pollution" via the user's
environment.

If you have Nix installed and are contributing to a project tracked in this
repository, you can usually build the project by calling `nix-build -A
path.to.project`.

For example, to build a project located at `tools/foo` you would call `nix-build
-A tools.foo`

If the project has tests, check that they still work before submitting your
change.

## Submitting patches

When making a change, please create an appropriate commit locally and send it to
me using either `git send-email` or `git format-patch`. The email address to use
for depot reviews is `depot@tazj.in`, which is a [public group][].

I recognise that most people are used to a GitHub-style workflow. If you run
into issues with the above but would still like to contribute, feel free to
reach out to me.

[magit]: https://magit.vc/
[Nix]: https://nixos.org/nix/
[public group]: https://groups.google.com/a/tazj.in/forum/?hl=en#!forum/depot
