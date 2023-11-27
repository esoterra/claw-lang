---
name: B) Incorrect compilation issue
about: Report an issue with generated code
title: 'Runtime bug: *describe problem here*'
labels: incorrect-compilation
assignees: ''

---

<!-- Only use this template when your code compiles without errors but behaves incorrectly when run. -->
<!-- Make sure that your issue is not already represented by an existing bug or feature request. -->

**Reproduction case**
The code you compiled or a minimal version of it that reproduces the incorrect behavior you've observed either inline as a snippet or as a link to a repository.

**To Reproduce**
Code for reproducing the incorrect behavior.
Ideally something similar to the [claw runtime tests](https://github.com/esoterra/claw-lang/blob/main/tests/runtime.rs) using Wasmtime to instantiate the component, perform calls, and assert the expected behavior.

**Expected behavior**
A clear and concise description of what you expected to happen.

**Additional context**
Add any other context about the problem here.
