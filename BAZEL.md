# [Bazel](https://www.bazel.build/) Guide

## Setup

If you don't have bazel installed, Bazelisk is recommended. See https://docs.bazel.build/versions/master/install-bazelisk.html for installation instructions.

Under Bazel, we need to indicate where Erlang can be found. Create a `.bazelrc` file containing:

```
build --@bazel-erlang//:erlang_home=/path/to/kerl/23.1
build --@bazel-erlang//:erlang_version=23.1
build --@rabbitmq-server//:elixir_home=/path/to/.kiex/elixirs/elixir-1.10.4/lib/elixir
```

Adjusting the paths as needed. On macOS, you will also want to add `build --spawn_strategy=local` due to https://github.com/bazelbuild/bazel/issues/7448.

You can then run the test suite with `bazel test //...`

## Start the broker with the Management and Metronome plugins

`bazel run broker`

## Run the test suite

`bazel test //...`
