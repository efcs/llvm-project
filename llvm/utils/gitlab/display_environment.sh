#!/usr/bin/env bash

set -x
set -e

env
pwd
ls
ls ..
ls ../..
clang++ --version
g++ --version
cmake --version
ninja --version
ninja --help || echo "Ninja Info Done"
