#!/usr/bin/env bash

set -x

mkdir build/
cd build/

cmake "-DLLVM_ENABLE_PROJECTS=libcxx;libcxxabi" ../llvm/
