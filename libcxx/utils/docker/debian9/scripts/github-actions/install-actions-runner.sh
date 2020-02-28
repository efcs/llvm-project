#!/usr/bin/env bash
#===- libcxx/utils/docker/scripts/install_clang_package.sh -----------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

set -e

function show_usage() {
  cat << EOF
Usage: install-actions-runner.sh [options]

Install
Available options:
  -h|--help           show this help message
  --install           the install prefix to use.
  --version           the package version to install
EOF
}

VERSION=""
INSTALL_PATH=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version)
      shift
      VERSION="$1"
      shift
      ;;
    --install)
      shift
      INSTALL_PATH="$1"
      shift
      ;;
    -h|--help)
      show_usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
  esac
done

set -x

if [ "$VERSION" == "" ]; then
  echo "--version must be specified"
  show_usage
  exit 1
fi

mkdir $INSTALL_PATH
cd $INSTALL_PATH

DEST_FILE=actions-runner-linux-x64-$VERSION.tar.gz

curl -O -L https://github.com/actions/runner/releases/download/v$VERSION/$DEST_FILE
tar xzf $DEST_FILE


echo "Done"
