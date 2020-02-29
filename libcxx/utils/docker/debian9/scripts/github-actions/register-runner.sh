#!/usr/bin/env bash

set -x

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
Usage: install-actions-runner.sh [options] [--name runner-name]...

Install
Available options:
  -h|--help           show this help message
  --install           the install prefix to use.
  --token             token to register the github runner
  --url               url of the repository
  --name              a name to use for the buildbot
EOF
}

TOKEN=""
INSTALL_PATH=""
RUNNERS=()
URL=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --token)
      shift
      TOKEN="$1"
      shift
      ;;
    --install)
      shift
      INSTALL_PATH="$1"
      shift
      ;;
    --url)
      shift
      URL="$1"
      shift
      ;;
    --name)
      shift
      RUNNERS=($RUNNERS $1)
      shift
      ;;
    -h|--help)
      show_usage
      exit 0
      ;;
    -*)
      echo "unknown option: $1"
      show_usage
      exit 1
      ;;
    *)
      break
  esac
done

set -x

echo setting up runnings $RUNNERS
if [ "$RUNNERS" == "" ]; then
  echo "--name must be specified at least once"
  show_usage
  exit 1
fi
if [ "$INSTALL_PATH" == "" ]; then
  echo "--install must be specified"
  show_usage
  exit 1
fi

if [ "$URL" == "" ]; then
  echo "--url must be specified"
  show_usage
  exit 1
fi
if [ "$TOKEN" == "" ]; then
  echo "--token must be specified"
  show_usage
  exit 1
fi


cd $INSTALL_PATH

function register_runner {
  local NAME=$1
  $INSTALL_PATH/config.sh --unattended --url https://github.com/efcs/llvm-project  --token $TOKEN --name $NAME
  return $?
}

for R in $RUNNERS
do
  if register_runner $R; then
    break
  fi
  echo "failed to register any builder"
  shutdown nowe
done

echo "Done"
