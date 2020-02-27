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
Usage: start-actions-runner.sh [options]

Run autoconf with the specified arguments. Used inside docker container.

Available options:
  -h|--help           show this help message
  --install        the install path of the actions runner
  --url            the URL of the github repository to start the runner for
  --token          the token used to start the runner
Required options: --install and --branch

EOF
}

INSTALL_DIR=""
URL=""
TOKEN=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --install)
      shift
      INSTALL_DIR="$1"
      shift
      ;;
    --url)
      shift
      URL="$1"
      shift
      ;;
    --token)
      shift
      TOKEN="$1"
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

if [ "$INSTALL_DIR" == "" ]; then
  echo "No install directory. Please specify the --install argument."
  show_usage
  exit 1
fi
if [ "$URL" == "" ]; then
  echo "No repository url. Please specify the --url argument."
  show_usage
  exit 1
fi
if [ "$TOKEN" == "" ]; then
  echo "No runner token specified. Please specify the --token argument."
  show_usage
  exit 1
fi

#!/usr/bin/env bash
set -x


cd $INSTALL_DIR
ls

set +x
./config.sh --url $URL --token $TOKEN
set -x

./run.sh

echo "Done"
