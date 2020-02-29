#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
SECRETS_DIR=$(realpath $SCRIPT_DIR/../../secrets)

set -x
set -e

echo $SCRIPT_DIR
ls $SECRETS_DIR

if [ ! -f "$SECRETS_DIR/github-token.txt" ]; then
  echo github-token.txt must be present
  exit 1
else
  sudo docker secret create github_token $SECRETS_DIR/github-token.txt
fi
