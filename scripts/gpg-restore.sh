#!/usr/bin/env bash
set -eou pipefail

DIR="$1"
KEY_ID="$2"

pushd "$DIR"

gpg --import "$KEY_ID".pub.asc
gpg --import "$KEY_ID".priv.asc
gpg --import "$KEY_ID".sub_priv.asc
gpg --import-ownertrust ownertrust.txt

popd
