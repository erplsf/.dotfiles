#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

PACKAGES=$(cat $DIR/packages.txt)

if ! sudo -v; then
  echo 'Your user needs to have sudo rights!'
  exit 1
else
  echo 'User has sudo rights!'
fi

if pacman -Qi $PACKAGES > /dev/null; then
  echo "$PACKAGES are installed"
fi

for file in "${DIR}/configs/"* "${DIR}/configs/".[!.]* ; do
    echo "$file -> ${HOME}/$(basename $file)"
    ln -st "${HOME}/$(basename $file)" "$file"
done
