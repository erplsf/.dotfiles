#!/bin/bash

PACKAGES=$(cat packages.txt)

if ! sudo -v; then
  echo 'Your user needs to have sudo rights!'
  exit 1
else
  echo 'User has sudo rights!'
fi

if pacman -Qi $PACKAGES > /dev/null; then
  echo "$PACKAGES are installed"
fi
