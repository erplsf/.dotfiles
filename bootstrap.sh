#!/bin/sh

alias spacy="sudo pacman -Sy"

if ! sudo -v; then
  echo 'Your user needs to have sudo rights!'
  exit 1
else
  echo 'User has sudo rights!'
fi

if pacman -Qi git > /dev/null; then
  echo "git is installed already"
  pacman -R git
else
	spacy git
fi
