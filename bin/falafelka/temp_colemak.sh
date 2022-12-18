#!/bin/zsh

pkill qxkb

cd ~/BigBagKbdTrixXKB
./install-dreymar-xmod.sh
./setxkb.sh 5ca us us

read -n 1
./install-dreymar-xmod.sh -r 1

qxkb &
pkill qxkb
