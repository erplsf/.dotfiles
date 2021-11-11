#!/bin/sh

connection_status() {
    status=$(mullvad status | grep Connected)
    if [ -z "$status" ]; then
        echo -n "%{B#f00}  %{B-}"
    else
        echo -n "%{B#0b7327}  %{B-}"
    fi
}

connection_status
