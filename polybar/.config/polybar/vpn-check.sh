#!/bin/sh

connection_status() {
    status=$(sudo wg show 2>/dev/null | head -n 1 | awk '{print $NF}')
    if [ -z "$status" ]; then
        echo -n "%{B#f00}  vpn is down %{B-}"
    else
        echo -n "%{B#0b7327}   $status %{B-}"
    fi
}

connection_status
