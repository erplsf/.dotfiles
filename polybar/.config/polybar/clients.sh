#!/usr/bin/env bash

herbstclient list_clients --title | while read -r line; do
    name="${line%% *}"
    echo "$name"
done
