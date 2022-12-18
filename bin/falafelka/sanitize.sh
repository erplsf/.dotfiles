#!/usr/bin/env bash

while read arg; do
    if [ "${arg}" != "${arg//:/_}" ]; then
        $1 "${arg}" "${arg//:/_}"
    fi
done
