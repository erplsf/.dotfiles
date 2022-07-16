#!/usr/bin/env bash

# HACK: Warning, this is broken!

vpns="$(nmcli connection | grep vpn | tr -s ' ' | cut -d' ' --complement -f2,3)"
checkmark="<span foreground=\"green\"></span>"
rule="<span foreground=\"yellow\">-</span>"

if [ -n "$1" ]; then # we got input from rofi
    echo $1 | grep -q 
    up=$?
    vpn=$(echo "$1" | cut -d' ' -f3)

    if [ $up -eq 0 ]; then # enabled
        coproc ( nmcli connection down $vpn > /dev/null 2>1 )
    else
        op whoami
        logged_in=$?
        if [ $logged_in -ne 0 ]; then
            pass=$(rofi -replace -dmenu -password -no-fixed-num-lines -p "$(printf "$1" | sed s/://)")
            eval $(echo "$pass" | op signin)
            op item get 'pritunl' --otp | nmcli connection up $vpn
        fi
    fi

    exit 0
else
    # echo $checkmark
    while IFS= read -r line; do
        echo -en "\0markup-rows\x1ftrue\n"
        line=${line//wlp2s0/up}
        line=$(echo $line | sed "s^\(.*\)up^$checkmark \1^") # add checkmark to enabled vpns
        line=$(echo $line | sed "s^\(.*\)--^$rule \1^") # add space to disabled vpns
        # echo $line
        echo -en "$line\n"
    done <<< "$vpns"
fi
