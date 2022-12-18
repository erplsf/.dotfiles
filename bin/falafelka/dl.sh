#!/bin/sh

# NOTE: Doesn't work! debug on some test videos!

url="$1"

filename=$(youtube-dl "${url}" -j | jq '._filename')
while [ $? -ne 0 ]; do
	sleep 5
	filename=$(youtube-dl "${url}" -j | jq '._filename')
done

echo "${filename}"

if [ -n "${filename}" ]; then
	youtube-dl "${url}" -o - 2>"${filename}" | tee "${filename}" >/dev/null
fi
