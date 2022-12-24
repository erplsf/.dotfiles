#!/bin/sh

# NOTE: Doesn't work! debug on some test videos!

url="$1"

filename=$(yt-dlp "${url}" -j | jq '._filename')
while [ $? -ne 0 ]; do
	sleep 5
	filename=$(yt-dlp "${url}" -j | jq '._filename')
done

echo "${filename}"

if [ -n "${filename}" ]; then
	yt-dlp "${url}" -o - 2>"${filename}" | tee "${filename}" >/dev/null
fi
