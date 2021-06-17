#!/usr/bin/env zsh
set -euo pipefail

# indexing in zsh arrays starts at 1
# gitstatusd params taken from a running p90k prompt

declare -A repos
repos=("$HOME/org" "" "$HOME/ledger" "" "$HOME/.dotfiles" "略" "$HOME/.password-store" "")

is_repo_dirty() {
	local req_id=id
	local dir="$1"
	echo -nE $req_id$'\x1f'$dir$'\x1e' | ~/gitstatus/usrbin/gitstatusd -G v1.3.1 -s -1 -u -1 -d -1 -c -1 -m -1 -v FATAL -t 32 | {
	    local resp
	    IFS=$'\x1f' read -rd $'\x1e' -A resp
	    # echo "${resp[@]}" - array with all the fields
	    # 11 - number of staged changes
	    # 12 - number of unstaged changes
	    # 14 - number of untracked files
	    # see more info at `gitstatusd --help`
	    echo "$(((resp[11] + resp[12] + resp[14]) > 0))"
	}
}

for path icon in ${(kv)repos}; do
	final_string=""
	is_dirty=$(is_repo_dirty $path)
	if [[ $is_dirty -eq 1 ]]; then
		final_string="${final_string}%{B#dce800}%{F#000000} $icon %{F-}%{B-}"
	fi
	echo -n $final_string
done
