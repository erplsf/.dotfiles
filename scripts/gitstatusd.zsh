#!/usr/bin/env zsh
set -euo pipefail

# indexing in zsh arrays starts at 1
# gitstatusd params taken from a running p90k prompt

declare -A repos
repos=(
	"$HOME/org" ""
	"$HOME/ledger" ""
	"$HOME/.dotfiles" "略"
	"$HOME/.password-store" ""
)

is_repo_dirty() {
	local req_id=id
	local dir="$1"
	echo -nE $req_id$'\x1f'$dir$'\x1e' | gitstatusd -v FATAL -t $(nproc --all) | {
      local resp
      IFS=$'\x1f' read -rd $'\x1e' -A resp && print -lr -- "${(@qq)resp}"
    }
}

is_repo_dirty "$1"
