if [ -n "$DESKTOP_SESSION" ]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

function set_aliases() {
  alias k9s="k9s ${K9S_CMD}"
}

if [ ! -z "${AWS_VAULT+x}" ]; then
    set_aliases
fi
