if [ -n "$DESKTOP_SESSION" ]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

# alias k9s
if command -v k9s 1>/dev/null 2>&1; then
      alias k9s="k9s --context \"\${K9S_CONTEXT}\""
fi

# alias kubectl
if command -v kubectl 1>/dev/null 2>&1; then
      alias kubectl="kubectl --context \"\${KUBECTL_CONTEXT}\""
fi
