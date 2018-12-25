# Created by newuser for 5.6.2

#if ! pgrep -u "$USER" ssh-agent > /dev/null; then
#    ssh-agent > ~/.ssh-agent-env
#fi
#if [[ "$SSH_AGENT_PID" == "" ]]; then
#    eval "$(<~/.ssh-agent-env)"
#fi

EDITOR=nvim
alias vim=nvim
alias ls=exa
alias md=mkdir
