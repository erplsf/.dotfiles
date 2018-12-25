# Created by newuser for 5.6.2

source ~/.zplug/init.zsh

zplug "0i0/0i0.zsh-theme", as:theme

zplug load

autoload -Uz compinit promptinit
compinit
promptinit

export EDITOR=nvim
alias vim=nvim
alias ls=exa
alias md=mkdir

# https://github.com/clvv/fasd
# fast file-dir navigation-access
eval "$(fasd --init auto)"
