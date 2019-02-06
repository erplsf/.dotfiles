# Created by newuser for 5.6.2

source ~/.zplug/init.zsh

bindkey -e

zplug "dracula/zsh", as:theme

# Let zplug manage itself. To update run `zplug update`
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

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

# history stuff
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000000

setopt appendhistory
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
# /end of history stuff

# no beep!
unsetopt beep

# load key definitions
source ~/.bindkeys.zsh

# alias hub as git for better future!
# https://github.com/github/hub
eval "$(hub alias -s)"

# nodenv
# https://github.com/nodenv/nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
eval "$(nodenv init -)"
