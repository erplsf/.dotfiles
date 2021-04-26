# Created by newuser for 5.6.2

source ~/.zplug/init.zsh

bindkey -e

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[ShiftTab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"      beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"       end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"    overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}" backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"    delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"        up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"      down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"      backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"     forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"    beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"  end-of-buffer-or-history
[[ -n "${key[ShiftTab]}"  ]] && bindkey -- "${key[ShiftTab]}"  reverse-menu-complete

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start {
		echoti smkx
	}
	function zle_application_mode_stop {
		echoti rmkx
	}
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

### ctrl+arrows
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word

### ctrl+delete
bindkey "\e[3;5~" kill-word

### ctrl+backspace
bindkey '^H' backward-kill-word

### ctrl+shift+delete
bindkey "\e[3;6~" kill-line

zplug "dracula/zsh", as:theme

# Let zplug manage itself. To update run `zplug update`
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "plugins/kubectl", from:oh-my-zsh

# load .env files from directories
zplug "plugins/dotenv", from:oh-my-zsh

zplug load

fpath+=~/.zfunc # add completitions and stuff

autoload -Uz compinit promptinit
compinit
promptinit

if command -v nvim >/dev/null 2>&1; then
	alias vim=nvim
	export EDITOR=nvim
fi

if command -v exa >/dev/null 2>&1; then
	alias ls=exa
fi

alias md=mkdir
alias be="bundle exec"

# https://github.com/clvv/fasd
# fast file-dir navigation-access

if command -v fasd >/dev/null 2>&1; then
	eval "$(fasd --init auto)"
fi

if [[ -f $HOME/.nodenv/bin/nodenv ]]; then
  export PATH="$HOME/.nodenv/bin:$PATH"
  eval "$(nodenv init -)"
fi

if [[ -d $HOME/.krew/bin/ ]]; then
  export PATH="${PATH}:${HOME}/.krew/bin"
fi

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

# alias hub as git for better future!
# https://github.com/github/hub

if command -v hub >/dev/null 2>&1; then
	eval "$(hub alias -s)"
fi

# nodenv
# https://github.com/nodenv/nodenv

if command -v nodenv >/dev/null 2>&1; then
	export PATH="$HOME/.nodenv/bin:$PATH"
	eval "$(nodenv init -)"
fi

# rbenv

if command -v rbenv >/dev/null 2>&1; then
	export PATH="$HOME/.rbenv/bin:$PATH"
	eval "$(rbenv init -)"
fi

if [[ -f $HOME/.nodenv/bin/nodenv ]]; then
  export PATH="$HOME/.nodenv/bin:$PATH"
  eval "$(nodenv init -)"
fi

export PATH=$PATH:~/bin

# WSL-specific configs
if uname -r | grep -qi 'Microsoft' ; then
	# For X-Server
  export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
	export LIBGL_ALWAYS_INDIRECT=1

  export DOCKER_HOST=tcp://localhost:2375

  function w-emacs() {
    nohup emacs $@ >& /dev/null &
  }
  export BROWSER="wslview"
  
  export PATH="$PATH:/mnt/c/Windows/System32"

  source ~/lib/azure-cli/az.completion
fi

# SSH completion
# taken from here https://serverfault.com/questions/170346/how-to-edit-command-completion-for-ssh-on-zsh
h=()
if [[ -r ~/.ssh/config ]]; then
  h=($h ${${${(@M)${(f)"$(cat ~/.ssh/config)"}:#Host *}#Host }:#*[*?]*})
fi
if [[ -r ~/.ssh/os-config ]]; then
  h=($h ${${${(@M)${(f)"$(cat ~/.ssh/os-config)"}:#Host *}#Host }:#*[*?]*})
fi
if [[ $#h -gt 0 ]]; then
  zstyle ':completion:*:ssh:*' hosts $h
  zstyle ':completion:*:slogin:*' hosts $h
fi

if command -v ros >/dev/null 2>&1; then
  export PATH="$HOME/.roswell/bin:$PATH"
fi

export PATH="$HOME/.local/bin:$PATH"

# hledger section

if command -v hledger >/dev/null 2>&1; then
  hledger-current-month () {
    hledger "$@" -B --depth=3 -b="`date '+%Y.%m'`" -e="`date -d '+1 month' '+%Y.%m'`"
  }
fi

if command -v nnn >/dev/null 2>&1; then
  export NNN_TRASH=1
fi

export PATH="$PATH:$HOME/go/bin"

eval "$(starship init zsh)"

if command -v fzf >/dev/null 2>&1; then
  hist() {
    print -z $(cat $HISTFILE | fzf-tmux --no-sort --no-mouse --no-multi)
  }
fi
