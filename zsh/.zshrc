### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

# Two regular plugins loaded without investigating.
zinit depth'1' \
      light-mode for \
      zsh-users/zsh-autosuggestions \
      zdharma/fast-syntax-highlighting \
      zdharma/history-search-multi-word

zinit depth'1' \
      light-mode for \
      romkatv/powerlevel10k

if [[ ! $TERM == (dumb|linux) ]]; then # fancy terminal, enable fancy theme
    P10K_CONFIG_FILE=~/.p10k.zsh
else # dumb terminal, load portable theme
    P10K_CONFIG_FILE=~/.p10k-portable.zsh
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f "${P10K_CONFIG_FILE}" ]] || source "${P10K_CONFIG_FILE}"

# envs for term
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

# direnv (support for .envrc)
zinit from"gh-r" as"program" mv"direnv* -> direnv" \
      atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' \
      pick"direnv" src="zhook.zsh" for \
      direnv/direnv

# History!
export HISTFILE="$HOME/.history"
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
# setopt HIST_BEEP                 # Beep when accessing nonexistent history.

## env and stuff


# Keychain -> do I need Keychain really?
if command -v keychain 1>/dev/null 2>&1; then
    eval `keychain -q --eval`
    keychain -q ~/.ssh/id_ed25519
    keychain -q ~/.ssh/amykhaylyk-ct
    keychain -q ~/.ssh/cdt-azure
fi

# Go stuff
if command -v go 1>/dev/null 2>&1; then
    eval `go env`
    export PATH="$PATH:$GOPATH/bin"
fi

# Custom bins
export PATH="$PATH:$HOME/bin"

# Rbenv / ruby stuff
zinit depth'1' atclone'RBENV_ROOT="$PWD" bin/rbenv init - > zrbenv.zsh' \
      atinit'export RBENV_ROOT="$PWD"' atpull"%atclone" \
      as'command' pick'bin/rbenv' src"zrbenv.zsh" nocompile'!' \
      light-mode lucid for \
      rbenv/rbenv

zinit depth'1' atclone'mkdir -p "$RBENV_ROOT/plugins/"; ln -sf "$PWD" "$RBENV_ROOT/plugins/ruby-build"' \
      as'null' nocompile \
      light-mode for \
      rbenv/ruby-build    

# Nodenv
zinit depth'1' atclone'NODENV_ROOT="$PWD" bin/nodenv init - > znodenv.zsh' \
      atinit'export NODENV_ROOT="$PWD"' atpull"%atclone" \
      as'command' pick'bin/nodenv' src"znodenv.zsh" nocompile'!' \
      light-mode lucid for \
      nodenv/nodenv

zinit depth'1' atclone'mkdir -p "$NODENV_ROOT/plugins/"; ln -sf "$PWD" "$NODENV_ROOT/plugins/node-build"' \
      as'null' nocompile \
      light-mode for \
      nodenv/node-build

# kubeval
zinit from'gh-r' as'program' \
      light-mode for \
      instrumenta/kubeval

# hyperfine (cli benchmarking)
zinit from'gh-r' as'program' \
      mv'hyperfine* -> hyperfine' nocompile'!' \
      pick'hyperfine/hyperfine' \
      light-mode for \
      @sharkdp/hyperfine

# tfenv stuff
zinit depth'1' atinit'export PATH="$PATH:$PWD"' \
      as'command' pick'bin/tfenv' nocompile'!' \
      light-mode for \
      tfutils/tfenv

# tgenv version manager # TODO: replace? it's old and unmaintained
zinit depth'1' atinit'export PATH="$PATH:$PWD"' \
      as'command' pick'bin/tgenv' nocompile'!' \
      light-mode for \
      cunymatthieu/tgenv

# Exa
zinit from'gh-r' \
      as'command' mv'exa* -> exa' \
      pick'bin/exa' \
      light-mode for \
      ogham/exa

# TODO: alias only if command exists
alias l='exa -l'
alias ls='exa'

# jq
zinit from'gh-r' as'program' mv'jq* -> jq' \
      light-mode for \
      stedolan/jq

# nnn stuff: TODO: migrate nnn to zinit

export NNN_TRASH=1 # trash (needs trash-cli) instead of delete

# EDITOR

export EDITOR="emacsclient"

# z.lua: jumping around TODO: Maybe add c-compiled module

zinit depth'1' atclone'lua z.lua --init zsh once enhanced > zzlua.zsh' \
      atpull"%atclone" \
      src'zzlua.zsh' nocompile'!' \
      light-mode wait lucid for \
      skywind3000/z.lua

# pyenv

zinit depth'1' atclone'PYENV_ROOT="$PWD" bin/pyenv init --path > zpyenv.zsh' \
      atinit'export PYENV_ROOT="$PWD"' atpull"%atclone" \
      as'command' pick'bin/pyenv' src"zpyenv.zsh" nocompile'!' \
      light-mode wait lucid for \
      pyenv/pyenv

export PATH="$PATH:$HOME/.local/bin"

# fzf
zinit depth'1' atclone'./install --no-bash --no-fish --completion --no-key-bindings --no-update-rc' \
      atpull'./install --bin' as'program' pick'bin/fzf' src'../../../.fzf.zsh' \
      light-mode for \
      junegunn/fzf

# fzy (to compare with fzf)
zinit depth'1' make  as'program' pick'bin/fzf' src'../../../.fzf.zsh' \
      light-mode for \
      jhawthorn/fzy

# install rustup annex
zinit light-mode for \
      zinit-zsh/z-a-rust

# Just install rust and make it available globally in the system
zinit id-as"rust" rustup as"command" \
      pick"bin/rustc" \
      atload="[[ ! -f ${ZINIT[COMPLETIONS_DIR]}/_cargo ]] && zi creinstall -q rust; export CARGO_HOME=\$PWD; export RUSTUP_HOME=\$PWD/rustup" \
      light-mode for \
      zdharma/null

# sudo plugin -> allows to use ESC-ESC to prepend `sudo` or `sudoedit` to previous command
zinit light-mode for OMZP::sudo

# keys, copied from: https://wiki.archlinux.org/index.php/Zsh#Key_bindings

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
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	  autoload -Uz add-zle-hook-widget
	  function zle_application_mode_start { echoti smkx }
	  function zle_application_mode_stop { echoti rmkx }
	  add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	  add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

zicompinit # init completions
zicdreplay # replay defined completion functions so they are added

# alias for mplayer

alias mpv="mpv --hwdec=API --input-ipc-server=/tmp/mpv.socket"

# alias for mkdir

alias md="mkdir -p"

# hledger stuff

export PATH="$PATH:$HOME/ledger/bin"

alias hl="hledger"
alias hb="hl balance -B --pretty-tables --auto --monthly -b 'last quarter' -T budget"
alias he="hl is -B --pretty-tables --monthly -b 'last quarter'"

# doom emacs
export PATH="$PATH:$HOME/.emacs.d/bin"

# alias herbsluftwm
alias hc="herbstclient"


# TODO: alias only if command exists
alias vim="nvim"

export PATH="$HOME/.poetry/bin:$PATH"

# alias editor
alias edit="$EDITOR"

# TODO: switch to zinit to manage istioctl

if [ -d "$HOME/istio-1.9.5" ]; then
    export PATH="$PATH:$HOME/istio-1.9.5/bin"
fi

if [ -f "/opt/local/bin/port" ]; then
    export PATH="$PATH:/opt/local/bin"
fi

# TODO: better aliases
if command -v aws-vault 1>/dev/null 2>&1; then
      function awe() {
            prev_KUBECTL_CONTEXT=$KUBECTL_CONTEXT
            prev_K9S_CONTEXT=$K9S_CONTEXT
            case "$1" in
                  "--")
                        aws-vault exec default -- zsh -c "${*[2,-1]}"
                        ;;
                  "d")
                        export KUBECTL_CONTEXT='develop'
                        export K9S_CONTEXT='develop'
                        aws-vault exec klar-develop -- zsh -c "${*[3,-1]}"
                        ;;
                  "n")
                        export KUBECTL_CONTEXT='neutral'
                        export K9S_CONTEXT='neutral'
                        aws-vault exec klar-neutral -- zsh -c "${*[3,-1]}"
                        ;;
                  "s")
                        export KUBECTL_CONTEXT='staging'
                        export K9S_CONTEXT='staging'
                        aws-vault exec klar-staging -- zsh -c "${*[3,-1]}"
                        ;;
                  "live")
                        export KUBECTL_CONTEXT='live'
                        export K9S_CONTEXT='live'
                        aws-vault exec klar-live -- zsh -c "${*[3,-1]}"
                        ;;
                  *)
                        echo "You have failed to specify what to do correctly."
                        false
                        ;;
            esac
            export KUBECTL_CONTEXT=$prev_KUBECTL_CONTEXT
            export K9S_CONTEXT=$prev_K9S_CONTEXT
      }
fi

zinit from'gh-r' as'program' \
      nocompile'!' \
      pick'kustomize' \
      light-mode for \
      @kubernetes-sigs/kustomize
