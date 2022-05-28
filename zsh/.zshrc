# NOTE: Evaluate later
# # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# # Initialization code that may require console input (password prompts, [y/n]
# # confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

# Clone zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
if [ ! -d "${ZINIT_HOME}" ]; then
      mkdir -p "$(dirname ${ZINIT_HOME})"
      git clone --depth 1 https://github.com/zdharma-continuum/zinit.git "${ZINIT_HOME}"
fi

# Load it
source "${ZINIT_HOME}/zinit.zsh"

# Regular plugins loaded without investigating.
zinit depth'1' \
      light-mode wait lucid for \
      zdharma-continuum/history-search-multi-word \
      zdharma-continuum/zinit-annex-readurl \
      zdharma-continuum/zinit-annex-bin-gem-node \
      zdharma-continuum/zinit-annex-patch-dl \
      zdharma-continuum/zinit-annex-rust

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
      light-mode wait lucid for \
      rbenv/rbenv

zinit depth'1' atclone'mkdir -p "$RBENV_ROOT/plugins/"; ln -sf "$PWD" "$RBENV_ROOT/plugins/ruby-build"' \
      as'null' nocompile \
      light-mode wait lucid for \
      rbenv/ruby-build

# Nodenv
zinit depth'1' atclone'NODENV_ROOT="$PWD" bin/nodenv init - > znodenv.zsh' \
      atinit'export NODENV_ROOT="$PWD"' atpull"%atclone" \
      as'command' pick'bin/nodenv' src"znodenv.zsh" nocompile'!' \
      light-mode wait lucid for \
      @nodenv/nodenv

zinit depth'1' atclone'mkdir -p "$NODENV_ROOT/plugins/"; ln -sf "$PWD" "$NODENV_ROOT/plugins/node-build"' \
      as'null' nocompile \
      light-mode wait lucid for \
      @nodenv/node-build

# kubeval
zinit from'gh-r' as'program' \
      light-mode wait lucid for \
      instrumenta/kubeval

# hyperfine (cli benchmarking)
zinit from'gh-r' as'program' \
      mv'hyperfine* -> hyperfine' nocompile'!' \
      pick'hyperfine/hyperfine' \
      light-mode wait lucid for \
      @sharkdp/hyperfine

# tfenv stuff
zinit depth'1' atinit'export PATH="$PATH:$PWD"' \
      as'command' pick'bin/tfenv' nocompile'!' \
      light-mode wait lucid for \
      tfutils/tfenv

# tgswitch
zinit from'gh-r' as'program' \
      light-mode wait lucid for \
      @warrensbox/tgswitch

# load-tgswitch() {
#   local tgswitchrc_path=".tgswitchrc"

#   if [ -f "$tgswitchrc_path" ]; then
#     tgswitch
#   fi
# }
# add-zsh-hook chpwd load-tgswitch
# load-tgswitch

# Exa
zinit from'gh-r' \
      as'command' mv'bin/exa* -> bin/exa' \
      pick'bin/exa' \
      light-mode wait lucid for \
      ogham/exa

# TODO: alias only if command exists
alias l='exa -l'
alias ls='exa'

# jq
zinit from'gh-r' as'program' mv'jq* -> jq' \
      light-mode wait lucid for \
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

# fzf TODO: Fix ungly backtracking
zinit depth'1' atclone'./install --no-bash --no-fish --completion --no-key-bindings --no-update-rc' \
      atpull'./install --bin' as'program' pick'bin/fzf' src'../../../../../.fzf.zsh' \
      light-mode wait lucid for \
      junegunn/fzf

# fzy (to compare with fzf)
# zinit depth'1' make  as'program' pick'bin/fzf' src'../../../../../.fzf.zsh' \
#       light-mode for \
#       jhawthorn/fzy

# install rustup annex
# zinit light-mode for \
#       zinit-zsh/z-a-rust

# Just install rust and make it available globally in the system
# zinit id-as"rust" rustup as"command" \
#       pick"bin/rustc" \
#       atload="[[ ! -f ${ZINIT[COMPLETIONS_DIR]}/_cargo ]] && zi creinstall -q rust; export CARGO_HOME=\$PWD; export RUSTUP_HOME=\$PWD/rustup" \
#       light-mode for \
#       zdharma-continuum/null

# sudo plugin -> allows to use ESC-ESC to prepend `sudo` or `sudoedit` to previous command
zinit \
      light-mode wait lucid for \
      OMZP::sudo

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

# alias for mplayer

if [ -f "$HOME/mpv/mpv.app/Contents/MacOS/mpv" ]; then
      alias mpv="$HOME/mpv/mpv.app/Contents/MacOS/mpv --hwdec=API --input-ipc-server=/tmp/mpv.socket"
else
      alias mpv="mpv --hwdec=API --input-ipc-server=/tmp/mpv.socket"
fi

# alias for mkdir

alias md="mkdir -p"

# hledger stuff

export PATH="$PATH:$HOME/ledger/bin"

alias hl="hledger"
alias hb="hl balance -B --pretty-tables --auto --monthly -b 'last quarter' -T budget"
alias he="hl is -B --pretty-tables --monthly -b 'last quarter'"
alias hr="hl roi -Y --inv investments --pnl 'unrealized' --value='then'"
alias heh="he -b2018 -ethisyear -YA --depth 1" # expenses horizon

# doom emacs
export PATH="$PATH:$HOME/.emacs.d/bin"

# alias herbsluftwm
alias hc="herbstclient"


# TODO: alias only if command exists
alias vim="nvim"

export PATH="$HOME/.poetry/bin:$PATH"

# alias editor
alias edit="$EDITOR"

if [ -f "/opt/local/bin/port" ]; then
    export PATH="$PATH:/opt/local/bin"
fi

if command -v aws-vault 1>/dev/null 2>&1; then
      function awe() {
            declare -A envs
            envs=(
                  [d]=develop
                  [n]=neutral
                  [s]=staging
                  [live]=live
            )
            caseBranch=${(j:|:)${(k)envs}}
            envList=${(j:,:)${(k)envs}}

            case "$1" in
                  --)
                        aws-vault exec default -- zsh -c "${*[2,-1]}"
                        exitCode=$?
                        ;;
                  $~caseBranch)
                        mapped_env="${envs[$1]}"
                        aws-vault exec "klar-${mapped_env}" -- zsh -c "export KUBE_CONFIG_PATH=$HOME/.kube/config && export KUBECTL_CONTEXT=${mapped_env} && . ${HOME}/.vault-shadows && ${*[3,-1]}"
                        exitCode=$?
                        ;;
                  *)
                        echo "First argument needs to be any environment from this [${envList}] list or '--' for default profile." >&2
                        exitCode=1
                        ;;
            esac
            return $exitCode
      }
fi

zinit from'gh-r' as'program' \
      nocompile'!' \
      pick'kustomize' \
      light-mode wait lucid for \
      @kubernetes-sigs/kustomize

if !command -v zig 1>/dev/null 2>&1; then
      zinit depth'1' atclone'mkdir build; cd build && cmake -DZIG_PREFER_CLANG_CPP_DYLIB=true .. && make install' \
            atpull"%atclone" \
            as'command' pick'build/bin/zig' nocompile'!' \
            light-mode wait lucid for \
            @ziglang/zig
fi

zinit from'gh-r' as'program' mv"gomplate* -> gomplate" \
      nocompile'!' \
      pick'gomplate' \
      light-mode wait lucid for \
      @hairyhenderson/gomplate

zinit from'gh-r' as'program' \
      nocompile'!' \
      light-mode wait lucid for \
      @homeport/dyff

zinit depth'1' \
      light-mode wait lucid for \
      @asdf-vm/asdf

if [ ! -d "${HOME}/.asdf/installs/istioctl/" ]; then
   asdf plugin-add istioctl
fi

alias qemu-nixos="qemu-system-x86_64 \
      -accel hvf \
      -boot d \
      -m 4G \
      -cpu host \
      -smp 4 \
      -hda ~/nixos/nixos.img \
"

# Stolen from here: https://github.com/zdharma-continuum/fast-syntax-highlighting#zinit
zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

function tcl() {
      fd -t d --hidden '.terragrunt|.terraform' | xargs rm -r
      fd -t f --hidden '.terraform.lock.hcl' | xargs rm
}

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

zinit from'gh-r' as'program' \
      light-mode wait lucid for \
      @craftypath/nextver

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer
