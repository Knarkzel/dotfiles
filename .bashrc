#    _               _
#   | |__   __ _ ___| |__  _ __ ___
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__
# (_)_.__/ \__,_|___/_| |_|_|  \___|

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias vim="nvim"
alias sudo="sudo"
alias ls="exa --group-directories-first --icons -x"
alias gg="gitui"
alias si="sudo pacman -S"
alias sr="sudo pacman -R"
alias cr="cargo run --"
alias cb="cargo build"
alias cdo="cargo doc --open"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias bashconf="nvim ~/.bashrc"
alias lfconf="nvim ~/.config/lf/lfrc"
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# exports
export PATH="$PATH:/home/odd/.cargo/bin/:/home/odd/.scripts:/opt/devkitpro/devkitPPC/bin"
export FZF_DEFAULT_COMMAND="rg --files"
export ALTERNATE_EDITOR=""
export EDITOR="nvim"
export VISUAL="less"
export RUSTC_WRAPPER=""
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
export LEMMY_DATABASE_URL=postgres://lemmy:password@localhost:5432/lemmy
export DATABASE_URL=postgres://lemmy:password@localhost:5432/lemmy
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

DEVKITPRO=/opt/devkitpro
DEVKITARM=/opt/devkitpro/devkitARM
DEVKITPPC=/opt/devkitpro/devkitPPC

# lf
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}
bind '"\C-l":"lfcd\C-m"'

# zoxide
bind '"\C-o":"ji\C-m"'
eval "$(zoxide init --cmd j bash)"
