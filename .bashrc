#    _               _
#   | |__   __ _ ___| |__  _ __ ___
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__
# (_)_.__/ \__,_|___/_| |_|_|  \___|

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias lf="nnn -e"
alias vim="nvim"
alias sudo="sudo"
alias cat="bat -P"
alias ls="exa --group-directories-first --icons -x"
alias top="btm"
alias gg="gitui"
alias si="sudo pacman -S"
alias sr="sudo pacman -R"
alias cr="cargo run --"
alias cb="cargo build"
alias cdo="cargo doc --open"
alias vimconf="nvim ~/.config/nvim/init.vim"
alias bashconf="nvim ~/.bashrc"
alias config="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias server="ssh root@51.195.40.125"

# exports
export ALTERNATE_EDITOR=""
export CLANG_VERSION=12.0.1
export EDITOR="nvim"
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:/home/odd/.cargo/bin/:/home/odd/.scripts:/opt/devkitpro/devkitPPC/bin"
export RUSTC_WRAPPER=""
export TERM="xterm-256color"
export VISUAL="nvim"

# devkitpro
DEVKITPRO="/opt/devkitpro"
DEVKITARM="/opt/devkitpro/devkitARM"
DEVKITPPC="/opt/devkitpro/devkitPPC"

# fzf
source "/usr/share/fzf/key-bindings.bash"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--height 20% --border --layout=reverse"

# zoxide
bind '"\C-o":"\C-uji\C-m"'
export _ZO_FZF_OPTS="--height 20% --border --layout=reverse"
eval "$(zoxide init --cmd j bash)"
