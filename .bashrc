#    _               _
#   | |__   __ _ ___| |__  _ __ ___
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__
# (_)_.__/ \__,_|___/_| |_|_|  \___|

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias lf="n -e"
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
alias config="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias server="ssh root@51.195.40.125"
alias rm="rm -r"
alias cp="cp -r"

# exports
export RUSTC_WRAPPER=""
export ALTERNATE_EDITOR=""
export CLANG_VERSION=12.0.1
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:/home/odd/.cargo/bin/:/home/odd/.scripts:/opt/devkitpro/devkitPPC/bin:/home/odd/.local/share/gem/ruby/3.0.0/bin"
export TERM="screen-256color"

# nvim
nvim_wrapper() {
  if test -z $NVIM_LISTEN_ADDRESS; then
      nvim $@
  else
      nvr $@
  fi
}

alias vim="nvim_wrapper"
alias bashconf="vim ~/.bashrc"

export EDITOR="nvim_wrapper"
export VISUAL="nvim_wrapper"

# devkitpro
DEVKITPRO="/opt/devkitpro"
DEVKITARM="/opt/devkitpro/devkitARM"
DEVKITPPC="/opt/devkitpro/devkitPPC"

# nnn
if [ -f /usr/share/nnn/quitcd/quitcd.bash_zsh ]; then
    source /usr/share/nnn/quitcd/quitcd.bash_zsh
fi

# fzf
source "/usr/share/fzf/key-bindings.bash"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--height 20% --border --layout=reverse"

# zoxide
bind '"\C-o":"\C-uji\C-m"'
export _ZO_FZF_OPTS="--height 20% --border --layout=reverse"
eval "$(zoxide init --cmd j bash)"
