# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
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
alias csi="chicken-csi -script"

# exports
export RUSTC_WRAPPER=""
export ALTERNATE_EDITOR=""
export CLANG_VERSION=12.0.1
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:/home/odd/.cargo/bin/:/opt/devkitpro/devkitPPC/bin"
export TERM="screen-256color"
export BROWSER="firefox"

# nvim
nvim_wrapper() {
  if test -z $VIMRUNTIME; then
      nvim $@
  else
      nvr $@
  fi
}

alias vim="nvim_wrapper"
alias bashconf="nvim_wrapper ~/.bashrc"

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

nnn_wrapper() {
  if test -z $VIMRUNTIME; then
    export EDITOR="nvim"
    export VISUAL="nvim"
  else
    export EDITOR="nvr"
    export VISUAL="nvr"
  fi
  n -e $@
}

alias lf="nnn_wrapper"

# fzf
source "/usr/share/fzf/key-bindings.bash"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--height 20% --border --layout=reverse"

# zoxide
bind '"\C-o":"\C-uji\C-m"'
export _ZO_FZF_OPTS="--height 20% --border --layout=reverse"
eval "$(zoxide init --cmd j bash)"
