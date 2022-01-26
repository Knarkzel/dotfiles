# aliases
alias vim="emacsclient -nw"
alias emacs="emacsclient -nw"
alias sudo="sudo"
alias cat="bat --paging=never --theme='Solarized (light)'"
alias ls="exa --group-directories-first -x"
alias top="btm"
alias gg="gitui"
alias si="sudo pacman -S"
alias sr="sudo pacman -R"
alias cr="cargo run --"
alias cb="cargo build"
alias cdo="cargo doc --open"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias bashconf="vim ~/.bashrc"
alias termconf="vim ~/.config/alacritty/alacritty.yml"
alias mupdf="~/.scripts/mupdf"

# exports
export RUSTC_WRAPPER=""
export ALTERNATE_EDITOR=""
export CLANG_VERSION=13.0.0
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:/home/odd/.cargo/bin/:/opt/devkitpro/devkitPPC/bin:/home/odd/.local/bin:/home/odd/.scripts/menu"
export TERM="screen-256color"
export BROWSER="chromium"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient -nw"

# devkitpro
DEVKITPRO="/opt/devkitpro"
DEVKITARM="/opt/devkitpro/devkitARM"
DEVKITPPC="/opt/devkitpro/devkitPPC"

# fzf
source "/usr/share/fzf/key-bindings.bash"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS_LIGHT="--color fg:-1,bg:-1,hl:33,fg+:235,bg+:-1,hl+:33 --color info:33,prompt:33,pointer:33,marker:234,spinner:33"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_LIGHT --height 20% --border --layout=reverse"

# zoxide
bind '"\C-o":"\C-uji\C-m"'
export _ZO_FZF_OPTS="$FZF_DEFAULT_OPTS"
eval "$(zoxide init --cmd j bash)"
