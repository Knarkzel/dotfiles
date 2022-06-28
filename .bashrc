# aliases
alias si="nix-env -iA"
alias sr="sudo pacman -R"
alias su="sudo nixos-rebuild switch"
alias gs="git status"
alias gp="git push"
alias gc="git commit"
alias ga="git add"
alias cb="cargo build"
alias cr="cargo run"
alias zb="zig build"
alias zd="zig build deploy"
alias zr="zig build run"
alias zt="zig build test"
alias zt="zig init-exe"

# other
alias ls="ls --color=always --group-directories-first"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias tmp="cd $(mktemp -d); clear"
alias nix-search="nix-env -qP --available"

# emacs vterm
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${PWD}\007"'

# exports
export EDITOR="nano"
export VISUAL="nano"
export BROWSER="firefox"
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:$HOME/.cargo/bin/:$HOME/.local/bin:$HOME/.scripts/menu:$HOME/.nix-profile/bin"
export TERM="xterm-256color"

# starship
eval "$(starship init bash)"
