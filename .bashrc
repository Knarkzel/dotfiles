# aliases
alias si="nix-env -iA"
alias sr="nix-env --uninstall"
alias su="sudo nixos-rebuild switch"
alias cb="cargo build"
alias cr="cargo run"
alias zb="zig build"
alias zr="zig build run"
alias zt="zig build test"

# other
alias tmp="cd $(mktemp -d); clear"
alias ls="ls --color=always --group-directories-first"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

# emacs vterm
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${PWD}\007"'

# exports
export EDITOR="nano"
export VISUAL="nano"
export BROWSER="firefox"
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:$HOME/.cargo/bin/:$HOME/.local/bin:$HOME/.scripts:$HOME/.nix-profile/bin"
export TERM="xterm-256color"

# starship
eval "$(starship init bash)"
