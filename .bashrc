# aliases
function si() {
    echo "Looking up nixos.$1..."
    nix-env -iA nixos.$1
}
alias sr="nix-env --uninstall"
alias su="sudo nixos-rebuild switch"
alias cb="cargo check"
alias cr="cargo run"
alias ct="cargo nextest run"
alias cdo="cargo doc --open"
alias zb="zig build"
alias zr="zig build run"
alias zt="zig build test"
alias tmp="cd $(mktemp -d); clear"
alias ls="ls --color=always --group-directories-first"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

# exports
export EDITOR="nano"
export VISUAL="nano"
export BROWSER="firefox"
export PATH="$PATH:$HOME/.cargo/bin/:$HOME/.local/bin:$HOME/.scripts:$HOME/.nix-profile/bin"
export TERM="xterm-256color"
export NIXPKGS_ALLOW_UNFREE=1
export _JAVA_AWT_WM_NONREPARENTING=1

# history
PROMPT_COMMAND="history -a; history -c; history -r; ${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${PWD}\007"'
export HISTCONTROL=ignorespace:ignoredups:erasedups
shopt -s histappend
export HISTSIZE=100000
export HISTFILESIZE=100000

# direnv
export DIRENV_LOG_FORMAT=
eval "$(direnv hook bash)"

# starship
eval "$(starship init bash)"
