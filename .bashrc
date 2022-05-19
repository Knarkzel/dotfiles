# aliases
alias si="sudo pacman -Sy"
alias sr="sudo pacman -R"
alias su="sudo pacman -Syyu"
alias gs="git status"
alias gp="git push"
alias gc="git commit"
alias ga="git add"
alias cb="cargo build"
alias cr="cargo run"

# other
alias nano="nano"
alias ls="ls --color=always --group-directories-first"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias tmp="cd $(mktemp -d); clear"
alias launch="sudo screen -d -m"

# exports
export EDITOR="nano"
export VISUAL="nano"
export BROWSER="firefox"
export HISTCONTROL=ignorespace:ignoredups:erasedups
export PATH="$PATH:$HOME/.cargo/bin/:$HOME/.local/bin:$HOME/.scripts/menu"
export TERM="xterm-256color"

# better tab complete
bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'
bind '"\e[Z":menu-complete-backward'
bind 'set page-completions off'

# use bash-completion if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# starship
eval "$(starship init bash)"
