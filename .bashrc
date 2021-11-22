# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias
alias sudo="sudo"
alias ls="exa --group-directories-first -x"
alias cr="cargo run --"
alias cb="cargo build"
alias cdo="cargo doc --open"
alias config="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"
alias bashconf="vim ~/.bashrc"

# Export
export CC="gcc"
export EDITOR="vim"
export VISUAL="vim"
export BROWSER="firefox"
export HISTCONTROL=ignorespace:ignoredups:erasedups
#export PATH="$PATH:/home/odd/.cargo/bin:/home/odd/.temp/:/home/odd/source/vlang/v:/home/odd/source/vlang/vls/bin"
export TERM="screen-256color"

# Fzf
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--height 20% --border --layout=reverse"

# Zoxide
bind '"\C-o":"\C-uji\C-m"'
export _ZO_FZF_OPTS="--height 20% --border --layout=reverse"
eval "$(zoxide init --cmd j bash)"

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

# Source the system-wide file.
source /etc/bashrc

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi
