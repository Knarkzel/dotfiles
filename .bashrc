#    _               _
#   | |__   __ _ ___| |__  _ __ ___
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__
# (_)_.__/ \__,_|___/_| |_|_|  \___|

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# lf
# lfcd () {
#     tmp="$(mktemp)"
#     lf -last-dir-path="$tmp" "$@"
#     if [ -f "$tmp" ]; then
#         dir="$(cat "$tmp")"
#         rm -f "$tmp"
#         if [ -d "$dir" ]; then
#             if [ "$dir" != "$(pwd)" ]; then
#                 cd "$dir"
#             fi
#         fi
#     fi
# }
# 
# # aliases
# alias vim="nvim"
# alias lf="lfcd"
# alias sudo="sudo"
# alias ls="exa --group-directories-first --icons -x"
# alias si="sudo pacman -Sy"
# alias sr="sudo pacman -R"
# alias cr="cargo run --"
# alias cb="cargo build"
# alias cdo="cargo doc --open"
# alias vimconf="nvim ~/.config/nvim/init.vim"
# alias bashconf="nvim ~/.bashrc"
# alias lfconf="nvim ~/.config/lf/lfrc"
# alias cpuhigh="echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor"
# alias config='/usr/bin/git --git-dir=$HOME/dotfiles/.cfg/ --work-tree=$HOME'
# 
# # exports
# export PATH="$PATH:/home/odd/.cargo/bin/:/home/odd/.scripts:/opt/devkitpro/devkitPPC/bin"
# export FZF_DEFAULT_COMMAND="rg --files"
# export ALTERNATE_EDITOR=""
# export EDITOR="nvim"
# export VISUAL="less"
# export RUSTC_WRAPPER=""
# export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# 
# DEVKITPRO=/opt/devkitpro
# DEVKITARM=/opt/devkitpro/devkitARM
# DEVKITPPC=/opt/devkitpro/devkitPPC
# 
# vterm_printf(){
#     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
#         # Tell tmux to pass the escape sequences through
#         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
#     elif [ "${TERM%%-*}" = "screen" ]; then
#         # GNU screen (screen, screen-256color, screen-256color-bce)
#         printf "\eP\e]%s\007\e\\" "$1"
#     else
#         printf "\e]%s\e\\" "$1"
#     fi
# }
# 
# # current prompt setting
# #PS1="\[\e[32m\][\[\e[m\]\[\e[31m\]\u\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[32m\]\h\[\e[m\]:\[\e[36m\]\w\[\e[m\]\[\e[32m\]]\[\e[m\]\[ $\[\e[m\] "
# 
# # autojump
# [[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh
# 
# if [ "${BASH_VERSINFO[0]}" -gt 4 ] || ([ "${BASH_VERSINFO[0]}" -eq 4 ] && [ "${BASH_VERSINFO[1]}" -ge 1 ])
# then
# source <("/usr/bin/starship" init bash --print-full-init)
# else
# source /dev/stdin <<<"$("/usr/bin/starship" init bash --print-full-init)"
# fi

elvish
