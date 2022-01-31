setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
#setxkbmap -option "caps:escape" -layout "no"
#setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl
setxkbmap us -variant colemak
xsetroot -cursor_name left_ptr
xset r rate 200 50
xset m 0 0
xset -dpms
xset s off
redshift -l 58:9 -t 6500K:2000K -b 0.60 &
dunst &
hsetroot -solid "#ffffff" &
banish &
source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh &
#xrandr --auto --output DP-1 --mode 1920x1080 --left-of HDMI-0 &
DISPLAY=":0" picom --backend glx --vsync --xrender-sync-fence --no-fading-openclose -b &
nohup ~/.scripts/statusbar > /dev/null &
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK
exec ~/.scripts/dwm
