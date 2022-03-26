# xorg
xset m 0 0
xset -dpms
xset s off
xset r rate 200 50
xsetroot -cursor_name left_ptr

# keyboard
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
setxkbmap us -variant colemak

# other
dunst &
xbanish &
hsetroot -solid "#ffffff" &
redshift -l 58:9 -t 6500K:2000K -b 0.60 &

# fix java
wmname "LG3D"
export _JAVA_AWT_WM_NONREPARENTING=1

# dwm
nohup ~/.scripts/statusbar > /dev/null &
exec ~/.scripts/dwm
