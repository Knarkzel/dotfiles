setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
#setxkbmap -option "caps:escape" -layout "no"
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl
xsetroot -cursor_name left_ptr
xset r rate 200 50
xset m 0 0
xset -dpms
xset s off
redshift -l 58:9 -t 6500K:2000K -b 0.60 &
dunst &
hsetroot -solid black &
start-pulseaudio-x11 &
xrdb -merge ~/.Xresources &
nvidia-settings --assign CurrentMetaMode="HDMI-0: nvidia-auto-select @1920x1080 +1920+0 {ViewPortIn=1920x1080, ViewPortOut=1920x1080+1920+0, ForceFullCompositionPipeline=On}, HDMI-1: nvidia-auto-select @1920x1080 +0+0 {ViewPortIn=1920x1080, ViewPortOut=1920x1080+0+0, ForceFullCompositionPipeline=On}"
nvidia-settings -a 'AllowFlipping=0' &
source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh &
exec stumpwm
