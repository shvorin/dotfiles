#!/bin/bash
. ~/.bashrc
/home/art/bin/set_keycodes_1.sh 
gnome-screensaver &

xrdb ~/.Xresources

#setxkbmap -v 10 "us,ru(common)" ""  "altwin:meta_win,grp:caps_toggle,numpad:microsoft,caps:shift"
#xkbcomp $HOME/xkb_keymap $DISPLAY
#xmodmap ~/.xmodmaprc

#startkde
#wmaker
#afterstep
#enlightenment
#sawfish
xmonad
#gnome-session
