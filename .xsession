#!/bin/bash
PATH=$HOME/.cabal/bin:$PATH

#setxkbmap -layout "us,ru(common)" -option "altwin:meta_win,grp:caps_toggle,numpad:microsoft,caps:shift,compose:menu,terminate:ctrl_alt_bksp"
xkbcomp -I$HOME/.xkb -R$HOME/.xkb keymap/custom $DISPLAY
xmodmap $HOME/.xmodmaprc

# xautolock  -time 5 -locker "xlock -lockdelay 60" -nowlocker xlock &
#xscreensaver -no-splash &
gnome-screensaver&

#xsetbg -onroot $HOME/.desktop-image.png
#startkde
#wmaker
#afterstep
#enlightenment
#sawfish
xmonad
#xmonad
#ion3
#gnome-session
