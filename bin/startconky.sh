#!/usr/bin/env sh

########################################################################
# pulled from the internets, modified a bit.  original author unknown  #
# found on a conky list someplace.				       #
# Scot Junkin							       #
########################################################################

#conky from git is signicantly different from conky from distro repo.
#CONKY=/home/sjunkin/sandbox/conky/src/conky

CONKY=conky
#sleep 5 #time (in s) for the DE to start; use ~20 for Gnome or KDE, less for Xfce/LXDE etc
$CONKY -D -c ~/.conky/rings & # the main $CONKY with rings
sleep 5 #time for the main $CONKY to start; needed so that the smaller ones draw above not below (probably can be lower, but we still have to wait 5s for the rings to avoid segfaults)
$CONKY -D -c ~/.conky/cpu &
$CONKY -D -c ~/.conky/mem &
$CONKY -D -c ~/.conky/notes &

#tilda #used to get a small desktop terminal (not in the screenshot); you need tilda installed (use your package manager), and the tilda config is included
